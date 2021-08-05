
library(tidyverse)
library(brms)
library(patchwork)
library(colorspace)

## get data files
source("global_variables.R")
source("process_respondent_data.R")

#create datasets
diaries_wide <- respondent_data
#filtering join so only the analysed sample is included
diaries_long <- right_join(primary_long, respondent_data) 

#function to make priors for brms softmax models
make_prior <- function(resp_var) {
  outcome_levels <- levels(resp_var)[-1]
  expand_grid(dpar = str_c("mu", outcome_levels),
              class = c("Intercept", "b", "sd")) %>%
    rowwise() %>%
    mutate(prior_string = if_else(class %in% c("Intercept", "b"), "normal(0, 2)", "exponential(3)"),
           prior = list(set_prior(prior_string, class = class, dpar = dpar))) %>%
    with(bind_rows(prior)) %>%
    bind_rows(set_prior("normal(0, 2)", class = "Intercept")) %>%
    bind_rows(set_prior("normal(0, 2)", class = "b"))
}

#### Function to plotting time diaries ####

#Tempogram
plot_cumulative <- function(diary_tbl, 
                            values_col = coded5_activity,
                            slots_col = slot,
                            value_levels = coded5_order,
                            palette = coded5_palette) {
  
  values_col <- enquo(values_col); slots_col <- enquo(slots_col)
  
  diary_tbl %>%
    select(diaryid, crudewt2, cluster,  slot = !!slots_col, value = !!values_col) %>%
    #calculate grouped average percent
    group_by(cluster, slot, value) %>%
    summarise(n = sum(crudewt2), .groups = "drop_last") %>%
    mutate(percent = n / sum(n), .keep = "unused")  %>%
    ungroup() %>%
    #make sure every cluster - timeslot - value combination has a value, even if 0
    full_join(with(., expand_grid(cluster = unique(cluster),
                                  slot = unique(slot), 
                                  value = unique(value))), 
              by = c("cluster", "slot", "value")) %>%
    mutate(percent = coalesce(percent, 0)) %>%
    #add in cluster sizes and cluster descriptions
    left_join(count(diary_tbl, cluster), by = "cluster") %>%
    left_join(distinct(diary_tbl, cluster, description), by = "cluster") %>%
    #change labelling
    mutate(slot_time = slot_to_date(slot),
           value = factor(value, levels = value_levels),
           facet_label = str_glue("Cluster {cluster}  (n = {n / 144})"),
           facet_label_long = str_wrap(str_glue("{cluster}: {description} (n = {n / 144})"), 30),
           across(starts_with("facet"), ~fct_reorder(., cluster))) %>%
    #plot
    ggplot(aes(x = slot_time, y = percent, fill = value)) +
      geom_area(position = "stack") +
      geom_vline(xintercept = plot_date + hours(12), linetype = "dashed") +
      geom_vline(xintercept = c(plot_date + hours(c(9, 17))), linetype = "solid") + 
      scale_x_datetime(date_breaks = "4 hours", date_labels = "%I %p") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = palette, drop = FALSE, name = "Activity Group") +
      labs(x = "Time of Day", y = "Percent (weighted)") +
      theme_minimal() +
      theme(strip.text = element_text(size = 12, face = "bold"),
            legend.position = "bottom") +
      facet_wrap(~facet_label_long, labeller = label_wrap_gen(30))
}
  
#Proportion of diary days belonging to a cluster, by a faceting variable
plot_survey_cluster <- function(tbl, 
                                in_q = NULL,
                                cluster_label = NULL,
                                subset = "Workday",
                                cluster_subset = 1:37) {
  
  in_q <- enquo(in_q)
  if(is.null(cluster_label)) cluster_label <- str_to_title(as_label(in_q))
  title <- str_glue("Percentage of diary days in each survey wave belonging to a cluster, by {cluster_label}")
  
  tbl %>%
    mutate(cluster = as.character(cluster)) %>%
    group_by(!!in_q, survey, cluster, description) %>%
    summarise(n = sum(crudewt2), .groups = "drop") %>%
    group_by(survey, !!in_q) %>%
    mutate(percent = n  / sum(n) * 100) %>%
    ungroup() %>%
    filter(str_detect(description, subset),
           cluster %in% cluster_subset)  %>%
    mutate(label = if_else(survey == "2016",
                           str_wrap(str_glue("{cluster}: {description}"), 25),
                           cluster)) %>%
    ggplot(aes(x = survey, y = percent, label = label, colour = cluster, group = cluster)) +
    geom_path() +
    geom_label(alpha = .5, aes(hjust = if_else(survey == "2016", 1, 0.5))) +
    scale_x_discrete(expand = c(0, 1.8, 0, 0.6)) +
    scale_colour_discrete_qualitative(palette = "Dark 3", guide = FALSE) +
    facet_wrap(vars(!!in_q)) +
    labs(title = title, x = "Survey Wave", y = "Percent (weighted)") +
    my_theme() +
    theme(panel.grid = element_line(colour = "grey70"))
}

#Same as previous, without a faceting variable
plot_survey_cluster_no_facet <- function(tbl, 
                                subset = "Workday",
                                cluster_subset = 1:37) {
  title <- str_glue("Percentage of diary days in each survey wave belonging to a cluster")
  
  tbl %>%
    mutate(cluster = as.character(cluster)) %>%
    group_by(survey, cluster, description) %>%
    summarise(n = sum(crudewt2), .groups = "drop") %>%
    group_by(survey) %>%
    mutate(percent = n  / sum(n) * 100) %>%
    ungroup() %>%
    filter(str_detect(description, subset),
           cluster %in% cluster_subset)  %>%
    mutate(label = if_else(survey == "2016",
                           str_wrap(str_glue("{cluster}: {description}"), 25),
                           cluster)) %>%
    ggplot(aes(x = survey, y = percent, label = label, colour = cluster, group = cluster)) +
    geom_path() +
    geom_label(alpha = .5, aes(hjust = if_else(survey == "2016", 1, 0.5))) +
    scale_x_discrete(expand = c(0, 1.8, 0, 0.6)) +
    scale_colour_discrete_qualitative(palette = "Dark 3", guide = FALSE) +
    labs(title = title, x = "Survey Wave", y = "Percent (weighted)") +
    my_theme() +
    theme(panel.grid = element_line(colour = "grey70"))
}


#### Show all clusters with summary statistics ####

#Figure 4.1
plot_cumulative(diaries_long) 

#Table 4.1
modal_age <- diaries_wide %>%
  group_by(cl_desc, dagegrp) %>%
  summarise(n = sum(crudewt2), .groups = "drop_last") %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  select(cl_desc, `Modal Age Group` = dagegrp)

#max entropy
max_H <- log(n_distinct(diaries_long$coded5_activity))

entropy <- diaries_long %>%
  group_by(cl_desc, slot, coded5_activity) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  summarise(H = -sum(prop * log(prop)), .groups = "drop_last") %>%
  summarise(mean_H = mean(H)) %>%
  mutate(prop_max_H = mean_H / max_H, .keep = "unused")

diaries_wide %>%
  group_by(cl_desc, cluster) %>%
  summarise(Female = sum(crudewt2[sex == "Female"]) / sum(crudewt2),
            `With Children`= sum(crudewt2[has_kids == "Children"] / sum(crudewt2)),
            `Not Working` = sum(crudewt2[in_work == "Not Working"], na.rm = T) / sum(crudewt2),
            `Degree` = sum(crudewt2[hied2 == "Degree"], na.rm = T) / sum(crudewt2),
            `Higher Professional` = sum(crudewt2[dclasuk == "Higher managerial and professional"]) / sum(crudewt2),
            `Intermediate` = sum(crudewt2[dclasuk == "Intermediate"]) / sum(crudewt2),
            `Routine` = sum(crudewt2[dclasuk == "Routine"]) / sum(crudewt2), .groups = "drop") %>%
  full_join(modal_age) %>%
  full_join(entropy) %>%
  mutate(across(where(is.numeric), ~ round(., 3) * 100)) %>%
  arrange(cluster) %>%
  select(-cluster) %>%
  rename(cluster = cl_desc) %>%
  rename_with(~str_c("% ", .), .cols = where(is.numeric)) %>%
  write_rds("table_4_1.rds")
  
#### Workdays ####

#Figure 4.1.1
diaries_long %>%
  filter(str_detect(description, "^Workday")) %>%
  plot_cumulative() 

#Figure 4.1.2
diaries_wide %>%
  mutate(w9to5 = if_else(str_detect(description, "Workday 9"), "9 to 5 Workday", "Other Workday"),
         cluster = as.character(cluster)) %>%
  group_by(survey, w9to5, cluster, description) %>%
  summarise(n = sum(crudewt2), .groups = "drop") %>%
  group_by(survey) %>%
  mutate(percent = n  / sum(n) * 100) %>%
  ungroup() %>%
  filter(str_detect(description, "^Workday")) %>%
  mutate(label = if_else(survey == "2016",
                         str_wrap(str_glue("{cluster}: {description}"), 25),
                         cluster)) %>%
  ggplot(aes(x = survey, y = percent, label = label, colour = cluster, group = cluster)) +
    geom_path() +
    geom_label(alpha = .5, aes(hjust = if_else(survey == "2016", 1, 0.5))) +
    facet_wrap(vars(w9to5), scales = "free_y") +
    scale_x_discrete(expand = c(0, 1.8, 0, 0.6)) +
    scale_colour_discrete_qualitative(palette = "Dark 3", guide = FALSE) +
    labs(title = "Percentage of diary days in each survey wave belonging to a cluster",
         x = "Survey Wave", y = "Percent (weighted)") +
    my_theme() +
    theme(panel.grid = element_line(colour = "grey70"))

#9 to 5 workdays by class and cluster
workday95_data <- diaries_wide %>%
  filter(workday == "Workday",
         dclasuk != "Not working or unemployed") %>%
  mutate(w9to5 = if_else(str_detect(description, "Workday 9"), "9 to 5 Workday", "Non-standard Workday"),
         is_not_w9to5 = if_else(!str_detect(description, "Workday 9"), 1L, 0L)) 

#number of cases
nrow(workday95_data)

#when done by sex, we just get maintenance of inequality where women have more disrupted work

#model by survey: create dataset
workday_data <- diaries_wide %>%
  filter(workday == "Workday",
         !is.na(day_type)) %>%
  select(survey, crudewt2, cluster, cl_desc, day_type, dclasuk, dagegrp, sex) %>%
  mutate(workday_type = case_when( cluster == 6 ~ "SecondShift",
                                   cluster %in% c(1, 2, 3) ~ "Morning",
                                   cluster %in% c(8, 9, 15) ~ "NoLeisure",
                                   cluster %in% c(12, 13, 14) ~ "Afternoon",
                                   str_detect(cl_desc, "9 to 5") ~ "9to5",
                                   TRUE ~ "OtherWorkday"),
         is_teleworkday = if_else(day_type == "Home Workday", 1L, 0L),
         workday_type = fct_relevel(workday_type, "OtherWorkday")) #make this the pivot

#show number of cases
nrow(workday_data) 

#model
model_4_1_1 <- brm(workday_type | weights(crudewt2) ~ dagegrp + dclasuk + sex + (1|survey),
                      family = categorical,
                      data = workday_data, 
                      prior = make_prior(workday_data$workday_type),
                      chains = 3, cores = 3, iter = 3000, warmup = 1000,
                      file = "cache/model_4_1_1.rds")

draws <- get_wide_draws(model_4_1_1, resp_levels = levels(workday_data$workday_type), cluster = F)

#calculate bfs 
bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, resp)

#Figure 4.1.3
draws %>%
  survey_differences() %>%
  plot_categ_draws(~diff)  +
  labs(title = "Difference in linear model intercepts, Model 4.1.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Cluster Group", x = "Difference") +
  bayes_factor_text(bfs)

#interesting by-lockdown pattern in class
plot_w95 <- function(in_q, x_label) {
  in_q <- enquo(in_q);
  workday95_data %>%
    group_by(survey, !!in_q, w9to5) %>%
    summarise(n = sum(crudewt2), .groups = "drop_last") %>%
    mutate(prop = n  / sum(n)) %>%
    ungroup() %>%
    filter(w9to5 == "Non-standard Workday") %>%
    ggplot(aes(x = !!in_q, y = prop, fill = survey)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = round(prop, 2) * 100),
                position = position_dodge(width = .9), size = 5, vjust = -.2) +
      scale_fill_manual(values = survey_palette, name = "Survey Wave") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous(expand = c(0, 0, .1, .1),
                         label = scales::label_percent(accuracy = 1)) +
      scale_colour_discrete_qualitative(palette = "Dark 3", guide = FALSE) +
      labs(x = x_label, y = "Percent (weighted)") +
      my_theme()
}

#Figure 4.1.4
plot_w95(sex, "Sex") + plot_w95(dclasuk, "Class") + 
  plot_annotation(title = "Proportion of non - '9 to 5' workdays, by Survey Wave") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

model_4_1_2 <- brm(is_not_w9to5 | weights(crudewt2) ~ 1 + dagegrp + sex + (1|dclasuk:survey),
                 family = bernoulli,
                 data = workday95_data,
                 prior = c(prior(normal(0, 2), class = "Intercept"),
                           prior(normal(0, 2), class = "b"),
                           prior(exponential(3), class = "sd")),
                 chains = 3, cores = 3, warmup = 1000, iter = 3000,
                 file = "cache/model_4_1_2.rds")

draws <- get_wide_draws(model_4_1_2)

class_draws <- draws %>%
    pivot_wider(names_from = dclasuk, values_from = value) %>%
    mutate(`Higher` - `Intermediate`,
           `Higher` - `Routine`,
           `Intermediate` - `Routine`, .keep = "unused") %>%
    pivot_longer(contains(" - "), names_to = "diff") 

bfs_class <- get_bfs(class_draws, diff, survey)

#Figure 4.1.5
class_draws %>%
  plot_draws(cols = survey) +
  bayes_factor_text(bfs_class) +
  labs(x = "Difference", y = "",
       title = "Linear model intercepts, Model 4.1.2",
            subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0")

bfs_survey <- draws %>%
  survey_differences() %>%
  get_bfs(diff, dclasuk)

#Figure 4.1.6
draws %>%
  survey_differences() %>%
  plot_draws(cols = dclasuk) +
  bayes_factor_text(bfs_survey) +
  labs(x = "Difference", y = "",
       title = "Linear model intercepts, Model 4.1.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0")

#Figure 4.1.7
diaries_wide %>%
  filter(!is.na(day_type)) %>%
  group_by(survey, day_type) %>%
  summarise(n = sum(crudewt2, na.rm = T), .groups = "drop_last") %>%
  mutate(prop = n / sum(n),
         label = round(prop * 100, 1)) %>%
  ungroup() %>%
  ggplot(aes(x = survey, y = prop, fill = day_type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = label), position = position_dodge(width = .9), vjust = -.3) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = daytype_palette, name = "Workday Type") +
  labs(title = "Proportion of diary days, by Day Type and Survey Wave",
       x = "Survey Wave", y = "Percent (weighted)") +
  my_theme()

model_4_1_3 <- brm(is_teleworkday ~ (1|cluster:survey),
                         family = bernoulli,
                         data = workday_data,
                         prior = c(prior(normal(0, 5), class = "Intercept"),
                                   prior(exponential(3), class = "sd")),
                         chains = 3, cores = 3, iter = 1000, warmup = 500,
                         file = "cache/model_4_1_3.rds")

draws <- get_wide_draws(model_4_1_3, unique(workday_data$teleworkday), cluster = TRUE)

description_key <- distinct(transmute(diaries_wide, cluster = as.character(cluster), cl_desc))

#bayes factors
bfs <- draws %>%
  get_bfs(survey, cluster) %>%
  left_join(description_key)

#Figure 4.1.8
draws %>%
  left_join(description_key) %>%
  mutate(cl_desc = fct_reorder(cl_desc, as.numeric(cluster))) %>%
  ggplot(aes(x = value, y = cl_desc)) +
  stat_interval() +
  geom_vline(xintercept = 0) +
  facet_wrap(~survey) + 
  scale_x_continuous(expand = c(.35, 0, .1, 0)) +
  intervals_colour_scale +
  labs(title = "Linear Model Intercepts: Working From Home by Cluster, Model 4.1.3",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Cluster", x = "Value", caption = "logit link") +
  my_theme() +
  theme(plot.title.position = "plot") +
  bayes_factor_text(bfs, vjust = .5, x = -Inf)


#### Unpaid days ####

#Figure 4.2.1
diaries_long %>%
  filter(str_detect(description, "^Unpaid")) %>%
  plot_cumulative() 

#Figure 4.2.2
diaries_wide %>%
  plot_survey_cluster(in_q = sex, subset = "^Unpaid")

#hmm there's a thing for leisure where higher is mixed, men lower are 33 and female lower are 29/17
#to check
#Disrupted, 35 is male unemployed, 34 is female unemployed
#26 is male intermediate and female routine...

#model that
unpaid_gender <- diaries_wide %>%
  filter(!is.na(in_work)) %>%
  select(survey, crudewt2, cluster, cl_desc, day_type, dclasuk, workday, dagegrp, has_kids, sex, in_work) %>%
  filter(!is.na(in_work)) %>% #17 NA
  mutate(unpaid_cluster = case_when(cluster == 32 ~ "c32",
                                    cluster %in% c(16, 18) ~ "UnpaidAndLeisure",
                                    cluster %in% c(21, 22, 23) ~ "UnpaidWorkday",
                                    TRUE ~ "other"),
         unpaid_cluster = fct_relevel(unpaid_cluster, "other")) #make other the pivot

model_4_2_1 <- brm(unpaid_cluster | weights(crudewt2) ~ 1 + dclasuk + dagegrp + has_kids + (1|sex:survey),
                  family = categorical,
                  data = unpaid_gender,    
                  prior = make_prior(unpaid_gender$unpaid_cluster),
                  chains = 3, cores = 3, warmup = 1000, iter = 3000,
                 file = "cache/model_4_2_1.rds")

draws <- get_wide_draws(model_4_2_1, resp = unpaid_gender$unpaid_cluster)

#calculate bayes factors
bfs_survey <- draws %>%
  survey_differences() %>%
  get_bfs(diff, sex, resp)

bfs_sex <- draws %>%
  sex_differences() %>%
  get_bfs(diff, survey, resp)

#Figure 4.2.3
draws %>%
  survey_differences() %>%
  plot_categ_draws(formula(sex ~ diff)) +
  labs(title = "Difference in linear model intercepts, Model 4.2.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
     y = "Cluster Group", x = "Difference") +
  bayes_factor_text(bfs_survey)

#Figure 4.2.4
draws %>%
  sex_differences() %>%
  plot_categ_draws(formula(~survey)) +
  labs(title = "Difference in linear model intercepts, Model 4.2.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Cluster Group", x = "Female - Male") +
  bayes_factor_text(bfs_sex)

model_4_2_2 <- update(model_4_2_1,
                      formula = unpaid_cluster | weights(crudewt2) ~ 1 + dclasuk + dagegrp + has_kids + workday + (1|sex:survey:in_work),
                   newdata = unpaid_gender,    
                   chains = 3, cores = 3, warmup = 1000, iter = 3000,
                   file = "cache/model_4_2_2.rds")

draws <-  get_wide_draws(model_4_2_2, resp_levels = levels(unpaid_gender$unpaid_cluster)) 

bfs_working <- draws %>%
  working_differences() %>%
  get_bfs(diff, survey, resp, sex)

#Figure 4.2.5
draws %>%
  working_differences() %>%
  mutate(survey = fct_rev(survey)) %>%
  plot_categ_draws(formula(survey~sex)) +
  labs(title = "Difference in linear model intercepts, Model 4.2.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
     y = "Cluster Group", x = "Working - Not Working") +
  bayes_factor_text(bfs_working)

#### Leisure ####

#Figure 4.3.1
diaries_long %>%
  filter(str_detect(description, "^Leisure")) %>%
  plot_cumulative()  

#Figure 4.3.2
plot_survey_cluster_no_facet(diaries_wide, subset = "^(Leisure)")

#Figure 4.3.3
plot_survey_cluster(diaries_wide, in_q = dclasuk, subset = "^Leisure")

#create model data
leisure_data <- diaries_wide %>%
  select(survey, crudewt2, cluster, cl_desc, dclasuk, dagegrp, sex, in_work, workday) %>%
  filter(!is.na(in_work)) %>%
  mutate(interrupted_cluster = case_when(cluster %in% c(17, 30) ~ "LeisureInterrupted",
                                    cluster == 29 ~ "LeisureSlightlyInterrupted",
                                    cluster %in% c(28, 33) ~ "LeisureAllDay",
                                    TRUE ~ "other"),
         leisure_cluster = if_else(cluster %in% c(17, 28, 29, 30, 32, 33), as.character(cluster), "other"),
         across(c(interrupted_cluster, leisure_cluster), ~fct_relevel(., "other")))#make other the pivot

#Figure 4.3.4
diaries_wide %>%
  plot_survey_cluster(in_q = sex, subset = "^(Leisure)")

model_4_3_1 <- brm(interrupted_cluster | weights(crudewt2) ~ 1 + dclasuk + dagegrp + (1|sex:survey),
                  family = categorical,
                  data = leisure_data,        
                  prior = make_prior(leisure_data$interrupted_cluster),
                  chains = 3, cores = 3, warmup = 1000, iter = 3000,
                  file = "cache/model_4_3_1.rds")

draws <- get_wide_draws(model_4_3_1, resp_levels = levels(leisure_data$interrupted_cluster)) 

#calculate bayes factors
bfs_sex <- draws %>%
  sex_differences() %>%
  get_bfs(diff, resp, survey)

bfs_survey <- draws %>%
  survey_differences() %>%
  get_bfs(diff, resp, sex)

#Figure 4.3.5
draws %>%
  sex_differences() %>%
  plot_categ_draws(formula(~survey)) +
    labs(title = "Difference in linear model intercepts, Model 4.3.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
     y = "Cluster Group", x = "Female - Male") +
  bayes_factor_text(bfs_sex)

#Figure 4.3.6
draws %>%
  survey_differences() %>%
  plot_categ_draws(formula(sex~diff)) +
  labs(title = "Difference in linear model intercepts, Model 4.3.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Cluster Group", x = "Female - Male") +
  bayes_factor_text(bfs_survey)

model_4_3_2 <- brm(interrupted_cluster | weights(crudewt2) ~ 1 + dclasuk + dagegrp + workday + (1|sex:survey:in_work),
                  family = categorical,
                  data = leisure,        
                  prior = make_prior(leisure$interrupted_cluster),
                  chains = 3, cores = 3, warmup = 1000, iter = 3000,
                  file = "cache/model_4_3_2.rds")

draws <- get_wide_draws(model_4_3_2, resp_levels = levels(leisure$interrupted_cluster)) 

bfs_working <- draws %>%
  working_differences() %>%
  get_bfs(diff, resp, survey, sex)

bfs_sex <- draws %>%
  sex_differences() %>%
  get_bfs(diff, resp, survey, in_work)

#Figure 4.3.7
draws %>%
  working_differences() %>%
  mutate(survey = fct_rev(survey)) %>%
  plot_categ_draws(formula(sex~survey)) +
  labs(title = "Difference in linear model intercepts, Model 4.3.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Cluster Group", x = "Working - Not Working") +
  bayes_factor_text(bfs_working)

#Figure 4.3.8
draws %>%
  sex_differences() %>%
  plot_categ_draws(formula(in_work~survey)) +
  labs(title = "Difference in linear model intercepts, Model 4.3.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Cluster Group", x = "Female - Male") +
  bayes_factor_text(bfs_sex)

#### Disrupted days ####

#Figure 4.4.1
diaries_long %>%
  filter(str_detect(description, "^(Rest|Disrupted)")) %>%
  plot_cumulative() 

#Figure 4.4.2
plot_survey_cluster_no_facet(diaries_wide, subset = "^(Rest|Disrupted)")

disrupted_data <- diaries_wide %>%
  select(survey, crudewt2, cluster, teleworkday, dclasuk, dagegrp, sex, has_kids, in_work, workday) %>%
  filter(!is.na(in_work)) %>%
  mutate(disr = case_when(cluster %in% c(19, 20, 26, 27, 34, 35, 37) ~ as.character(cluster),
                             TRUE ~ "other") %>% fct_relevel("other"),
         disr_unpaid = case_when(cluster %in% c(35, 37) ~ "c35and37",
                                 cluster %in% c(19, 20, 26, 27, 34) ~ "OtherDisrupted",
                                 TRUE ~ "other") %>% fct_relevel("other"))

#model 4.4.1
model_4_4_1 <- brm(disr | weights(crudewt2) ~ 1 + dclasuk + sex + dagegrp + (1|survey),
                  family = categorical,
                  data = disrupted_data,  
                  prior = filter(make_prior(disrupted_data$disr)),
                  chains = 3, cores = 3, warmup = 1000, iter = 3000,
                 file = "cache/model_4_4_1.rds")

draws <- get_wide_draws(model_4_4_1, levels(disrupted_data$disr))

#calculate bayes_factors
bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, resp)

#Figure 4.4.3
draws %>%
  survey_differences() %>%
  mutate(resp = fct_rev(resp)) %>%
  plot_categ_draws(formula(~diff)) +
    labs(title = "Difference in linear model intercepts, Model 4.4.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
     y = "Cluster", x = "Difference") +
  bayes_factor_text(bfs)

#Figure 4.4.4
plot_survey_cluster(diaries_wide, in_q = sex, subset = "^(Rest|Disrupted)")

model_4_4_2 <- brm(disr_unpaid | weights(crudewt2) ~ 1 + dclasuk + dagegrp + (1|survey:sex),
                  family = categorical,
                  data = disrupted_data,  
                  prior = make_prior(disrupted_data$disr_unpaid),
                  chains = 3, cores = 3, warmup = 1000, iter = 3000,
                 file = "cache/model_4_4_2.rds")

draws <- get_wide_draws(model_4_4_2, levels(disrupted_data$disr_unpaid))

#calculate bayes factors
bfs <- draws %>%
  sex_differences() %>%
  get_bfs(diff, resp, survey)

#Figure 4.4.5
draws %>%
  sex_differences() %>%
  plot_categ_draws(formula(~survey)) +
    labs(title = "Difference in linear model intercepts, Model 4.4.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
     y = "Cluster Group", x = "Female - Male") +
  bayes_factor_text(bfs)  +
  theme(plot.title.position = "plot")
draws <- get_wide_draws(model_4_4_2b, levels(disrupted_data$disr_unpaid))


#### Other days ####
#figure 4.5.1
diaries_long %>%
  filter(str_detect(description, "^Other")) %>%
  plot_cumulative(desc = T)

#### cluster - mh ####

#cronbach alpha of mh
data %>%
  distinct(mainid, .keep_all = TRUE) %>%
  select(starts_with("nq"), -nq5_1) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(if_all(everything(), ~!is.na(.))) %>%
  cronbach_alpha()

#link not strong overall, but there is a link between 9-5 and disrupted
#note 9-5 no leisure is counting as no leisure not 9 to 5
cluster_mh_data <- diaries_wide %>%
  select(cluster, cl_desc, crudewt2, wrk_now, sex, has_kids, mh) %>%
  mutate(cluster_group = case_when( str_detect(cl_desc, "9 to 5") & !str_detect(cl_desc, "no leisure") ~ "9to5WithLeisure",
                                   str_detect(cl_desc, "Workday") ~ "OtherWorkday",
                                   str_detect(cl_desc, "Disrupted|Rest") ~ "DisruptedorRest",
                                   str_detect(cl_desc, "late start") ~ "LateStart",
                                   str_detect(cl_desc, "Unpaid and Leisure") ~ "UnpaidAndLeisure",
                                   str_detect(cl_desc, "Unpaid Workday") ~ "UnpaidWorkday",
                                   TRUE ~ "OtherNotWorkday")) %>%
  filter(if_all(c(mh, wrk_now), ~ !is.na(.)))

#number of cases
nrow(cluster_mh_data)

model_4_6_1 <- brm(mh | weights(crudewt2) ~ 1 + wrk_now + sex + has_kids + (1|cluster_group),
                  family = hurdle_gamma,
                              data = cluster_mh_data,
                              prior = c(prior(exponential(3), class = "sd"),
                                        prior(normal(0, 1), class = "b"),
                                  prior(gamma(5, 1), class = "shape"),
                                  prior(beta(3, 3), class = "hu"),
                                  prior(normal(0, 1), class = "Intercept")),
                              chains = 3, cores = 3, iter = 3000, warmup = 1000,
                             control = list(adapt_delta = 0.95),
                              file = "cache/model_4_6_1.rds")

#Figure 4.6.1, posterior predictive check
preds <- posterior_predict(model_4_6_1, nsamples = 30) %>%
  t() %>%
  as_tibble() %>%
  add_column(cluster_mh_data) %>%
  pivot_longer(starts_with("V"), names_to = "draw") 

preds %>%
  ggplot(aes(x = value)) +
  geom_line(stat = "density", alpha = .3, colour = "#833437FF", aes(group = interaction(draw))) +
  geom_line(stat = "density", data = cluster_mh_data,
            aes(x = mh, weight = crudewt2),
            size = 1, linetype = "longdash", colour = "#833437FF") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 3), oob = scales::oob_keep, expand = c(0, 0, 0, 0)) +
  scale_y_continuous(expand = c(0, 0, .1, 0)) +
  scale_colour_discrete_sequential(palette = "#583B2BFF") +
  labs(title = "Posterior predictions of mental health responses, Model 4.6.1",
       subtitle = "with original data as thick dashed line",
       x = "Mental Health (lower is better)", y = "Density (weighted)",
       caption = "30 posterior samples") +
  my_theme()

#Figure 4.6.2
cluster_differences <- gather_draws(model_4_6_1, r_cluster_group[cluster_group,]) %>%
  ungroup() %>%
  pivot_wider(names_from = cluster_group, values_from = .value) %>%
  mutate(UnpaidAndLeisure - OtherNotWorkday,
         LateStart - OtherNotWorkday,
         DisruptedorRest - OtherNotWorkday,
         `9to5WithLeisure` - OtherWorkday) %>%
  pivot_longer(contains(" - "), names_to = "diff")

#calculate bayes factors
bfs <- get_bfs(cluster_differences, diff)

ggplot(cluster_differences, aes(x = value, y = diff)) +
  stat_interval(.width = c(.5, .8, .95)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(expand = c(.3, 0, .1, 0)) +
  intervals_colour_scale +
  labs(title = "Linear model intercepts, Model 4.6.1",
     subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
   y = "Predictor", x = "Value") +
  my_theme() +
  theme(plot.title.position = "plot") +
  bayes_factor_text(bfs)
