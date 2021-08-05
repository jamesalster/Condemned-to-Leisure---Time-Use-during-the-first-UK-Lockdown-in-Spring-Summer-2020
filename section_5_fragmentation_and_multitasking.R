
library(tidyverse)
library(brms)
library(tidybayes)

#### Import Data #### 
source("global_variables.R")
source("process_respondent_data.R")

#### Create Indicators ####

## Fragmentation ##

all_regex <- "pri|sec|enj|loc|dev|who[abcd]?[0-9]+"

all_long <-  data %>%
  select(diaryid, matches(all_regex), -c(pricrech, chcarenj)) %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  #convert to long, with separate category and slot columns
  pivot_longer(matches(all_regex), names_to = "category_slot", values_to = "value") %>%
  mutate(category = str_extract(category_slot, c("pri|sec|enj|loc|dev|who[abcd]")),
         slot = as.numeric(str_extract(category_slot, "[0-9]+")), .keep = "unused") %>%
  pivot_wider(names_from = category, values_from = value)

#function to detect activity changes
get_change <- function(vec) {
  vec2 <- coalesce(vec, "PLACEHOLDER")
  lagged <- lag(vec2, default = "BEGINNING")
  vec2 != lagged
}

all_spells <- all_long %>%
  group_by(diaryid) %>%
  mutate(pri_activity = pri,
         across(pri:whod, get_change),
         any_change = pri | sec | enj | dev | whoa | whob | whoc | whod,
         period = cumsum(any_change)) %>%
  group_by(diaryid, period) %>%
  summarise(begin = min(slot),
            end = max(slot),
            pri_activity = unique(pri_activity),
            length = end - begin + 1,
            .groups = "drop") %>%
  left_join(activity_key, by = c("pri_activity" = "original_level")) 

diary_spells <- all_spells %>%
  group_by(diaryid) %>%
  summarise(n_spells = n(),
            sd_length = sd(length, na.rm = T))
            
#create dataset, keeping only cases in sample
spells_data <- left_join(respondent_data,diary_spells, by = "diaryid") %>%
  mutate(telework = coalesce(telework, "Not Workday"))

## Multitasking ##

#get activity info for multitasking
multitasking <- activity_long %>%
  filter(!is.na(sec_tidied),
         pri_tidied != sec_tidied) %>%
  count(diaryid, name = "n_multi") 
  
#create dataset
multi_data <- multitasking %>%
  right_join(respondent_data, by = "diaryid") %>% #filter so only diaries in sample are used
  mutate(n_multi = coalesce(n_multi, 0), #make no multitasking 0 not NA
         prop_multi = n_multi / 144)

##### 5.1 Fragmentation by Male-Female and Kids #####

#Figure 5.1.1
spells_data %>%
  group_by(survey, sex, has_kids) %>%
  summarise(mean_n = weighted.mean(n_spells, w = crudewt2)) %>%
  ggplot(aes(x = survey, fill = sex, y = mean_n)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_n, 1)), position = position_dodge(width = .9),
            size = 4, vjust = -.3) +
  facet_wrap(~has_kids) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
  scale_fill_manual(values = mf_palette, name = "Sex") +
  labs(title = "Number of periods in a diary day, by Male / Female and Children",
       y = "Mean number of periods (weighted)", x = "Number of Periods") +
  my_theme()
  
#Figure 5.1.2
all_spells %>%
  right_join(respondent_data, by = "diaryid") %>%
  mutate(coded5 = code5(activity_group)) %>%
  group_by(sex, coded5, survey, diaryid, crudewt2) %>%
  summarise(n_periods = n(), .groups = "drop") %>%
  group_by(sex, coded5, survey) %>%
  summarise(weighted_n_periods = sum(n_periods * crudewt2), .groups = "drop") %>%
  ggplot(aes(x = coded5, fill = sex, y = weighted_n_periods)) +
  geom_col(position = "dodge") +
  facet_wrap(~survey) +
  scale_fill_manual(values = mf_palette, name = "Sex") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(expand = c(0, 0, .1, 0)) +
  labs(title = "Number of activity periods by Activity Group, Male / Female and Survey Wave",
       y = "Total number of activity periods (weighted)", x = "Activity Group") +
  my_theme()

#Model 5.1.1
model_5_1_1 <- brm(n_spells | weights(crudewt2) ~ 1 + dagegrp + dclasuk + day_type + (1|sex:has_kids:survey),
                              family = negbinomial,
                              data =  spells_data,
                              prior = c(prior(normal(0, 5), class = "Intercept"),
                                        prior(normal(0, 2), class = "b"),
                                        prior(exponential(3), class = "sd"),
                                        prior(gamma(5, 1), class = "shape")),
                              chains = 3, cores = 3, warmup = 1000, iter = 3000,
                              file = "cache/model_5_1_1.rds")

draws <- get_wide_draws(model_5_1_1)

bfs_sex <- draws %>%
  sex_differences() %>%
  get_bfs(has_kids, survey)

bfs_kids <- draws %>%
  kids_differences() %>%
  get_bfs(sex, survey)

#Figure 5.1.3
draws %>%
  sex_differences() %>%
  mutate(survey = fct_rev(survey)) %>%
  plot_draws(cols = has_kids, survey_on_y = T) +
  labs(title = "Linear Model Intercepts\nModel 5.1.1, predicting Number of Periods in terms of Male/Female, Children, and Survey Wave",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       x = "Female - Male", y = "Survey Wave",
       caption = "Model estimating the mean, log link") +
  bayes_factor_text(bfs_sex)

#Figure 5.1.4
draws %>%
  kids_differences() %>%
  mutate(survey = fct_rev(survey)) %>%
  plot_draws(cols = sex, survey_on_y = T) +
  labs(title = "Linear Model Intercepts\nModel 5.1.1, predicting Number of Periods in terms of Male/Female, Children, and Survey Wave",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       x = "Children - No Children", y = "Survey Wave",
       caption = "Model estimating the mean, log link") +
    bayes_factor_text(bfs_kids)

#compute quoted bayes factor
draws %>%
  sex_differences() %>%
  pivot_wider(names_from = survey, values_from = value, id_cols = c(has_kids, .draw)) %>%
  group_by(has_kids) %>%
  summarise(bf_june_2016 = sum(`June 2020` > `2016`) / sum(`June 2020` < `2016`),
            bf_august_june = sum(`August 2020` > `June 2020`) / sum(`August 2020` < `June 2020`),
            bf_august_2016 = sum(`August 2020` > `2016`) / sum(`August 2020` < `2016`)) %>%
  mutate(across(where(is.numeric), ~ 1 / .)) #find inverse

##### Fragmentation over survey waves ####

n_length <- spells_data %>%
  group_by(survey) %>%
  summarise(mean_n = weighted.mean(n_spells, w = crudewt2), .groups = "drop")

#as barplot
barplot <- ggplot(n_length, aes(x = "", y = mean_n, fill = survey)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_n, 1)), position = position_dodge(width = .9), vjust = -.2) +
  scale_fill_manual(values = survey_palette, name = "Survey Wave") +
  scale_y_continuous(expand = c(0, .1, .1, 0)) +
  labs(x = "", y = "Mean number of periods (weighted)") +
  my_theme()

#as density
density <- spells_data %>%
  ggplot(aes(x = n_spells, colour = survey)) +
  geom_line(stat = "density", size = 1.5, aes(weight = crudewt2)) +
  geom_vline(data = n_length, linetype = "dashed", aes(xintercept = mean_n, colour = survey)) +
  scale_x_continuous(limits = c(0, 50), oob = scales::oob_keep) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
  scale_colour_manual(values = survey_palette, name = "Survey Wave") +
  labs(x = "Number of periods", y = "Density (weighted)") +
  my_theme() 

#Figure 5.1.5
barplot + density + plot_annotation(title = "Number of Periods in a diary day, by Survey Wave")

model_5_1_2 <- brm(bf(n_spells | weights(crudewt2) ~ 1 + dagegrp + sex + dclasuk + has_kids + (1|survey),
                               shape ~ 1 + dagegrp + sex + dclasuk + has_kids + (1|survey)),
                               family = negbinomial,
                               data =  spells_data,
                               prior = c(prior(normal(0, 5), class = "Intercept"),
                                         prior(normal(0, 5), class = "Intercept", dpar = "shape"),
                                         prior(normal(0, 2), class = "b"),
                                         prior(normal(0, 2), class = "b", dpar = "shape"),
                                         prior(exponential(3), class = "sd"),
                                         prior(exponential(3), class = "sd", dpar = "shape")),
                               chains = 3, cores = 3, warmup = 1000, iter = 3000,
                               control = list(adapt_delta = 0.95),
                               file = "cache/model_5_1_2.rds")


draws <- get_wide_draws(model_5_1_2)

bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, coef)

#Figure 5.1.6
draws %>%
  survey_differences() %>%
  plot_draws(rows =NULL) +
  labs(title = "Linear Model Intercepts\nModel 5.1.2, predicting Number of Periods in terms of Survey Wave",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       x = "Value", y = "Difference",
       caption = "Links: mu = log, shape = log") +
  bayes_factor_text(bfs)

model_5_1_3 <- update(model_5_1_2,
                    formula = bf(n_spells | weights(crudewt2) ~ 1 + dagegrp + sex + dclasuk + has_kids + workday + (1|survey:in_work),
                      shape ~ 1 + dagegrp + sex + dclasuk + has_kids + workday + (1|survey:in_work)),
                   newdata =  spells_data,
                   chains = 3, cores = 3, warmup = 1000, iter = 3000,
                   control = list(adapt_delta = 0.95),
                   file = "cache/model_5_1_3.rds")

bfs <- draws %>%
  working_differences() %>%
  get_bfs(diff, coef, survey)

#Figure 5.1.7
draws %>%
  working_differences() %>%
  plot_draws(survey_on_y = T, rows = NULL) +
  labs(title = "Linear Model Intercepts\nModel 5.1.3, predicting Number of Periods in terms of Survey Wave and Working",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Working - Not Working",
       caption = "Links: mu = log, shape = log") +
  bayes_factor_text(bfs)

bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, coef, in_work)

#Figure 5.1.8
draws %>%
  survey_differences() %>%
  plot_draws(rows = in_work) +
  labs(title = "Linear Model Intercepts\nModel 5.1.3, predicting Number of Periods in terms of Survey Wave and Working",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference",
       caption = "Links: mu = log, shape = log") +
  bayes_factor_text(bfs)

#Figure 5.1.9
n_length <- spells_data %>%
  group_by(day_type) %>%
  summarise(mean_n = weighted.mean(n_spells, w = crudewt2), .groups = "drop")

spells_data %>%
  filter(!is.na(day_type)) %>%
  ggplot(aes(x = n_spells, colour = day_type)) +
  geom_line(stat = "density", size = 1.2, aes(weight = crudewt2)) +
  geom_vline(data = n_length, linetype = "dashed", aes(xintercept = mean_n, colour = day_type)) +
  geom_text(data = n_length, aes(x = mean_n, y = Inf, angle = 90,
                                 hjust = 1.2, vjust = 1.2,
                                 label = round(mean_n, 1))) +
  scale_x_continuous(limits = c(0, 50), oob = scales::oob_keep, expand = c(0, 0, 0, 0)) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
  scale_colour_manual(values = daytype_palette, name = "Day Type") +
  labs(title = "Distribution of number of periods in a diary day, by Day Type",
       y = "Density (weighted)", x = "Number of periods", caption = "dashed line shows the mean. Means for 'Not Workday, Working' and 'Outside Workday' are identical.") +
  my_theme()


#### Fragmentation and Cluster #### 

#model that
model_5_1_4 <- brm(n_spells | weights(crudewt2) ~ 1 + (1|cluster),
                      family = negbinomial,
                      data = spells_data,
                      prior = c(prior(exponential(1), class = "sd"),
                                prior(normal(0, 5), class = "Intercept")),
                                prior(gamma(5, 1), class = "shape"),
                      chains = 3, cores = 3, warmup = 1000, iter = 3000,
                      file = "cache/model_5_1_4.rds")

draws <-  gather_draws(model_5_1_4, r_cluster[cluster,]) %>%
  ungroup() %>%
  filter(cluster != 37) %>%
  left_join(distinct(respondent_data, cluster, cl_desc, workday), by = "cluster") %>%
  mutate(value = .value,
         cl_desc = fct_reorder(cl_desc, value, mean)) 

bfs <- get_bfs(draws, cl_desc, workday)

#Figure 5.1.10
draws %>%
  ggplot(aes(x = .value, y = cl_desc)) +
  stat_interval(.width = c(.5, .8, .95), point_interval = mean_qi) +
  geom_vline(xintercept = 0) +
  facet_wrap(~workday, ncol = 1, scales = "free_y") +
  scale_x_continuous(expand = c(0, .2, 0, .1)) +
  intervals_colour_scale +
  my_theme() +
  theme(plot.title.position = "plot") +
  geom_text(data = bfs, aes(label = label), x = -Inf, vjust = .5, hjust = -.2) +
  labs(title = "Linear Model Intercepts\nModel 5.1.4, predicting Number of Periods in terms of Diary Day Cluster",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       x = "Intercept", y = "Cluster", 
       caption = "Cluster 37 not shown (value is ~ -1.5). Log link.") 

#### 5.2 Multitasking ####

##### Secondary Activity NAs ####

#80% for secondary activity
sum(is.na(activity_long$sec_tidied)) / nrow(activity_long)
#13% of time is multitasking
1 - with(activity_long, sum(is.na(sec_tidied) | pri_tidied == sec_tidied)) / nrow(activity_long)
#44% of diaries have some multitasking
sum(multi_data$prop_multi > 0) / nrow(multi_data)
  
##### Multitasking and Children ####

#FIgure 5.2.1
multi_data %>%
  filter(n_multi > 0) %>%
  group_by(has_kids, survey) %>%
  summarise(mean_multi = weighted.mean(prop_multi, w = crudewt2), .groups = "drop") %>%
  ggplot(aes(x = survey, y = mean_multi, fill = has_kids)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = str_c(round(mean_multi, 2) * 100, "%")), vjust = -.5, size = 6,
            position = position_dodge(width = .9)) +
  scale_fill_manual(values = kids_palette, name = "Children") +
  scale_y_continuous(expand = c(0, 0, .1, .05),
                     labels = scales::label_percent(accuracy = 1)) +
  labs(y = "Mean proportion of day spent multitasking (weighted)", x = "Survey Wave",
       title = "Multitasking by Survey Wave and Workday Type",
       subtitle = "Only days where some multitasking reported") +
  my_theme()

#Model 5.2.1
#zero inflated to 'capture' non response
model_5_2_1 <- brm(bf(n_multi | weights(crudewt2) ~ 1 +  dagegrp + sex + dclasuk + (1|has_kids:survey),
                              zi ~ 1 + dagegrp + sex + dclasuk + (1|has_kids:survey)),
                           family = zero_inflated_negbinomial,
                           data = multi_data,
                           prior = c(prior(exponential(3), class = "sd"),
                                     prior(gamma(2, 1), class = "sd", dpar = "zi"),
                                     prior(normal(0, 5), class = "Intercept"),
                                     prior(normal(0, 5), class = "Intercept", dpar = "zi"),
                                     prior(gamma(2, 1), class = "shape")),
                           chains = 3, cores = 3, iter = 3000, warmup = 1000,
                           file = "cache/model_5_2_1.rds")

draws <- get_wide_draws(model_5_2_1)

bfs <- draws %>%
  kids_differences() %>%
  get_bfs(diff, survey, coef)

#Figure 5.2.2
draws %>%
  kids_differences() %>%
  plot_draws(survey_on_y = TRUE) +
  labs(title = "Linear model intercepts\nModel 5.2.1, predicting Number of Time Multitasking in terms of Children and Survey Wave",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       x = "Children - No Children", y = "",
       caption = "Links: mean = log, zi = logit") +
  bayes_factor_text(bfs)

#Figure 5.2.3
multi_data %>%
  filter(n_multi > 0) %>%
  group_by(sex, has_kids, survey) %>%
  summarise(mean_multi = weighted.mean(prop_multi, w = crudewt2)) %>%
  ggplot(aes(x = survey, y = mean_multi, fill = sex)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = str_c(round(mean_multi, 2) * 100, "%")), vjust = -.5, size = 5,
            position = position_dodge(width = .9)) +
  scale_fill_manual(values = mf_palette, name = "Sex") +
  scale_y_continuous(expand = c(0, 0, .1, .05),
                     labels = scales::label_percent(accuracy = 1)) +
  labs(y = "Mean proportion of day spent multitasking (weighted)", x = "Survey Wave",
       title = "Multitasking by Sex, Survey Wave and Children",
       subtitle = "Only days where some multitasking reported") +
  facet_wrap(~has_kids) +
  my_theme()

###### Workday Type #####

all_workdays <- multi_data %>%
  filter(prop_multi > 0) %>%
  group_by(survey) %>%
  summarise(mean_multi = weighted.mean(prop_multi, w = crudewt2)) 

#Figure 5.2.4
multi_data %>%
  filter(prop_multi > 0,
         !is.na(day_type)) %>%
  group_by(day_type, survey) %>%
  summarise(mean_multi = weighted.mean(prop_multi, w = crudewt2)) %>%
  bind_rows(all_workdays) %>%
  mutate(day_type = coalesce(day_type, "All Diary Days")) %>%
  ggplot(aes(x = day_type, y = mean_multi, fill = survey)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = str_c(round(mean_multi, 2) * 100, "%")), vjust = -.5, size = 5,
            position = position_dodge(width = .9)) +
  scale_fill_manual(values = survey_palette, name = "Survey Wave") +
  scale_y_continuous(expand = c(0, 0, .1, 0),
                     labels = scales::label_percent(accuracy = 1)) +
  labs(y = "Mean proportion of day spent multitasking (weighted)", x = "Survey Wave",
       title = "Multitasking by Survey Wave and Workday Type",
       subtitle = "Only days where some multitasking reported") +
  my_theme()

#Model_5_2_2
model_5_2_2 <- brm(bf(n_multi | weights(crudewt2) ~ 1 + sex + has_kids + dagegrp + dclasuk + (1|day_type),
                      zi ~ 1 + sex + has_kids + dagegrp + dclasuk + (1|day_type)),
                 family = zero_inflated_negbinomial,
                 data = multi_data,
                 prior = c(prior(normal(0, 2), class = "Intercept"),
                           prior(normal(0, 2), class = "Intercept", dpar = "zi"),
                           prior(normal(0, 2), class = "b"),
                           prior(normal(0, 2), class = "b", dpar = "zi"),
                           prior(gamma(5, 1), class = "shape"),
                           prior(exponential(3), class = "sd"),
                           prior(exponential(3), class = "sd", dpar = "zi")),
                 chains = 3, cores = 3, iter = 3000, warmup = 1000,
                 file = "cache/model_5_2_2.rds")

draws <- get_wide_draws(model_5_2_2) 

bfs <- draws %>%
  day_type_differences() %>%
  get_bfs(diff, coef)

#Figure 5.2.5
draws %>%
  day_type_differences() %>%
  plot_draws(rows = NULL, cols = coef) +
  labs(title = "Linear Model Intercepts: Model 5.2.2, Multitasking by Workday Type",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "links: mu = log, zi = logit") +
  bayes_factor_text(bfs)

##### Multitasking and cluster #####

multi_95_data <- multi_data %>%
  filter(workday == "Workday") %>%
  mutate(workday95 = if_else(str_detect(description, "Workday 9 to 5"), "Workday 9 to 5", "Other Workday"))

multi_95_means <- multi_95_data %>%
  filter(n_multi > 0) %>%
  group_by(workday95, survey) %>%
  summarise(mean_multi = weighted.mean(prop_multi, w = crudewt2, na.rm = T))

#Figure 5.2.6
multi_95_data %>%
  filter(n_multi > 0) %>%
  group_by(workday95, survey) %>%
  summarise(mean_multi = weighted.mean(prop_multi, w = crudewt2, na.rm = T)) %>%
  ggplot(aes(y = mean_multi, x = survey, fill = workday95)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = str_c(round(mean_multi, 2) * 100, "%")), vjust = -.5, size = 6,
            position = position_dodge(width = .9)) +
  scale_fill_manual(values = w95_palette, name = "Workday Type") +
  scale_y_continuous(expand = c(0, 0, .1, 0.05),
                     labels = scales::label_percent(accuracy = 1)) +
  labs(y = "Mean proportion of day spent multitasking (weighted)", x = "Survey Wave",
       title = "Multitasking by Survey Wave and Workday Type",
       subtitle = "Only days where some multitasking reported") +
  my_theme()

#Model 5.2.3
model_5_2_3 <- brm(bf(n_multi | weights(crudewt2) ~ 1 + sex + has_kids + dagegrp + dclasuk + (1|workday95),
                      zi ~ 1 + sex + has_kids + dagegrp + dclasuk + (1|workday95)),
                      family = zero_inflated_negbinomial,
                      data = multi_95_data,
                      prior = c(prior(exponential(3), class = "sd"),
                                prior(exponential(3), class = "sd", dpar = "zi"),
                                prior(normal(0, 2), class = "b"),
                                prior(normal(0, 2), class = "b", dpar = "zi"),
                                prior(normal(0, 5), class = "Intercept"),
                                prior(normal(0, 5), class = "Intercept", dpar = "zi"),
                                prior(gamma(2, 1), class = "shape")),
                      chains = 3, cores = 3, warmup = 1000, iter = 3000,
                      file = "cache/model_5_2_3.rds")

draws <- get_wide_draws(model_5_2_3)

draws_95difference <- draws %>%
  pivot_wider(names_from = workday95, values_from = value) %>%
  mutate(`Workday 9 to 5` - `Other Workday`) %>%
  pivot_longer(contains(" - "), names_to = "diff")

bfs <- get_bfs(draws_95difference, diff, coef) 

#Figure 5.2.7
draws_95difference %>%
  ggplot(aes(x = value, y = coef)) +
  stat_interval(.width = c(.5, .8, .95)) +
  geom_vline(xintercept = 0) +
  intervals_colour_scale +
  scale_x_continuous(expand = c(.3, 0, .1, 0)) +
  my_theme() +
    labs(title = "Linear Model Intercepts: Model 5.2.3, Multitasking by Workday Type",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Parameter", x = "`Workday 9 to 5 - Other Workday", caption = "links: mu = log, zi = logit") +
  bayes_factor_text(bfs)

##### multitasking an cluster entropy ####
diaries_long <- right_join(primary_long, multi_data)

#max entropy
log(n_distinct(diaries_long$coded5_activity))

entropy <- diaries_long %>%
  group_by(cl_desc, slot, coded5_activity) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  summarise(H = -sum(prop * log(prop)), .groups = "drop_last") %>%
  summarise(mean_H = mean(H))

mean_multi <- multi_data %>%
  group_by(cl_desc) %>%
  summarise(mean_multi = weighted.mean(n_multi, w = crudewt2) / 144)

multi_entropy_grouped <- full_join(entropy, mean_multi) 

#Figure 5.2.8
multi_entropy_grouped %>%
  filter(!str_detect(cl_desc, "37")) %>%
  mutate(number = str_extract(cl_desc, "[0-9]{1,2}"),
         group = str_extract(cl_desc, "[A-Z][^,]*")) %>%
  ggplot(aes(x = mean_H, y = mean_multi, colour = group)) +
  geom_point() +
  geom_text(aes(label = number), nudge_x = 0.02) +
  scale_x_continuous(breaks = scales::breaks_width(.1)) +
  scale_y_continuous(label = scales::label_percent(accuracy = 1), breaks = scales::breaks_width(.05)) +
  scale_colour_manual(values = cluster_group_palette, name = "Type of Routine") +
  labs(title = "Diary Day Clusters in terms of average activity entropy and average rates of multitasking",
       x = "Average Activity Entropy", y = "Average Proprotion of day Multitasking (Weighted)",
       caption = "Cluster 37 not shown (Entropy = 0, Proportion multitasking = 24%)") +
  my_theme()

multi_entropy <- left_join(multi_data, entropy, by = "cl_desc")

model_5_2_4 <- brm(bf(n_multi | weights(crudewt2) ~ 1 + sex + has_kids + dclasuk + dagegrp + mean_H,
                      zi ~ 1 + sex + has_kids + dclasuk + dagegrp + mean_H),
                 family = zero_inflated_negbinomial,
                 data = multi_entropy,
                 prior = c(prior(normal(0, 2), class = "Intercept"),
                           prior(normal(0, 2), class = "Intercept", dpar = "zi"),
                           prior(normal(0, 2), class = "b"),
                           prior(normal(0, 2), class = "b", dpar = "zi"),
                           prior(gamma(5, 1), class = "shape")),
                 chains = 3, cores = 3, iter = 3000, warmup = 1000,
                 file = "cache/model_5_2_4.rds")

draws <- gather_draws(model_5_2_4, b_mean_H, b_zi_mean_H) %>%
  rename(value = .value, coef = .variable) %>%
  mutate(coef = if_else(coef == "b_mean_H", "mu", "zi"))

bfs <- get_bfs(draws, coef)

#Figure 5.2.9
ggplot(draws, aes(x = value, y = coef)) +
    stat_interval(.width = c(.5, .8, .95)) +
  geom_vline(xintercept = 0) +
  intervals_colour_scale +
  scale_x_continuous(expand = c(.3, 0, .1, 0)) +
  my_theme() +
  labs(title = "Linear Model Parameters: Model 5.2.4, Multitasking in terms of Diary Day Cluster Entropy",
       subtitle = "Showing Bayes factors for H1, x > 0, against H0, x < 0", 
       y = "Parameter", x = "Coefficient of Mean Cluster Entropy", caption = "links: mu = log, zi = logit") +
  bayes_factor_text(bfs) +
  theme(plot.title.position = "plot")

#### 5.3 Multitasking and Mental Health ####

cut_multi <- function(x, n) {
  x %>%
    cut(seq(0, 1, length.out = n + 1), ordered_result = TRUE) %>%
    coalesce("0") %>%
    fct_relevel("0")
}

multi_mh_data <- multi_data %>%
  filter(!is.na(mh), !is.na(wrk_now))

#Figure 5.3.1
multi_mh_data %>%
  mutate(proportion_multitasking = cut_multi(prop_multi, 3)) %>%
  left_join(count(., proportion_multitasking, wt = crudewt2, name = "n_obs")) %>%
  ggplot(aes(x = mh, fill = n_obs, weight = crudewt2)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_wrap(~proportion_multitasking, labeller = label_both) +
  scale_y_continuous(expand = c(0, 0, .1, 0)) +
  scale_fill_stepsn(n.breaks = 3, colours = obs_palette, name = "Number of Observations", show.limits = TRUE) +
  labs(title = "Distribution of subjective mental health responses",
       subtitle = "by proportion of time spent multitasking",
       x = "Mental Health Responses (lower is better)", y = "Density (weighted)") +
  my_theme()

#model that
model_5_3_1 <- brm(mh | weights(crudewt2) ~ 1 + sex + has_kids + dagegrp + dclasuk + wrk_now + prop_multi,
                      family = hurdle_gamma,
                      data = multi_mh_data,
                      prior = c(prior(normal(0, 2), class = "b"),
                                prior(normal(0, 2), class = "Intercept"),
                                prior(beta(3, 3), class = "hu"),
                                prior(gamma(5, 1), class = "shape")),
                      chains = 3, cores = 3, iter = 3000, warmup = 1000,
                      file = "cache/model_5_3_1.rds")

draws <- gather_draws(model_5_3_1, `b_.*`, regex = T) %>%
  ungroup() %>%
  rename(value = .value) %>%
  filter(str_detect(.variable, "wrk_now|prop_multi")) %>%
  mutate(.variable = fct_recode(.variable,
                                "Proportion of time spent multitasking" = "b_prop_multi",
                                "work_now = part time" = "b_wrk_nowparttime",
                                "work_now = not working" = "b_wrk_nownotworking",
                                "work_now = furlough" = "b_wrk_nowfurlough") %>% fct_rev()) 

bfs <- get_bfs(draws, .variable)

#Figure 5.3.2
ggplot(draws, aes(x = value, y = .variable)) +
  stat_interval(.width = c(.5, .8, .95)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(expand = c(.3, 0, .1, 0)) +
  intervals_colour_scale +
  labs(title = "Linear Model Parameters: Model 5.3.1, Mental Health in Lockdown in terms of Multitasking",
       subtitle = "Showing Bayes factors for H1, x > 0, against H0, x < 0\nReference: work_now = full time", 
       y = "Parameter", x = "Value", caption = "log link") +
  my_theme() +
  bayes_factor_text(bfs) +
  theme(plot.title.position = "plot")

#calculate effect, with link function
draws %>%
  filter(str_detect(.variable, "Proportion")) %>%
  with(exp(mean(value)))

#### END ####
