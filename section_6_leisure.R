
library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

#### Import Data #### 
source("global_variables.R")
source("process_respondent_data.R")

#### Calculate indicators ####
#using tbl primary_long sourced from process_respondent_data.R

#get breakdown of leisure_time
#if no leisure, value is NA
leisure_proportions <- primary_long %>%
  filter(str_detect(activity_group, "leisure")) %>%
  group_by(diaryid, pri_tidied) %>%
  summarise(time = n(), .groups = "drop_last") %>%
  mutate(prop = time / sum(time), .keep = "unused") %>%
  ungroup() %>%
  pivot_wider(names_from = pri_tidied, values_from = prop, values_fill = 0) 

#get voracity indicator
outside_leisure <- c("Consuming services", "Church, Temple, Synagogue, Prayer",
                     "Walking, Jogging", "Playing sports, exercise",
                     "Going out to eat, drink", "Walking, dog walking")
voracity <- primary_long %>%
  filter(str_detect(activity_group, "leisure")) %>%
  group_by(diaryid) %>%
  summarise(voracity = n_distinct(pri_tidied), 
            outside_voracity = n_distinct(pri_tidied[pri_tidied %in% outside_leisure]), .groups = "drop")

#get leisure time
leisure_time <- primary_long %>%
  group_by(diaryid) %>%
  summarise(leisure_time = sum(str_detect(coded5_activity, "^leisure"))) 

#join, keeping only cases in smaller sample
leisure_data <- leisure_time %>%
  full_join(leisure_proportions, by = "diaryid") %>%
  full_join(voracity, by = "diaryid") %>%
  right_join(respondent_data, by = "diaryid") # filter to concentrate on smaller sample

#### Intro ####

#correlation of the two indicators
cor(leisure_proportions$`Watching tv, video, dvd, music`, voracity$voracity)

###### 6.1 voracity #####

#voracity
leisure_voracity_model_data <- leisure_data %>%
  select(diaryid, crudewt2, hied2, cluster, sex, dclasuk, dagegrp,
         workday, survey, voracity, outside_voracity, in_work, leisure_time) %>%
  filter(!is.na(voracity), !is.na(in_work)) %>% #174 excluded
  mutate(voracity0 = voracity - 1,
         inside_voracity = voracity - outside_voracity)    #create modelling voracity indicator to fit poisson distribution

#Figure 6.1.1
leisure_voracity_model_data %>%
  rename(inside_leisure = inside_voracity, outside_leisure = outside_voracity) %>%
  pivot_longer(c(inside_leisure, outside_leisure),
               names_to = "location", values_to = "vorac") %>%
  mutate(hied2 = fct_rev(hied2)) %>%
  group_by(hied2, survey, workday, location) %>%
  summarise(mean_voracity = weighted.mean(vorac, w = crudewt2), .groups = "drop") %>%
  ggplot(aes(x = survey, y = mean_voracity, fill = location)) +
  geom_col(width = .7) +
  geom_text(aes(label = round(mean_voracity, 2),
                y = if_else(location == "inside_leisure", 1.25, 0.25)), size = 6) +
  facet_grid(hied2~workday) +
  scale_fill_manual(values = in_out_palette) +
  scale_y_continuous(expand = c(0, 0, .1, 0)) +
  labs(x = "Survey Wave", y = "Mean Number of Leisure Activities per Day, (Weighted)",
       title = "Voracity, in terms of Survey Wave, Activity Location, Workday, and Degree") +
  my_theme()

#Figure 6.1.2
leisure_time %>%
  right_join(respondent_data) %>%
  mutate(leisure_hours = leisure_time / 6) %>% #convert to hours
  group_by(hied2, survey) %>%
  summarise(mean_leisure_hours = weighted.mean(leisure_hours, w = crudewt2), .groups = "drop") %>%
  mutate(rounded = round(mean_leisure_hours, 1),
         h = rounded %/% 1,
         m = round((rounded - h) * 60, 0),
         label = str_glue("{h}h {m}m")) %>%
  ggplot(aes(x = survey, y = mean_leisure_hours, fill = hied2)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = label),
            position = position_dodge(width = .9), vjust = -.3, size = 6) +
  scale_y_continuous(expand = c(0, 0, .1, 0), breaks = scales::breaks_width(1)) +
  scale_fill_manual(values = hied2_palette, name = "Degree") +
  labs(x = "Survey Wave", y = "Mean Hours of Leisure per Day",
       title = "Hours of Leisure per day, by Degree") +
  my_theme()

#show the masking relatinoship
model_6_1_1 <- brm(voracity0 | weights(crudewt2) ~ 1 + (1|hied2),
                        family = poisson(link = "log"),
                        data = leisure_voracity_model_data,
                        prior = c(prior(exponential(3), class = "sd"),
                                  prior(normal(0, 2), class = "Intercept")),
                        chains = 3, cores = 3, iter = 1000, warmup = 500, 
                        file = "cache/model_6_1_1.rds")

model_6_1_2 <- update(model_6_1_1,
                            formula = voracity0 | weights(crudewt2) ~ 1 + leisure_time,
                        newdata = leisure_voracity_model_data,
                        prior = c(prior(normal(0, 2), class = "b"),
                                  prior(normal(0, 2), class = "Intercept")),
                        chains = 3, cores = 3, iter = 1000, warmup = 500,
                        file = "cache/model_6_1_2.rds")

model_6_1_3 <- update(model_6_1_1,
                            formula = voracity0 | weights(crudewt2) ~ 1 + leisure_time + (1|hied2),
                        newdata = leisure_voracity_model_data,
                        prior = c(prior(exponential(3), class = "sd"),
                                  prior(normal(0, 2), class = "b"),
                                  prior(normal(0, 2), class = "Intercept")),
                        chains = 3, cores = 3, iter = 1000, warmup = 500,
                        file = "cache/model_6_1_3.rds")

models_plot1 <- bind_rows(without_leisure_time = gather_draws(model_6_1_1, r_hied2[degree,]),
          with_leisure_time = gather_draws(model_6_1_3, r_hied2[degree,]), 
          .id = "model") %>%
  pivot_wider(names_from = degree, values_from = .value) %>%
  mutate(diff = Degree - `No.Degree`) %>%
  ggplot(aes(x = diff, y = model)) +
  stat_halfeye(position = "dodge", fill = in_out_palette[[1]], .width = 0) +
  labs(y = "Model", x = "Difference in Intercept: Degree - No Degree") +
  my_theme() +
  theme(plot.title.position = "plot")

models_plot2 <- bind_rows(without_degree= gather_draws(model_6_1_2, b_leisure_time),
          with_degree = gather_draws(model_6_1_3, b_leisure_time), 
           .id = "model") %>%
  ggplot(aes(x = .value, y = model)) +
  stat_halfeye(position = "dodge", fill = in_out_palette[[1]], .width = 0) +
  labs(y = "Model", x = "Coefficient for Leisure Time") +
  my_theme() +
  theme(plot.title.position = "plot")

#Figure 6.1.3
(models_plot1 / models_plot2) +
  plot_annotation(title = "Linear Model Parameters when predicting Voracity",
          subtitle = "Comparing models with only Leisure Time as a predictor, only Degree, and both.")

#Model 6.1.4
model_6_1_4 <- brm(voracity0 | weights(crudewt2) ~ 1 + sex + dagegrp + dclasuk + workday + 
                            leisure_time + (1|hied2:survey),
                          family = poisson(link = "log"),
                          data = leisure_voracity_model_data,
                          prior = c(prior(exponential(3), class = "sd"),
                                    prior(normal(0, 2), class = "b"),
                                    prior(normal(0, 2), class = "Intercept")),
                          chains = 3, cores = 3, iter = 3000, warmup = 1000, init = "0",
                          control = list(adapt_delta = .9),
                          file = "cache/model_6_1_4.rds")

draws <- get_wide_draws(model_6_1_4)

bfs_survey <- draws %>%
  survey_differences() %>%
  get_bfs(hied2, diff)

bfs_hied <- draws %>%
  hied2_differences() %>%
  get_bfs(survey, diff)

#Figure 6.1.4
draws %>%
  hied2_differences() %>%
  plot_draws(survey_on_y = T, cols = NULL, rows = NULL)  +
  labs(title = "Linear Model Intercepts: Voracity in terms of Degree and Survey Wave, Model 6.1.4",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Degree - No Degree", caption = "log link") +
  bayes_factor_text(bfs_hied)

#Figure 6.1.5
draws %>%
  survey_differences() %>%
  plot_draws(cols = hied2, rows = NULL) +
  labs(title = "Linear Model Intercepts: Voracity in terms of Degree and Survey Wave, Model 6.1.4",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "log link") +
  bayes_factor_text(bfs_survey)

#Figure 6.1.6
leisure_data %>%
  group_by(survey, hied2, in_work) %>%
  summarise(n = sum(crudewt2), .groups = "drop_last") %>%
  mutate(prop = n / sum(n),
         label = str_c(round(prop * 100, 1), "%")) %>%
  ungroup() %>%
  filter(in_work == "Working") %>%
  ggplot(aes(x = survey, fill = hied2, y = prop)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = label),
            position = position_dodge(width = .9), vjust = -.3, size = 6) +
  scale_fill_manual(values = hied2_palette, name = "Degree") +
  scale_y_continuous(expand = c(0, 0, .1, 0), labels = scales::label_percent()) +
  labs(x = "Survey Wave", y = "Proportion", title = "Proportion of diary days where respondent was in work,\nby Degree and Survey Wave") +
  my_theme()

model_6_1_5 <- update(model_6_1_4,
                      formula = voracity0 | weights(crudewt2) ~ 1 + sex + dagegrp + dclasuk +
                            leisure_time + (1|hied2:survey:in_work),
                          newdata = leisure_voracity_model_data,
                          chains = 3, cores = 3, iter = 3000, warmup = 1000, init = "0",
                          control = list(adapt_delta = .9),
                          file = "cache/model_6_1_5.rds")

draws <- get_wide_draws(model_6_1_5)

bfs_working <- draws %>%
  working_differences() %>%
  get_bfs(hied2, diff, survey)

bfs_survey <- draws %>%
  survey_differences() %>%
  get_bfs(hied2, diff, in_work)

#Figure 6.1.7
draws %>%
  working_differences() %>%
  plot_draws(cols = hied2, survey_on_y = T)  +
  labs(title = "Linear Model Intercepts: Voracity in terms of Degree, Work Status, and Survey Wave, Model 6.1.5",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Working - Not Working", caption = "log link") +
  bayes_factor_text(bfs_working)

#Figure 6.1.8
draws %>%
  survey_differences() %>%
  plot_draws(cols = hied2, rows = in_work) +
  labs(title = "Linear Model Intercepts: Voracity in terms of Degree, Work Status, and Survey Wave, Model 6.1.5",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "log link") +
  bayes_factor_text(bfs_survey)

#Get bayes factor for 2016 difference > June 2020 difference
draws %>%
  hied2_differences() %>%
  pivot_wider(names_from = survey, values_from = value) %>%
  summarise(bf_june = sum(`2016` > `June 2020`) / sum(`2016` < `June 2020`))

#### 6.2 Proportion of media leisure ####

#Figure 6.2.1
leisure_data %>%
  pivot_longer(`Time with friends,family`:`Cinema, theatre, sport etc.`, names_to = "leisure_type", values_to = "prop") %>%
  filter(!str_detect(leisure_type, "Church")) %>%
  group_by(leisure_type) %>%
  summarise(prop_0 = sum(crudewt2[prop == 0], na.rm = T) / sum(crudewt2, na.rm = T),
            .groups = "drop") %>%
  mutate(leisure_type = str_remove(leisure_type, "leisure_"),
         leisure_type = fct_reorder(leisure_type, prop_0),
         leisure_type = fct_rev(leisure_type)) %>%
  ggplot(aes(y = leisure_type, x = prop_0)) +
  geom_col(fill = in_out_palette[[2]]) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = scales::breaks_width(.2), expand = c(0, 0, .1, 0)) +
  labs(title = "Proportion of diaries with no periods of a given leisure type",
       x = "Percent", y = "Leisure Type") +
  my_theme() +
  theme(plot.title.position = "plot")

#create dataset
leisure_media_data <- leisure_data %>%
  select(crudewt2, survey, cluster, hied2, dclasuk, workday, sex, 
         dagegrp, cl_desc, in_work,
         leisure_media  = `Watching tv, video, dvd, music`) %>% 
  filter(!is.na(leisure_media), !is.na(in_work)) #174 cases

#TV as prop of all leisure
primary_long %>%
  filter(str_detect(coded5_activity, "^leisure")) %>%
  summarise(prop_media = sum(str_detect(activity_group, "media")) / n())

media_means2 <- leisure_media_data %>%
  group_by(survey, hied2, in_work) %>%
  summarise(mean_media = mean(leisure_media, na.rm = T), .groups = "drop")

leisure_media_data %>%
  ggplot(aes(x = leisure_media, fill = hied2, colour = hied2)) +
  geom_histogram(binwidth = .05, colour = "black", aes(weight = crudewt2)) +
  geom_vline(data = media_means2, colour = "black", aes(xintercept = mean_media)) + 
  geom_text(data = media_means2, colour = "black", aes(x = mean_media, y = Inf, angle = 90, 
                                          hjust = 1.2, vjust = 1.2 ,
                                          label = round(mean_media, 2))) +
  facet_grid(survey ~hied2 + in_work, labeller = label_wrap_gen(12)) +
  scale_x_continuous(expand = c(0, 0, 0, 0)) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0), limits = c(0, NA)) +
  scale_colour_manual(values = lighten(hied2_palette, amount = .2), guide = FALSE) +
  scale_fill_manual(values = hied2_palette, guide = FALSE) +
  labs(title = "Distribution of the proportion of leisure time spent in electronic media consumption",
       subtitle = "by Qualification and Survey Wave, showing the weighted mean for each panel",
       x = "Proportion of leisure time", y = "Number of Respondents (weighted)") +
  my_theme()

#Gaussian model works
model_6_2_1 <- brm(leisure_media | weights(crudewt2) ~ 1 + sex + dagegrp + workday + (1|hied2:survey),
                    family = gaussian,
                    data = leisure_media_data,
                    prior = c(prior(exponential(3), class = "sd"),
                              prior(normal(0, 2), class = "Intercept"),
                              prior(normal(0, 2), class = "b")),
                    chains = 3, cores = 3, iter = 3000, warmup = 1000,
                    file = "cache/model_6_2_1.rds")

draws <- get_wide_draws(model_6_2_1)

bfs_hied <- draws %>%
  hied2_differences() %>%
  get_bfs(diff, survey, coef)

bfs_survey <- draws %>%
  survey_differences() %>%
  get_bfs(diff, hied2, coef)

#Figure 6.2.3
draws %>%
  hied2_differences() %>%
  plot_draws(rows = NULL, cols = NULL, survey_on_y = T) +    
  labs(title = "Linear Model Intercepts: Proportion of leisure activities in electronic media consumption\nin terms of Degree and Survey Wave, Model 6.2.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Degree - No Degree", caption = "link = identity") +
  bayes_factor_text(bfs_hied)

#Figure 6.2.4
draws %>%
  survey_differences() %>%
  plot_draws(rows = NULL, cols = hied2) +    
  labs(title = "Linear Model Intercepts: Proportion of leisure activities in electronic media consumption\nin terms of Degree and Survey Wave, Model 6.2.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "link = identity") +
  bayes_factor_text(bfs_survey)

#Compute quoted bayes factors
bf_survey_differences(draws, hied2_differences, diff)

model_6_2_2 <- brm(leisure_media | weights(crudewt2) ~ 1 + sex + dagegrp + (1|hied2:survey:in_work),
                    family = gaussian,
                    data = leisure_media_data,
                    prior = c(prior(exponential(3), class = "sd"),
                              prior(normal(0, 2), class = "Intercept"),
                              prior(normal(0, 2), class = "b")),
                    chains = 3, cores = 3, iter = 3000, warmup = 1000,
                    file = "cache/model_6_2_2.rds")

draws <- get_wide_draws(model_6_2_2)

bfs_hied <- draws %>%
  hied2_differences() %>%
  get_bfs(diff, survey, coef, in_work)

bfs_working <- draws %>%
  working_differences() %>%
  get_bfs(diff, hied2, coef, survey)

#Figure 6.2.5
draws %>%
  working_differences() %>%
  plot_draws(survey_on_y = T, rows = hied2, cols = NULL) +    
  labs(title = "Linear Model Intercepts: Proportion of leisure activities in electronic media consumption\nin terms of Degree and Survey Wave, Model 6.2.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Working - Not Working", caption = "link = identity") +
  bayes_factor_text(bfs_working)

#Figure 6.2.6
draws %>%
  hied2_differences() %>%
  plot_draws(survey_on_y = T, rows = in_work, cols = NULL) +    
  labs(title = "Linear Model Intercepts: Proportion of leisure activities in electronic media consumption\nin terms of Degree and Survey Wave, Model 6.2.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Degree - No Degree", caption = "link = identity") +
  bayes_factor_text(bfs_hied)

#model_cluster_link
model_6_2_3 <- brm(leisure_media ~ 1 + dagegrp + sex + dclasuk + (1|cluster),
                   family = gaussian,
                   data = leisure_media_data,
                   prior = c(prior(normal(0, 2), class = "Intercept"),
                             prior(normal(0, 2), class = "b"),
                             prior(exponential(3), class = "sd")),
                   chains = 3, cores = 3, warmup = 1000, iter = 3000,
                   file = "cache/model_6_2_3.rds") 

draws <- gather_draws(model_6_2_3, r_cluster[cluster,]) %>%
  ungroup() %>%
  rename(value = .value) %>%
  left_join(distinct(leisure_data, cluster, cl_desc)) %>%
  mutate(cl_desc = fct_reorder(cl_desc, value))

bfs <- get_bfs(draws, cl_desc)

#Figure 6.5.1
draws %>%
  ggplot(aes(x = value, y = cl_desc)) +
  stat_interval() +
  geom_vline(xintercept = 0) +
  geom_text(data = bfs, aes(label = label), x = -Inf, hjust = - .2, vjust = .5) +
  scale_x_continuous(expand = c(.1, 0, .05, 0)) +
  intervals_colour_scale +
  labs(title = "Linear Model Intercepts: Proportion of Leisure Time in terms of Diary Day Cluster: Model 6.2.3",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Diary Day Cluster", x = "Value") +
  my_theme() +
  theme(plot.title.position = "plot")

##### 6.3 Mental Health and Proportion of Media Leisure ####

#convenience binning function
cut_prop_leisure <- function(x, n) {
  x %>%
    cut(0:n / n, ordered_result = TRUE) %>%
    coalesce("0") %>%
    fct_relevel("0")
}

leisure_mh_data <- leisure_data %>%
  select(crudewt2, dagegrp, dclasuk, sex, mh, wrk_now, 
         leisure_media  = `Watching tv, video, dvd, music`) %>% 
  filter(!is.na(leisure_media), !is.na(mh), !is.na(wrk_now))

#Figure 6.3.1
leisure_mh_data %>%
  mutate(prop_leisure_media = cut_prop_leisure(leisure_media, 5)) %>%
  left_join(count(., prop_leisure_media, name = "n_obs", wt = crudewt2)) %>%
  ggplot(aes(x = mh, weight = crudewt2, fill = n_obs)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_wrap(~prop_leisure_media) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
  scale_fill_stepsn(n.breaks = 3, colours = obs_palette, name = "Number of Observations", show.limits = TRUE) +
  labs(x = "Average of Mental Health Responses (lower is better)",
       y = "Density (weighted)",
       title = "Distribution of subjective mental health responses",
       subtitle = "by proportion of leisure time spent in media consumption") +
  my_theme()

model_6_3_1 <- brm(mh | weights(crudewt2) ~ 1 + wrk_now + sex + dagegrp + dclasuk + leisure_media,
                      family = hurdle_gamma(link = "log", link_shape = "identity", link_hu = "identity"),
                      data = leisure_mh_data,
                      prior = c(prior(normal(0, 2), class = "b"),
                                prior(normal(0, 2), class = "Intercept"),
                                prior(beta(3, 3), class = "hu"),
                                prior(gamma(5, 1), class = "shape")),
                      chains = 3, cores = 3, iter = 3000, warmup = 1000,
                      file = "cache/model_6_3_1.rds")

draws <- gather_draws(model_6_3_1, `b_.*`, regex = T) %>%
  rename(value = .value) %>%
  ungroup() %>%
  filter(str_detect(.variable, "wrk_now|leisure_media")) %>%
  mutate(.variable = fct_recode(.variable,
                                "Proportion of leisure\nin media consumption" = "b_leisure_media",
                                "work_now = part time" = "b_wrk_nowparttime",
                                "work_now = not working" = "b_wrk_nownotworking",
                                "work_now = furlough" = "b_wrk_nowfurlough") %>% fct_rev()) 

bfs <- get_bfs(draws, .variable)

#Figure 6.6.2
draws %>%
  ggplot(aes(x = value, y = .variable)) +
  stat_interval(.width = c(.5, .8, .95)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(expand = c(.2, 0, .1, 0)) +
  intervals_colour_scale +
  labs(title = "Posterior distributions of linear model coefficients predicting mental health in lockdown: Model 6.5.1",
       subtitle = "Showing Bayes Factors for H1, X > 0 against H0, X < 0", x = "Value", y = "Term\nReference: work_now = Full Time", caption = "log link") +
  my_theme() +
  theme(plot.title.position = "plot") +
  bayes_factor_text(bfs)

#estimated effect size
draws %>%
  filter(str_detect(.variable, "Proportion")) %>%
  with(mean(value)) %>%
  exp() #apply link function

#### END ####