
library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

source("global_variables.R")
source("process_respondent_data.R")

#### functions for specific plots ####

awake_plot <- function(tbl, fill, rows, cols, palette, palette_name, xlabel = "Time of Day") {
  fill <- enquo(fill); rows <- enquo(rows); cols <- enquo(cols)
  
  tbl %>%
    ggplot(aes(x = time, y = prop, fill = !!fill, colour = !!fill)) +
    geom_area(position = "identity", alpha = .1, size = 2) +
    #geom_vline(xintercept = plot_date + hours(9), linetype = "dashed") +
    scale_x_datetime(expand = c(0, 0, 0, 0), breaks = scales::breaks_pretty(10), name = xlabel,
                     minor_breaks = scales::breaks_pretty(6), date_labels = "%I") +
    scale_y_continuous(expand = c(0, 0, .1, 0), labels = scales::percent_format(), name  = "Percent (Weighted)") +
    facet_grid(rows = vars(!!rows), cols = vars(!!cols)) +
    scale_fill_manual(values = palette, name = palette_name) +
    scale_colour_manual(values = palette, name = palette_name) +
    my_theme()
}

amount_time_plot <- function(tbl, dep, indep_fill, indep_panel, palette,
                             palette_name = NA, density = T, ylim = NULL, xlabel = NULL,
                             all_panel_label = "All Respondents") {
  dep <- enquo(dep); indep_fill <- enquo(indep_fill); indep_panel <- enquo(indep_panel);
  
  all_days <- tbl %>%
  group_by(!!indep_fill) %>%
  summarise(mean_dep = weighted.mean(!!dep, w = crudewt2) / 6) %>%
  add_column(!!indep_panel := all_panel_label)

  bar_plot <- tbl %>%
    group_by(!!indep_fill, !!indep_panel) %>%
    summarise(mean_dep = weighted.mean(!!dep, w = crudewt2) / 6, .groups = "drop") %>%
    bind_rows(all_days) %>%
    mutate(!!indep_panel := fct_relevel(!!indep_panel, all_panel_label),
           !!indep_panel := suppressWarnings(fct_relevel(!!indep_panel, all_panel_label, "2016", "June 2020"))) %>%
    ggplot(aes(x = !!indep_panel, y = mean_dep, fill = !!indep_fill)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = palette, name = palette_name) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2), name = xlabel) +
    scale_y_continuous(expand = c(0, 0, .1, 0), limits = ylim,
                       oob = scales::oob_keep, name = "Mean Hours (Weighted)") +
    my_theme() 
  
  dens_plot <- tbl %>%
    mutate(dep_hours = !!dep / 6) %>%
    ggplot(aes(x = dep_hours, colour = !!indep_fill)) +
    geom_density(size = 2, adjust = 1.2, aes(weight = crudewt2)) +
    scale_colour_manual(values = palette, name = palette_name) +
    scale_x_continuous(limits = c(4, 15), name = "Hours") +
    scale_y_continuous(expand = c(0, 0, .1, 0), 
                       oob = scales::oob_keep, name = "Density (Weighted)") +
    my_theme() +
    facet_wrap(vars(!!indep_panel))
  
  if (density) bar_plot + dens_plot 
  else bar_plot
}

#### construct indicators ####

#waking up and sleeping times
waking_sleeping <- activity_long %>%
  mutate(sleep_status = case_when((pri_tidied == "Sleeping" | is.na(pri_tidied)) & sec_tidied == "Sleeping" ~ "Sleeping",
                                  pri_tidied == "Sleeping" & is.na(sec_tidied) ~ "Sleeping",
                                  pri_tidied == "Sleeping" & (!is.na(sec_tidied) & sec_tidied != "Sleeping") ~ "Half-Sleeping",
                                  (pri_tidied != "Sleeping" & !is.na(pri_tidied)) & sec_tidied == "Sleeping" ~ "Half-Sleeping",
                                  pri_tidied != "Sleeping" | sec_tidied != "Sleeping" ~ "Not Sleeping",
                                  TRUE ~ NA_character_)) %>%
  group_by(diaryid) %>%
  filter(!any(is.na(sleep_status))) %>%
  summarise(wakeup_time = detect_index(sleep_status, ~ . != "Sleeping", .default = NA),
            sleeping_time = detect_index(sleep_status, ~ . != "Sleeping", .dir = "backward", .default = NA),
            half_sleep_time = sum(sleep_status == "Half-Sleeping", na.rm = T))
  
                                  
#amounts of rest and sleep
rest_and_sleep <- activity_long %>%
  mutate(sleep_time = pri_tidied == "Sleeping" | sec_tidied == "Sleeping",
         rest_time = pri_tidied == "Resting" | sec_tidied == "Resting",
         work_time = pri_tidied == "Paid work including at home" | sec_tidied == "Paid work including at home",
         break_time = pri_tidied == "Work or study break"| sec_tidied == "Work or study break") %>%
  group_by(diaryid) %>%
  summarise(across(ends_with("_time"), ~ sum(., na.rm = T))) 

#create dataset
sleep_data <- respondent_data %>%
  left_join(waking_sleeping, by = "diaryid") %>%
  left_join(rest_and_sleep, by = "diaryid")

sleep_data_to_model <- filter(sleep_data, !is.na(day_type))

#### 7.1 waking up ####

wakeup_proportions <- sleep_data %>%
  filter(!is.na(day_type)) %>% #12 cases
  group_by(survey, day_type, hied2) %>%
  summarise(time = list(1:48),
            prop = list(map_dbl(1:48, ~ sum(crudewt2[wakeup_time < .], na.rm = T) / sum(crudewt2)))) %>%
  unnest(c(time, prop)) %>%
  mutate(time = plot_date + hours(4) + minutes(10 * time)) 

#Figure 7.1.1
awake_plot(wakeup_proportions, fill = survey, rows = hied2,
            cols = day_type, palette = survey_palette, palette_name = "Survey Wave") +
  geom_vline(xintercept = plot_date + hours(9), linetype = "dashed") +
  labs(title = "Cumulative percentage of respondents who had woken up,\nby Survey Wave, Day Type, and Degree")

#model 7.1.1
model_7_1_1 <- brm(bf(wakeup_time | weights(crudewt2) ~ 1 + sex + dagegrp + (1|survey:day_type:hied2)),
                    family = student,
                    data = sleep_data_to_model,
                    prior = c(prior(normal(20, 10), class = "Intercept"), #unstandardised
                              prior(normal(0, 10), class = "b"),
                              prior(gamma(5, 1), class = "nu"),
                              prior(exponential(1), class = "sigma"),
                              prior(exponential(1), class = "sd")),
                    chains = 3, cores = 3, warmup = 1000, iter = 3000,
                    file = "cache/model_7_1_1.rds")

draws <- get_wide_draws(model_7_1_1)

bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, hied2, day_type)

#Figure 7.1.2
draws %>%
  survey_differences() %>%
  plot_draws(rows = hied2, cols = day_type) +
  scale_x_continuous(expand = c(.5, 0, .1, 0)) +
  labs(title = "Linear Model Intercepts: Wakeup Time by Degree, Survey Wave and Day Type, Model 7.1.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "Difference", x = "Value") +
  bayes_factor_text(bfs)

#Figure 7.1.3
wakeup_proportions %>%
  mutate(day_type = fct_rev(day_type)) %>%
  awake_plot(fill = day_type, rows = hied2,
            cols = survey, daytype_palette, palette_name = "Day Type") +
  geom_vline(xintercept = plot_date + hours(9), linetype = "dashed") +
  labs(title = "Cumulative percentage of respondents who had woken up,\nby Day Type, Survey, and Degree")

#Figure 7.1.4
wakeup_proportions %>%
  awake_plot(fill = hied2, rows = survey,
            cols = day_type, palette = hied2_palette, palette_name = "Degree") + 
  geom_vline(xintercept = plot_date + hours(9), linetype = "dashed") +
  labs(title = "Cumulative percentage of respondents who had woken up,\nby Degree, Survey, and Day Type")

#calculate bayes factors discussed in section 7\.1
draws %>%
  hied2_differences() %>%
  get_bfs(diff, survey, day_type) %>%
  mutate(inverse = 1 / bf)

#and survey differences
bf_survey_differences(draws, hied2_differences, diff, day_type)

#Figure 7.1.5
wakeup_activity <- primary_long %>%
  full_join(waking_sleeping) %>%
  filter(!is.na(wakeup_time)) %>%
  group_by(diaryid) %>%
  filter(slot %in% unique(wakeup_time):(unique(wakeup_time) + 12)) %>%
  group_by(diaryid, coded5_activity) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  pivot_wider(names_from = coded5_activity, values_from = prop, id_cols = diaryid, values_fill = 0)

coded5_palette[[1]] <- "black"
wakeup_activity %>%
  right_join(respondent_data) %>%
  left_join(waking_sleeping) %>%
  mutate(wakeup_time = (wakeup_time / 6) + 4,
         wakeup_bin = cut(wakeup_time, breaks = c(4, 6, 8, 10, 12),#breaks = c(5, 6, 7, 8, 9, 10, 11),
                          include_lowest = TRUE)) %>%
  filter(!is.na(in_work), !is.na(wakeup_bin)) %>%
  group_by(survey, wakeup_bin, workday, hied2) %>%
  summarise(across(rest:leisure, ~ weighted.mean(., w = crudewt2)), .groups = "drop") %>%
  pivot_longer(rest:leisure) %>%
  ggplot(aes(x = wakeup_bin, colour = name, y = value, group = name)) +
  geom_point() +
  geom_line() +
  facet_grid(survey ~ workday + hied2) +
  #facet_grid(survey~hied2 + in_work) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_colour_manual(values = coded5_palette, name = "Activity Group") +
  labs(title = "Primary activity in the two hours following waking up, by Degree, Work Status and Survey Wave",
       y = "Average Percentage (Weighted)", x = "Wakeup Time Bracket") +
  my_theme()


#### 7.2 sleeping times ####

sleep_proportions <- sleep_data_to_model %>%
  group_by(survey, day_type) %>%
  summarise(time = list(1:144),
            prop = list(map_dbl(1:144, ~ sum(crudewt2[sleeping_time > .], na.rm = T) / sum(crudewt2)))) %>%
  unnest(c(time, prop)) %>%
  ungroup() %>%
  filter(time > (144 - 60)) %>%
  mutate(time = plot_date + hours(4) + minutes(10 * time)) 

#Figure 7.2.1
awake_plot(sleep_proportions, fill = day_type, rows = NULL,
            cols = survey, palette = daytype_palette, palette_name = "Survey Wave",
           xlabel = "Time of Day (7 PM to 4 AM)") +
  labs(title = "Cumulative percentage of respondents awake, by Survey Wave and Workday Type") 

#Figure 7.2.2
awake_plot(sleep_proportions, fill = survey, rows = NULL,
            cols = day_type, palette = survey_palette, palette_name = "Survey Wave",
           xlabel = "Time of Day (7 PM to 4 AM)") +
  labs(title = "Cumulative percentage of respondents awake, by Survey Wave and Workday Type") 

model_7_2_1 <- brm(bf(sleeping_time | weights(crudewt2) ~ 1 + dagegrp + sex + dclasuk + has_kids + (1|survey:day_type),
                    sigma ~ 1 + dagegrp + sex + dclasuk + has_kids + (1|survey:day_type)),
                   family = student,
                   data = sleep_data_to_model,
                   prior = c(prior(normal(0, 10), class = "Intercept"),
                             prior(normal(0, 2), class = "b"),
                             prior(exponential(3), class = "sd"),
                             prior(gamma(5, 1), class = "nu"),
                              prior(normal(0, 10), class = "Intercept", dpar = "sigma"),
                             prior(normal(0, 2), class = "b", dpar = "sigma"),
                             prior(exponential(3), class = "sd", dpar = "sigma")),
                   chains = 3, cores = 3, warmup = 1000, iter = 3000,
                   file = "cache/model_7_2_1.rds")

draws <- get_wide_draws(model_7_2_1)

bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, day_type, coef)

#Figure 7.2.3
draws %>%
  survey_differences() %>%
  plot_draws(cols = coef, rows = day_type) +
  facet_grid(day_type~coef, scales = "free_x", labeller = label_wrap_gen(12)) +
  bayes_factor_text(bfs) +
  labs(title = "Linear Model Intercepts: Sleeping Time by Survey Wave and Day Type, Model 7.2.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "links: mu = identity; sigma = log") 

#### 7.3 amount of sleep ####

#Figure 7.3.1
amount_time_plot(sleep_data_to_model, sleep_time, survey, day_type, 
                 survey_palette, ylim = c(5, 10), palette_name = "Survey Wave",
                 xlabel = "Day Type") +
  plot_annotation(title = "Time Spent Sleeping, by Day Type and Survey Wave")


model_7_3_1 <- brm(bf(sleep_time | weights(crudewt2) ~ 1 + dagegrp + dclasuk + sex + (1|survey),
                  sigma ~ 1 + dagegrp + dclasuk + sex + (1|survey)),
                  family = student,
                  data = sleep_data_to_model,
                  prior = c(prior(normal(0, 10), class = "Intercept"),
                            prior(normal(0, 5), class = "b"),
                            prior(exponential(3), class = "sd"),
                            prior(normal(0, 10), class = "Intercept", dpar = "sigma"),
                            prior(normal(0, 5), class = "b", dpar = "sigma"),
                            prior(exponential(3), class = "sd", dpar = "sigma")),
                  chains = 3, cores = 3, warmup = 1000, iter = 3000,
                  file = "cache/model_7_3_1.rds")

draws <- get_wide_draws(model_7_3_1)

bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, coef)

#Figure 7.3.2
draws %>%
  survey_differences() %>%
  plot_draws(cols = coef, rows = NULL) +
  bayes_factor_text(bfs) +
  labs(title = "Linear Model Intercepts: Time Asleep by Survey Wave, Model 7.3.1",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "links: mu = identity; sigma = log") 

model_7_3_2 <- update(model_7_3_1, 
                      formula = bf(sleep_time | weights(crudewt2) ~ 1 + dagegrp + sex + dclasuk + (1|day_type:survey),
                                   sigma ~ 1 + dagegrp + sex + dclasuk + (1|day_type:survey)),
                      newdata = sleep_data_to_model,
                      chains = 3, cores = 3, warmup = 1000, iter = 3000,
                      file = "cache/model_7_3_2.rds")

draws <- get_wide_draws(model_7_3_2)

bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, coef, day_type)

#Figure 7.3.3
draws %>%
  survey_differences() %>%
  plot_draws(cols = coef, rows = day_type)  +
  facet_grid(day_type~coef, scales = "free_x", labeller = label_wrap_gen(12)) +
  bayes_factor_text(bfs) +
    labs(title = "Linear Model Intercepts: Time Asleep by Survey Wave and Day Type, Model 7.3.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "links: mu = identity; sigma = log")

bfs <- draws %>%
  day_type_differences() %>%
  get_bfs(diff, coef, survey)

#Figure 7.3.4
draws %>%
  day_type_differences() %>%
  plot_draws(cols = coef, rows = survey)   +
  bayes_factor_text(bfs) +
    labs(title = "Linear Model Intercepts: Time Asleep by Survey Wave and Day Type, Model 7.3.2",
       subtitle = "Showing Bayes Factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference", caption = "links: mu = identity; sigma = log")

#compute quoted bayes factors
bf_survey_differences(draws, day_type_differences, diff, coef)

#### END ###