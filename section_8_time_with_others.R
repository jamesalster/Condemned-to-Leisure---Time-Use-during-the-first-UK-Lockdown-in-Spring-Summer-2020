#Section 8, Time with Other Household Members

library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

source("global_variables.R")
source("process_respondent_data.R")

#Function for pretty plotting
recode_measure <- function(vec) {
  fct_recode(vec,
             "At least one" = "at_least_one",
             "At least two" = "at_least_two",
             "At least three" = "at_least_three",
             "Person-Hours" = "person_hours") %>%
    fct_relevel("At least one", "At least two")
}

#### Import dataset ####
data_folder <- "data"

data <- read_dta(file.path(data_folder, "uk_3wave_caddi_data.dta")) %>%
  mutate(diaryid = row_number(), .after = 1L)

#### Process data, creating indicators ####
who_regex <- "who[abcd0-9]{2,4}"

who_long <- data %>%
  select(diaryid, matches(who_regex)) %>%
  #convert to long, with separate category and slot columns
  pivot_longer(matches(who_regex), names_to = "category_slot", values_to = "who_value") %>%
  mutate(who_value = as_factor(who_value),
         who_index = str_extract(category_slot, "who[abcd]"),
         slot = str_extract(category_slot, "[0-9]+"))

#number of NAs
who_long %>%
  filter(who_index == "whoa",
         is.na(who_value)) %>%
  distinct(diaryid)

who_household <- who_long %>%
  mutate(with_household = !str_detect(who_value, "(not from)|(other household)|Alone"),
         with_not_household = str_detect(who_value, "(not from)|(other household)")) %>%
  group_by(diaryid, slot) %>% #get per-slot number of household members
  summarise(across(starts_with("with_"), ~ sum(., na.rm = T)), .groups = "drop_last") %>%
  summarise(across(starts_with("with_"), list(Xat_least_three = ~ sum(. >= 3),
                                              Xat_least_two = ~ sum(. >= 2),
                                              Xat_least_one = ~ sum(. >= 1),
                                              Xperson_hours = ~ sum(.)))) %>%
  right_join(respondent_data, by = "diaryid") %>%
  pivot_longer(starts_with("with_"), names_to = "measure", values_to = "value") %>%
  separate(measure, into = c("household", "measure"), sep = "_X")

#### Analysis ####

who_household_to_model <- who_household %>%
  filter(household == "with_household") %>%
  pivot_wider(names_from = measure, values_from = value) 

who_household_to_model %>%
  group_by(dclasuk, survey, has_kids) %>%
  summarise(across(c(starts_with("at_least"), person_hours), 
                   ~ weighted.mean(., w = crudewt2) / 6 ),
            .groups = "drop") %>% #divide by 6 to convert slots to hours
  pivot_longer(c(starts_with("at_least"), person_hours), names_to = "measure", values_to = "value") %>%
  mutate(measure = recode_measure(measure)) %>%
  ggplot(aes(x = survey, colour = dclasuk, y = value, group = dclasuk)) +
  geom_point(size = 3) +
  geom_line(size = 1.2, linemitre = 2) +
  scale_colour_manual(values = class_palette, name = "Class") +
  facet_grid(measure ~ has_kids, scales = "free_y") +
  labs(title = "Mean hours per day spent with other household members, by Class, Survey Wave and Children",
       x = "Survey Wave", y = "Average hours per day (weighted)") +
  my_theme()

#distribution
model_8_1_1 <- brm(at_least_two | weights(crudewt2) ~ 1 + dagegrp + sex + (1|survey:dclasuk:has_kids),
                   data = who_household_to_model,
                   family = gaussian,
                   prior = c(prior(normal(0, 1), class = "Intercept"),
                             prior(normal(0, 1), class = "b"),
                             prior(gamma(3, 1), class = "sd")),
                   chains = 3, cores = 3, warmup = 1000, iter = 3000,
                   file = "cache/model_8_1_1.rds")

#There we go
draws <- get_wide_draws(model_8_1_1)
  
bfs <- draws %>%
  survey_differences() %>%
  get_bfs(diff, dclasuk, has_kids)

#Figure 8.1.2
draws %>%
  survey_differences() %>%
  plot_by_survey(multi_diff = T, rows = has_kids, cols = dclasuk, scales = "fixed") +
  scale_x_continuous(expand = c(.4, 0, .1, 0)) +
  labs(title = "Differences between Linear Model Intercepts: Model 8.1.1\nHours with at least two other household members, in terms of Class, Children and Survey",
       y = "", x = "Difference") +
  bayes_factor_text(bfs)

bfs <- draws %>%
  class_differences() %>%
  get_bfs(diff, survey, has_kids)

#Figure 8.1.3
draws %>%
  class_differences() %>%
  plot_by_survey(multi_diff = T, rows = has_kids, cols = survey, scales = "fixed") +
  scale_x_continuous(expand = c(.5, 0, .1, 0)) +
  labs(title = "Differences between Linear Model Intercepts: Model 8.1.1\nHours with at least two other household members, in terms of Class, Children and Survey",
       y = "", x = "Difference") +
  bayes_factor_text(bfs)

model_8_1_2 <- brm(at_least_two | weights(crudewt2) ~ 1 + dagegrp + sex + (1|survey:dclasuk:has_kids:in_work),
                   data = who_household_to_model,
                   family = gaussian,
                   prior = c(prior(normal(0, 1), class = "Intercept"),
                             prior(normal(0, 1), class = "b"),
                             prior(gamma(3, 1), class = "sd")),
                   chains = 3, cores = 3, warmup = 1000, iter = 3000,
                   file = "cache/model_8_1_2.rds")

draws <- get_wide_draws(model_8_1_2)

bfs_class <- draws %>%
  class_differences() %>%
  get_bfs(diff, survey, has_kids, in_work)

bfs_working <- draws %>%
  working_differences() %>%
  get_bfs(diff, survey, has_kids, dclasuk)

#Figure 8.1.4
draws %>%
  working_differences() %>%
  plot_by_survey(rows = has_kids, cols = dclasuk, scales = "fixed") +
  scale_x_continuous(expand = c(.4, 0, .1, 0)) +
  labs(title = "Differences between Linear Model Intercepts: Model 8.1.2\nHours with at least two other household members, in terms of Class, Children, In Work and Survey",
       subtitle = "Showing Bayes factors for H1, X > 0, against H0, X < 0",
        y = "", x = "Working - Not Working") +
    bayes_factor_text(bfs_working)

model_8_1_3 <- update(model_8_1_1,
                      at_least_two | weights(crudewt2) ~ 1 + dagegrp + (1|sex:has_kids:survey:dclasuk),
                      newdata = who_household_to_model,
                      chains = 3, cores = 3, warmup = 1000, iter = 3000,
                      file = "cache/model_8_1_3.rds")

draws <- get_wide_draws(model_8_1_3)
draws_tidied <- draws %>%
  filter(has_kids == "Children") %>%
  mutate(sex = str_c(sex, ", with Children")) 

bfs <- draws_tidied %>%
  survey_differences() %>%
  get_bfs(diff, sex, dclasuk, has_kids) 

#Figure 8.1.5
draws_tidied %>%
  survey_differences() %>%
  plot_draws(cols = dclasuk, rows = sex) +
  scale_x_continuous(expand = c(.5, 0, .1, 0)) +
  labs(title = "Differences between Linear Model Intercepts: Model 8.1.3\nHours with at least two other household members, in terms of Class, Male - Female, Children and Survey",
       subtitle = "Showing Bayes factors for H1, X > 0, against H0, X < 0", 
       y = "", x = "Difference",
       caption = "Values for respondents without children all overlap 0 at 95% and are not shown.") +
  bayes_factor_text(bfs)

#Show quoted Bayes factors
#between surveys
bf_survey_differences(draws, class_differences, diff, has_kids, in_work)

#Higher - Routine, June 2020 and August 2020, Working vs Not Working
draws %>%
  filter(dclasuk == "Routine") %>%
  pivot_wider(names_from = in_work, values_from = value) %>%
  group_by(survey, has_kids) %>%
  summarise(bf_Working_Not_Working = sum(`Working` > `Not Working`) / sum(`Not Working` > Working))

#### END ####
