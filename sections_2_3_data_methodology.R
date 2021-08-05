
library(tidyverse)
library(haven)
library(janitor)

#### Load Data ####

data_folder <- "data"

data <- read_dta(file.path(data_folder, "uk_3wave_caddi_data.dta")) %>%
  mutate(diaryid = row_number(), .after = 1L)

#Import global functions
source("global_variables.R")

#### Transform Data ####

weights_data <- data %>%
  select(mainid, crudewt2, survey, dday, dagegrp, econstat) %>%
  mutate(across(where(is.labelled), as_factor),
         in_sample = if_else((dagegrp != "65+") & (econstat != "Student"),
                             "Yes", "No", "No"), #NAs are also excluded from sample
         in_sample = fct_relevel(in_sample, "Yes", "No"))

demog_data <- data %>%
  select(mainid, crudewt2, survey,  dagegrp, econstat, sex, dclasuk, hied, emplnow, empbfore) %>%
  code_ld_emp() %>%
  mutate(across(where(is.labelled), as_factor),
         survey = str_remove(survey, "early "),
         survey = fct_relevel(survey, "2016", "June 2020", "August 2020"),
         dclasuk = code_dclasuk(dclasuk),
         hied = code_hied(hied, 2),
         in_sample = if_else((dagegrp != "65+") & (econstat != "Student"),
                             "Yes", "No", "No"), #NAs are also excluded from sample
         in_sample = fct_relevel(in_sample, "Yes", "No")) 

#### Some figures cited in text ####

#### Dates of survey waves ####

data %>%
  count(ddomonth, dmonth, dyear, survey) %>%
  distinct() %>%
  arrange(dyear, dmonth, ddomonth) %>%
  print(n = Inf)

data %>%
  count(dmonth, dyear) %>%
  arrange(dyear, dmonth)

#2583 is sample size, 30 because of nas, 419 because not available
count(weights_data, in_sample)

#means of weights
mean(weights_data$crudewt2, na.rm = T)

weights_data %>%
  filter(in_sample == "Yes") %>%
  with(mean(crudewt2, na.rm = T))

#N of weights over 4
sum(weights_data$crudewt2 > 4, na.rm = T)

weights_data %>%
  filter(in_sample == "Yes") %>%
  with(sum(crudewt2 > 4, na.rm = T))

#n of written in responses
all_activity_codes <- data %>%
  select(matches("pri[0-9]")) %>%
  as.matrix() %>%
  c()

sum(all_activity_codes == 138, na.rm = T) / length(all_activity_codes) * 100

#Number with no qualifications
data %>%
  mutate(hied = as_factor(hied)) %>%
  with(table(hied))

#### Table 2.1.1: survey numbers by wave ####

respondents <- weights_data %>%
  count(survey, mainid, name = "n_diaries") %>%
  count(survey, n_diaries) %>%
  pivot_wider(names_from = survey, values_from = n, values_fill = 0) 

diaries <- weights_data %>%
  count(survey) %>%
  pivot_wider(names_from = survey, values_from = n) %>%
  add_column(n_diaries = "Total diaries")

respondents %>%
  adorn_totals('row', name = "Total respondents") %>%
  bind_rows(diaries) %>%
  print() %>%
  write_rds("table_2_1.rds")
  
#### Figure 2.2.1: weekday correction by weight ####

weights_data %>%
  group_by(survey, dday, in_sample) %>%
  summarise(n = n(),
            n_weighted = sum(crudewt2, na.rm = T)) %>%
  pivot_longer(c(n, n_weighted), names_to = "weighted", values_to = "value") %>%
  ggplot(aes(x = dday, y = value, fill = in_sample)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    facet_grid(weighted~survey) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
    facet_grid(weighted~survey) +
    scale_fill_manual(values = sample_palette, name = "In Sample ?") +
    labs(title = "Effect of weighting (by crudewt2) on diary day",
         x = "Weekday", y = "Number of diaries") +
    my_theme()
  
#### Figure 2.2.2: age correction by weight ####

weights_data %>%
  group_by(survey, dagegrp, in_sample) %>%
  summarise(n = n(),
            n_weighted = sum(crudewt2, na.rm = T)) %>%
  pivot_longer(c(n, n_weighted), names_to = "weighted", values_to = "value") %>%
  ggplot(aes(x = dagegrp, y = value, fill = in_sample)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    facet_grid(weighted~survey) +
    scale_fill_manual(values = sample_palette, name = "In Sample ?") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
    labs(title = "Effect of weighting (by crudewt2) on distribtion of age groups",
         x = "Age Group", y = "Number of diaries") +
    my_theme()
  
#### Work change ####

#change in economic status into lockdown
work_change <- function(demog_data, var) {
  demog_data %>%
    filter(in_sample == "Yes",
           wrk_change != "not in work before lockdown") %>%
    group_by(survey, !!var, wrk_change) %>%
    summarise(n = sum(crudewt2), .groups = "drop_last") %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup() %>%
    rename(category = !!var)
}

changes <- tibble(var = quos(sex, dagegrp, hied, dclasuk)) %>% 
  mutate(props = map(var, ~ work_change(demog_data, .))) %>%
  unnest(props) %>%
  bind_rows(mutate(work_change(demog_data, NULL), category = factor("All")), .)

#Figure 2.3.1
changes %>%
  filter(survey != "2016",
         wrk_change %in% c("full to part time", "part to full time", "furlough", "not working"),
         !str_detect(category, "Not working")) %>%
  mutate(category = fct_rev(category),
         category = fct_recode(category, "Higher managerial\nand professional" = "Higher managerial and professional"),
         wrk_change = if_else(wrk_change == "not working",  "no longer working", wrk_change),
         wrk_change = fct_relevel(wrk_change, "part to full time")) %>%
  ggplot(aes(x = proportion, y = category, fill = wrk_change)) +
  geom_col() +
  facet_grid(~survey) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), 
                     limits = c(0, .5), oob = scales::oob_censor,
                     expand = c(0, 0, 0.1, 0)) +
  scale_fill_manual(values = wrk_change_palette, name = "Change in Work Status") +
  labs(title = "Change in work status of respondents in June and August 2020 waves, over lockdown", 
       subtitle = "as percentage of those who were working before lockdown",
       x = "Percentage of sample", y = NULL) +
  my_theme() +
  theme(plot.title.position = "plot")

#### Cronbach Alpha of Mental Health indicator ####

cronbach_alpha <- function(tbl) {
  n_var <- ncol(tbl)
  covariances <- cov(tbl, use = "pairwise")
  avg_var <- mean(diag(covariances)) 
  avg_covar <- mean(covariances[upper.tri(covariances)]) 
  
  alpha <- (n_var * avg_covar) / (avg_var + (n_var - 1) * avg_covar)
  alpha
}

data %>%
  distinct(mainid, .keep_all = TRUE) %>%
  select(starts_with("nq"), -nq5_1) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(if_all(everything(), ~!is.na(.))) %>%
  cronbach_alpha()

#### End #### 

  
