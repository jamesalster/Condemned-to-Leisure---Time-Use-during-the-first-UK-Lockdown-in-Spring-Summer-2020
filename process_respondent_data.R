#processes non-diary data for each respondent
#creates long version of main dataset
#makes available tbls respondent_data and activity_long

library(tidyverse)
library(haven)

#Import dataset
data_folder <- "data"

data <- read_dta(file.path(data_folder, "uk_3wave_caddi_data.dta")) %>%
  mutate(diaryid = row_number(), .after = 1L)

#Import coding keys
activity_key <- read_csv("activity_key_final.csv")
cluster_key <- read_rds(file.path(data_folder, "clusters_final.rds"))

#Define useful groups of variables
kids_vars <- quos(nunder5, n5to11, n12to16)
ld_emp_vars <- quos(emplnow, empbfore)
indep_vars <- quos(sex, dagegrp, econstat, dclasuk, hied) 

#Process independent variables
indep_data <- data %>%
  select(mainid, diaryid, survey, crudewt2, dday, starts_with("nq"), -nq5_1,
         !!!indep_vars, !!!ld_emp_vars, !!!kids_vars) %>%
         #create kids indicator
  mutate(n_kids = rowSums(select(., nunder5, n5to11, n12to16), na.rm = T),
         has_kids = if_else(n_kids > 0, "Children", "No Children"),
         #convert haven labelled format
         across(c(survey, dday, !!!indep_vars, !!!ld_emp_vars), as_factor),
         across(c(!!!kids_vars), ~ if_else(. > 0, "Yes", "No")),
         #create mh indicator
         across(starts_with("nq"), ~ . - 1)) %>%
  mutate(mh = rowMeans(select(., starts_with("nq")), na.rm = T)) %>%
  #add in clusters 
  left_join(cluster_key, by = c("diaryid" = "label")) %>%
  #recode some variables
  code_ld_emp() %>%
  mutate(dclasuk = code_dclasuk(dclasuk),
         hied2 = code_hied(hied, n_levels = 2),
         econ_status = code_econ_status(econstat),  
         survey = fct_recode(survey, "2016" = "early 2016"),
         in_work = case_when(survey == "2016" & econ_status == "In Employment" ~ "Working",
                             survey == "2016" & econ_status != "In Employment" ~ "Not Working",
                             survey != "2016" & wrk_now %in% c("full time", "part time") ~ "Working",
                             survey != "2016" & (! wrk_now %in% c("full time", "part time")) ~ "Not Working"),
         workday = if_else(cluster %in% c(1:15, 36), "Workday", "Not Workday"),
         cl_desc = str_c(cluster, ": ", description)) %>%
  #remove unneeded variables
  select(-c(!!!ld_emp_vars, starts_with("nq")))

#### Process activity information ####
act_regex <- "pri[0-9]"

primary_long <- data %>%
  select(diaryid, matches(act_regex)) %>%
  #convert to long, with separate category and slot columns
  pivot_longer(matches(act_regex), names_to = "category_slot", values_to = "pri") %>%
  #extract slot
  mutate(slot = str_extract(category_slot, "[0-9]+"),
         #convert labelled to factor
         pri = as_factor(pri)) %>%
  #code activity
  left_join(activity_key, by = c("pri" = "original_level")) %>%
  mutate(coded5_activity = code5(activity_group))

sec_regex <- "sec[0-9]"

secondary_long <- data %>%
  select(diaryid, matches(sec_regex)) %>%
  #convert to long, with separate category and slot columns
  pivot_longer(matches(sec_regex), names_to = "category_slot", values_to = "sec") %>%
  #extract slot
  mutate(slot = str_extract(category_slot, "[0-9]+"),
         #convert labelled to factor
         sec = as_factor(sec)) %>%
  #code activity
  left_join(activity_key, by = c("sec" = "original_level")) %>%
  select(diaryid, slot, sec_tidied = pri_tidied, sec_group = activity_group)

activity_long <- full_join(primary_long, secondary_long, by = c("diaryid", "slot"))
  
#### Create telework indicator ###
loc_regex <- "loc[0-9]"

telework_indicator <- data %>%
  select(diaryid, matches(loc_regex)) %>%
  pivot_longer(matches(loc_regex), names_to = "category_slot", values_to = "loc") %>%
  mutate(slot = str_extract(category_slot, "[0-9]+"),
         loc = as_factor(loc)) %>%
  left_join(primary_long, by = c("diaryid", "slot")) %>%
  filter(activity_group == "work") %>%
  group_by(diaryid, loc) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(id_cols = diaryid, names_from = loc, values_from = prop, values_fill = 0)  %>%
  transmute(diaryid, telework = if_else(Home > .5, "Home Workday", "Outside Workday")) 

#Create final respondent dataset
respondent_data <- indep_data %>%
  filter(dagegrp != "65+", econstat != "Student") %>% #cut to smaller sample
  mutate(across(c(dclasuk, dagegrp), fct_drop))  %>% #remove student class and 65+ class
  left_join(telework_indicator, by = "diaryid") %>% #add in teleworking indicator
  mutate(teleworkday = case_when(workday == "Not Workday" ~ "Not Workday",
                                 workday == "Workday" ~ telework,
                                 TRUE ~ NA_character_),
         day_type = case_when(workday == "Not Workday" & in_work == "Working" ~ "Not Workday, Working",
                              workday == "Not Workday" & in_work == "Not Working" ~ "Not Workday, Not Working",
                              workday == "Workday" ~ telework,
                              TRUE ~ NA_character_),
         teleworkday = fct_relevel(teleworkday, "Not Workday", "Outside Workday", "Home Workday"),
         day_type = fct_relevel(day_type, "Not Workday, Not Working", "Not Workday, Working", "Outside Workday", "Home Workday"))

#### End ####
            
