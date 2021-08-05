
library(tidyverse)
library(lubridate)
library(tidybayes)
library(ggdist)

#### Graph theme ####

my_theme <- function() {
  list(theme_bw(),
       theme(strip.text = element_text(size = 12, face = "bold"),
             legend.position = "bottom"))
}

#centralised date for time plots
plot_date <- dmy("01-01-2020")

#function to convert slot to date-time
slot_to_date <- function(slot_vec) {
  slot_number <- as.numeric(str_extract(slot_vec, "[0-9]+"))
  plot_date + hours(4) + minutes(10) * (slot_number - 1)
}

#Order for the 5-way activity coding
coded5_order <- rev(c("work", "unpaid_work", "other", "leisure", "rest"))

#Palettes
coded5_palette <- c("#FEF4D5", "#DB4743", "#F5AF4D", "#7C873E", "#5495CF") %>%
  setNames(coded5_order)
survey_palette <- c("2016" = "#333544FF",  "June 2020" = "#B50A2AFF", "August 2020" = "#0E84B4FF")
class_palette <- c("#4C413FFF", "#278B9AFF", "#E75B64FF", "#D8AF39FF")
kids_palette <- c("No Children" = "#5A6F80FF", "Children" = "#E75B64FF")
hied2_palette <- c("Degree" = "#446590FF", "No Degree" = "#BBA78CFF")
daytype_palette <- c("#DCCA2CFF", "#6FB382FF", "#132E41FF", "#4D6D93FF")
in_out_palette <- c("#F0D77BFF", "#5C5992FF")
obs_palette <- c("#AE93BEFF", "#5C5992FF", "#403369FF")
sample_palette <- c("No" = "#5E2D30FF", "Yes" = "#008E90FF")
mf_palette <- c("#657060FF", "#E48C2AFF")
wrk_change_palette <- c("#C3AF97FF", "#8F8093FF", "#833437FF", "#353831FF")
w95_palette <- c("#353831FF", "#B7D9F2FF")
cluster_group_palette <- c("#0F56BE", "#77BEC5", "#4F876C", "#7452A6", "#8105C4",
                           "#45167F", "#621124", "#D8391D", "#F79124", "#F6DB23")

#Colour scale for probability intervals
intervals_colour_scale  <- scale_colour_manual(values = c("#E48C2AFF", "#CD4F38FF", "#3D4F7DFF"),
                                               name = "Posterior probability interval (mean)") 

#### Functions to code variables ####

#Economic Status
code_econ_status <- function(vec) {
  short_econ <- str_extract(vec, "Self-employed|Employed|owner|Student|Casual|Homemaker|Retired|Unemployed")
  shorter_econ <- case_when(short_econ %in% c("owner", "Self-employed", "Employed") ~ "In Employment",
              short_econ %in% c("Causal", "Unemployed") ~ "Unemployed",
              short_econ %in% c("Student", "Homemaker", "Retired") ~ "Inactive")
  fct_relevel(shorter_econ, "In Employment", "Unemployed", "Inactive")
}

#Lockdown Employment Status
code_ld_emp <- function(tbl) {
  mutate(tbl, 
         across(c(emplnow, empbfore), as_factor),
         wrk_before = str_extract(empbfore, "full time|part time|wasn't working"),
         wrk_now = str_extract(emplnow, "full time|part time|not working|furlough"),
         wrk_before = if_else(wrk_before == "wasn't working", "not working", wrk_before),
         wrk_change = case_when(wrk_before == "full time" & wrk_now == "part time" ~ "full to part time",
                                wrk_before == "part time" & wrk_now == "full time" ~ "part to full time",
                                wrk_before %in% c("full time", "part time") ~ wrk_now,
                                wrk_before == "not working" ~ "not in work before lockdown",
                                is.na(wrk_before) | is.na(wrk_now) ~ NA_character_,
                                TRUE ~ "Other"),
         .keep = "unused")
}

#Class
code_dclasuk <- function(vec) {
  fct_recode(vec, "Higher managerial and professional" = "Higher managerial, administrative and professional occupatio",
              "Intermediate" = "Intermediate occupations",
              "Routine" = "Routine and manual occupations",
              "Not working or unemployed" = "Not working, never worked and long-term unemployed",
             "Student" = "STUDENT")
}

#Qualification
code_hied <- function(vec, n_levels = c(2, 4)) {
  four_levels <- str_extract(vec, "Higher degree|First degree|f|GCSE|Apprenticeship|No qualifications") %>%
    fct_collapse("A Levels" = "f",
                 "Apprenticeship, GCSE or None" = c("Apprenticeship", "GCSE", "No qualifications")) %>%
    fct_relevel("Higher degree", "First degree", "A Levels", "Apprenticeship, GCSE or None")
  if (n_levels == 2) {
    fct_collapse(four_levels, "Degree" = c("Higher degree", "First degree"),
                 "No Degree" = c("A Levels", "Apprenticeship, GCSE or None"))
  } else four_levels
}

#Activity Grouping
code5 <- function(act_group) {
  case_when(act_group == "work" ~ "work",
            str_detect(act_group, "^unpaid") ~ "unpaid_work",
            str_detect(act_group, "^leisure") ~ "leisure",
            str_detect(act_group, "^rest") ~ "rest",
            is.na(act_group) ~ NA_character_, 
            TRUE ~ "other") 
}


#### Processing brms model output ####

#Getting draws for group-level effects
get_wide_draws <- function(model, resp_levels = "", cluster = FALSE) {
  
  long_draws <- tidy_draws(model) %>%
    select(.draw, starts_with("r_")) %>%
    pivot_longer(-c(.draw))
  
  #Cluster can interfere with other regexes so search explicitly
  if (cluster) long_draws <- mutate(long_draws, cluster = str_extract(name, "[0-9]{1,2}(?=[^0-9])"))
  
  long_draws %>% 
    #extract variable values by variable name, using string regex
    mutate(resp = str_extract(name, str_c(resp_levels, collapse = "|")),
           coef = coalesce(str_extract(name, "hu|zi|zoi|coi|sigma|shape|nu"), "mu"),
           survey = str_extract(name, "2016|June.2020|August.2020"),
           sex = str_extract(name, "Male|Female"),
           dclasuk = str_extract(name, "Higher|Intermediate|Routine|unemployed"),
           hied2 = str_extract(name, "(No.)?Degree"),
           in_work = str_extract(name, "(Not.)?Working"),
           has_kids = str_extract(name, "(No.)?Children"),
           day_type = str_extract(name, "(Not.Workday\\,.Working)|(Not.Workday\\,.Not.Working)|Home.Workday|Outside.Workday"),
           workday95 = str_extract(name, "Other.Workday|Workday.9.to.5"),
           #change for pretty printing
           across(where(is.character), ~ str_replace_all(., "\\.", " ")),
           dclasuk = suppressWarnings(fct_recode(dclasuk, "Not Working" = "unemployed") %>%
                                        fct_relevel("Not Working", after = Inf)),
           survey = suppressWarnings(fct_relevel(survey, "2016", "June 2020", "August 2020")),
           day_type = suppressWarnings(fct_relevel(day_type, "Not Workday, Not Working",
                                                   "Not Workday, Working", "Home Workday",
                                                   "Outside Workday"))) %>%
    select(where(~ !any(is.na(.))), -name) #remove all NA columns
}

## Functions to calculate differences between levels ##   
sex_differences <- function(draws) {
  differences <- draws %>%
    pivot_wider(names_from = sex, values_from = value) %>%
    mutate(Female - Male, .keep = "unused") %>%
    pivot_longer(contains(" - "), names_to = "diff")
}

class_differences <- function(draws) {
  differences <- draws %>%
    pivot_wider(names_from = dclasuk, values_from = value) %>%
    mutate(`Higher` - `Intermediate`,
           `Higher` - `Routine`,
           `Intermediate` - `Routine`,
           `Routine` - `Not Working`, .keep = "unused") %>%
    pivot_longer(contains(" - "), names_to = "diff")
}

survey_differences <- function(draws) {
  draws %>%
    pivot_wider(names_from = survey, values_from = value) %>%
    mutate(`June 2020` - `2016`,
           `August 2020` - `June 2020`,
           `August 2020` - `2016`, .keep = "unused") %>%
    pivot_longer(contains(" - "), names_to = "diff") %>%
    mutate(diff = fct_rev(diff))
}

day_type_differences <- function(draws) {
  draws %>%
    pivot_wider(names_from = day_type, values_from = value) %>%
    mutate(`Outside Workday` - `Not Workday, Working`,
           `Outside Workday`- `Home Workday`,
           `Home Workday` - `Not Workday, Working`,
           `Not Workday, Working` - `Not Workday, Not Working`,
            .keep = "unused") %>%
    pivot_longer(contains(" - "), names_to = "diff")
}  

kids_differences <- function(draws) {
  differences <- draws %>%
    pivot_wider(names_from = has_kids, values_from = value) %>%
    mutate(`Children` - `No Children`, .keep = "unused") %>%
    pivot_longer(contains(" - "), names_to = "diff")
}  

hied2_differences <- function(draws, categ = FALSE) {
  draws %>%
    pivot_wider(names_from = hied2, values_from = value) %>%
    mutate(Degree - `No Degree`, 
           survey = fct_rev(survey), .keep = "unused")  %>%
    pivot_longer(contains(" - "), names_to = "diff")
}

working_differences <- function(draws, categ = FALSE) {
  draws %>%
    pivot_wider(names_from = in_work, values_from = value) %>%
    mutate(Working - `Not Working`,
           survey = fct_rev(survey), .keep = "unused")  %>%
    pivot_longer(contains(" - "), names_to = "diff")
}

## Functions to calculate Bayes factors from draws ##
get_bfs <- function(draws, ...) {
  draws %>% 
    group_by(...) %>%
    summarise(bf = sum(value > 0) / sum(value < 0), .groups = "drop") %>%
    mutate(label = case_when(bf > 100 ~ "> 100",
                             bf < 0.01 ~ "< 0.01",
                             TRUE ~ as.character(signif(bf, 2))))
}

## Get difference of difference across surveys ##
bf_survey_differences <- function(draws, difference_function, ...) {
  draws %>%
    difference_function() %>%
    pivot_wider(names_from = survey, values_from = value) %>%
    group_by(...) %>%
    summarise(bf_june_2016 = sum(`June 2020` > `2016`) / sum(`June 2020` < `2016`),
              bf_august_june = sum(`August 2020` > `June 2020`) / sum(`August 2020` < `June 2020`),
              bf_august_2016 = sum(`August 2020` > `2016`) / sum(`August 2020` < `2016`))
}


#### Functions for commonly used plots ####



## Model Output plots ##

#Plot model draws
plot_draws <- function(differences, rows = NULL, cols = coef,
                           scales = "free_x", survey_on_y = FALSE,
                       expand = c(.1, 0, .1, 0)) { 
  rows <- enquo(rows); cols <- enquo(cols)
  
  data <- mutate(differences, diff = fct_rev(diff))
  
  if (survey_on_y) plot <- ggplot(data, aes(y = survey, x = value)) 
  else plot <- ggplot(data, aes(y = diff, x = value))
  
  plot + 
    stat_interval(.width = c(.5, .8, .95)) +
    intervals_colour_scale +
    geom_vline(xintercept = 0) +
    scale_y_discrete(expand = expand) +
    scale_x_continuous(expand = c(.25, 0, .1, 0)) +
    facet_grid(rows = vars(!!rows), cols = vars(!!cols), scales = scales) +
    my_theme() +
    theme(plot.title.position = "plot")
}

#Categorical version, with response levels on y axis
plot_categ_draws <- function(differences, formula) {
  ggplot(differences, aes(y = resp, x = value)) +
    geom_vline(xintercept = 0) +
    my_theme() +
    stat_interval(.width = c(.5, .8, .95)) + 
    intervals_colour_scale +
    scale_x_continuous(expand = c(.3, 0, .1, 0)) +
    facet_grid(formula) +
    theme(plot.title.position = "plot")
}


#Add Bayes Factors to output
bayes_factor_text <- function(results_table,  x = -Inf, vjust = .5) { #vjust = 1.8, x = 0) {
  list(geom_text(data = results_table, aes(label = label), x = x, hjust = -.2, vjust = vjust)) 
}