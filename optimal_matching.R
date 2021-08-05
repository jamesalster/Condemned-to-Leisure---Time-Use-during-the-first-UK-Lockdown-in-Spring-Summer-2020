#### Derive Diary Day Clusters, using Optimal Matching ####

library(tidyverse)
library(haven)
library(TraMineR)
library(cluster)
library(dendextend)

source("global_variables.R")

#### import data ####
data_folder <- "data"

data <- read_dta(file.path(data_folder, "uk_3wave_caddi_data.dta")) %>%
  mutate(diaryid = row_number(), .after = 1L)

activity_key <- read_csv("activity_key_final.csv")

### code primary activity ####

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

primary_coded5_wide <- pivot_wider(primary_long,
                                   id_cols = diaryid,
                                   names_from = category_slot,
                                   values_from = coded5_activity)

##select population to be studied
active_population <- data %>%
  mutate(across(c(dagegrp, econstat), as_factor)) %>%
  filter(dagegrp != "65+", econstat != "Student") %>%
  select(diaryid, crudewt2)

#### convert to sequence ####

#check na diaries (27 diaries)
summarise(primary_coded5_wide, across(starts_with("pri"), ~sum(is.na(.))))

#remove those diaries, and then filter by the active population
diaries_coded5 <- primary_coded5_wide %>%
  filter(if_all(matches(act_regex), ~!is.na(.)))  %>%
  right_join(active_population, by = "diaryid")
  
#create sequence object
activity_seq <- seqdef(diaries_coded5,
                       informat = "STS",
                       var = str_which(names(diaries_coded5), act_regex), 
                       id = diaries_coded5$diaryid, 
                       weights = diaries_coded5$crudewt2)

#check it worked
summary(activity_seq)
seqdplot(activity_seq, with.legend = FALSE, border = NA)

#### calculate distances #####
activity_seq_dist <- seqdist(activity_seq,
                             method = "DHD",
                             full.matrix = FALSE)

#### cluster ####

activity_clusters <- agnes(activity_seq_dist, method = "gaverage", par.method = -0.3)

#use dendextend package to manage information on clusters
activity_dend <- as.dendrogram(activity_clusters)

#determine number of clusters
n_clusters <- 60

#get cluster number ordering in the order in which they appear in the plot
clustering <- cutree(activity_dend, k = n_clusters, order_clusters_as_data = FALSE) %>%
  as_tibble(rownames = "label") %>% 
  rename(cluster = value) %>%
  mutate(label = as.numeric(label))

#### re-cluster and write key to file ####

#add back into dataset, inspect clustering
primary_long %>%
  right_join(select(diaries_coded5, diaryid, crudewt2), by = "diaryid") %>%
  full_join(clustering, by = c("diaryid" = "label")) %>%
  mutate(description = "") %>%
  plot_cumulative()

#copy Lesnard's heights plot to determine possible optimal cluster numbers
heights <- sort(activity_clusters$height, decreasing = T) 
ggplot(data = NULL, aes(x = 2:51, y = heights[2:51])) + #first height is enormous 
  geom_point() +
  scale_y_log10() #note log scale

#read in reclustering key and write to file
reclustering <- read_csv("reclustering_final.csv") %>%
  mutate(description = fct_reorder(description, original_cluster)) %>%
  group_by(description) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup() 

clustering %>%
  rename(cluster60 = cluster) %>%
  left_join(reclustering, by = c("cluster60" = "original_cluster")) %>%
  write_rds(file.path(data_folder, "clusters_final.rds"))

#### END ####
