library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

#This works for projects that ONLY have "how many" subquestions.
jdata_unfiltered <- read.csv(file = "data/wildcam-darien-classifications.csv", stringsAsFactors = F)

# So, need to limit to final workflow version and ideally split by task. 
jdata_unfiltered %>% mutate(., created_at = ymd_hms(created_at)) %>% 
     group_by(., workflow_id, workflow_version) %>% summarise(., max(created_at), n()) %>% View

# filter by workflow version
jdata <- jdata_unfiltered %>% filter(., workflow_version %in% c(577.9, 579.9))


############### SURVEY TASK
head(jdata)
for (i in 15:20) {
     jdata$annotations[i] %>% prettify %>% print
}

# preliminary flat

basic_flat_with_values <- jdata %>% 
     select(., subject_ids, classification_id, workflow_version, annotations) %>%
     as.tbl_json(json.column = "annotations") %>%
     gather_array(column.name = "task_index") %>% # really important for joining later
     spread_values(task = jstring("task"), task_label = jstring("task_label"), value = jstring("value")) 

View(basic_flat_with_values)

basic_summary <-  basic_flat_with_values %>% 
     gather_keys %>%
     append_values_string()

basic_summary %>% View # this will have all the classification IDs; if Value is empty, then the field will be null. This will have multiple rows per classification if there are multiple tasks completed

basic_summary %>% data.frame %>% group_by(., workflow_version, key, task) %>% summarise(., n())

# quick check the filtered original data
jdata %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id), n_distinct(workflow_version))

#--------------------------------------------------------------------------------#
# split into survey vs. non-survey data frames. Question is flattened and can be exported as a separate file now.
survey <- basic_flat_with_values 

###----------------------------### SURVEY FLATTENING ###----------------------------### 

# grab choices; append embedded array values just for tracking
with_choices <- survey %>%
     enter_object("value") %>% json_lengths(column.name = "total_species") %>% 
     gather_array(column.name = "species_index") %>% #each classification is an array. so you need to gather up multiple arrays.
     spread_values(choice = jstring("choice")) 

# if there are multiple species ID'd, there will be multiple rows and array.index will be >1
with_choices %>% View
with_choices %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id))
with_choices %>% group_by(., classification_id) %>% summarise(., count = n(), max(species_index)) %>% arrange(., -count)


### FLATTEN WITH SUBQUESTIONS. NEED INPUT HERE.
#Single Choice Qs
count_string <- "HOWMANY" 



# grab answers - for some reason, this keeps rows even if there are no answers! 
# Note that this last bit is the part that would need to be customized per team, I think
with_answers <-  with_choices %>% 
     enter_object("answers") %>% 
     spread_values(how_many = jstring(count_string))
      

with_answers %>% View

# the number of rows where count >1 should be the same as the difference between the row count for add_counces and basic_flat
with_answers %>% group_by(classification_id) %>% summarise(., count = n()) %>% arrange(., -count) %>% View   

# in theory, you want to tie all of these back together just in case there are missing values
add_choices <- left_join(survey, with_choices)
tot <- left_join(add_choices, with_answers)
flat_data <- tot %>% select(., -task_index, -task_label, -value)

#check that the number of distinct subject IDs and classification IDs is still the same
flat_data %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id), n()) #flattened,
jdata %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id), n()) #original

write.csv(flat_data, file = "data/wildcam-darien-flattened.csv")

