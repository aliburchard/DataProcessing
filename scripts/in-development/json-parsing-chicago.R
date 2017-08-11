library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)

chicago_unfiltered <- read.csv("data/chicago-wildlife-watch-classifications.csv", stringsAsFactors = F)

# So, need to limit to final workflow version and ideally split by task. T0 is clearly the only task we really care about in this dataset (though note the changed format of current site). 

# Run things on filtered and unfilterd to compare
chicago <- chicago_unfiltered %>% filter(., workflow_version == 397.41)
chicago$annotations[1] %>% prettify()


############### SURVEY TASK
head(chicago)
for (i in 10:20) {
     chicago$annotations[i] %>% prettify %>% print
}

# preliminary flat
basic_flat <- chicago %>% 
     select(., subject_ids, classification_id, workflow_version, annotations) %>%
     as.tbl_json(json.column = "annotations") %>%
     gather_array(column.name = "task_index") %>% 
     spread_values(task = jstring("task"), task_label = jstring("task_label"))

View(basic_flat)

basic_flat_with_values <- chicago %>% 
     select(., subject_ids, classification_id, workflow_version, annotations) %>%
     as.tbl_json(json.column = "annotations") %>%
     gather_array(column.name = "task_index") %>% # really important for joining later
     spread_values(task = jstring("task"), task_label = jstring("task_label"), value = jstring("value")) 

View(basic_flat_with_values)

chicago_summary <-  basic_flat_with_values %>% 
     gather_keys %>%
     append_values_string()

chicago_summary %>% View # this will have all the classification IDs; if Value is empty, then the field will be null. This will have multiple rows per classification if there are multiple tasks completed

chicago_summary %>% data.frame %>% group_by(., workflow_version, key, task) %>% summarise(., n())

# quick check the filtered original data
chicago %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id), n_distinct(workflow_version))


# grab choices; append embedded array values just for tracking
# Note that this will break if any of the tasks are simple questions. You would need to split by task before here.
chicago_choices <- basic_flat_with_values %>%
     enter_object("value") %>% json_lengths(column.name = "total_species") %>% 
     gather_array(column.name = "species_index") %>% #each classification is an array. so you need to gather up multiple arrays.
     spread_values(choice = jstring("choice"), answers = jstring("answers")) #append the answers as characters just in case

# if there are multiple species ID'd, there will be multiple rows and array.index will be >1
chicago_choices %>% View
chicago_choices %>% group_by(., classification_id) %>% summarise(., count = n(), max(species_index)) %>% arrange(., -count)

# grab answers - for some reason, this keeps rows even if there are no answers! 
# Note that this last bit is the part that would need to be customized per team, I think
chicago_answers <- chicago_choices %>% 
     enter_object("answers") %>% 
     spread_values(how_many = jstring("HWMN"), wow = jstring("CLCKWWFTHSSNWSMPHT"), off_leash = jstring("CLCKSFDGSFFLSH"))

chicago_answers %>% View     
chicago_answers %>% group_by(classification_id) %>% summarise(., n())     

# in theory, you want to tie all of these back together just in case there are missing values
add_choices <- left_join(basic_flat_with_values, chicago_choices)
tot <- left_join(add_choices, chicago_answers)
flat_data <- tot %>% select(., -task_index, -task_label, -value)

write.csv(flat_data, file = "data/chicago-flattened.csv")

