library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)

wilde <- read.csv("data/points-wildebeest.csv", stringsAsFactors = F)


dat <- wilde %>% filter(., workflow_id == 78, workflow_version == 36.60) 
dat$annotations[1] %>% prettify



# View the data, note that anything with zero length "value" field is dropped
dat$annotations %>% as.tbl_json %>% 
     gather_array() %>%
     spread_values(task = jstring("task"), tasklabel = (jstring("task_label"))) %>%
     enter_object("value") %>%
     gather_array() %>%
     gather_keys() %>% 
     append_values_string() %>% head %>% View

# Grab the top-level info for ALL classifications
all_submissions <- dat %>% 
     select(., subject_ids, classification_id, user_name, workflow_id, workflow_version, created_at, annotations) %>%
     as.tbl_json(json.column = "annotations") %>%
     gather_array(column.name = "task_index") %>%
     spread_values(task = jstring("task"), task_label = (jstring("task_label"))) %>%
     gather_keys() %>%
     json_lengths(column.name = "total_marks") %>% 
     filter(., key == "value") 


flattened <- dat %>% 
     select(., subject_ids, classification_id, user_name, workflow_id, workflow_version, created_at, annotations) %>%
     as.tbl_json(json.column = "annotations") %>%
     gather_array(column.name = "task_index") %>%
     spread_values(task = jstring("task"), task_label = (jstring("task_label"))) %>%
     enter_object("value") %>%
     gather_array(column.name = "mark_index") %>% #don't gather keys, whole point is that you are spreading out the damn keys.
     spread_values(tool_label = jstring("tool_label"), xcoord = jnumber("x"), ycoord = jnumber("y"), tool = jstring("tool"))



#check that this works. empty plus full should equal original total classifications.
all_submissions %>% filter(., total_marks == 0) %>% summarise(., n(), n_distinct(classification_id), n_distinct(subject_ids))
dat %>% summarise(., n(), n_distinct(classification_id), n_distinct(subject_ids))
flattened %>% summarise(., n(), n_distinct(classification_id), n_distinct(subject_ids))


# recombine datasets: merge flat and empty (okay, do a full + meaty join)
# all_submissions - has one record per classification per subject
# flattened has one record per mark per classification, but only if the counter >0

tot <- left_join(all_submissions, flattened) # note that you have to drop array index because they reference two different things

data_out <- tot %>% 
     mutate(., task_label = str_trunc(task_label, width = 25)) %>%
     select(., -task_index, -key)

write.csv(x = data_out, file = "data/flattened-wildebeest.csv")
