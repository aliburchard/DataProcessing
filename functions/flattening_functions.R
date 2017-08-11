# 
# library(tidyjson)
# library(magrittr)
# library(jsonlite)
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(lubridate)



####################### FLATTEN ####################### 
# Data formats:
# choice is standard and always one item.
# answers is standard and contains a list of multiple values, some of which can be arrays. Empty arrays are handled differently than empty lists.
# how interpretable the text is, and whether you need to link to workflow content to get vowels, depends on when the project was uploaded.

# This is meant to be run on a single workflow version. 


# FLATTEN TO TASK
# Highest order flattening to extract relevant tasks *within* a classification.

flatten_to_task <- function(json_data, survey_task_id = NA) {
     flat_to_task <- json_data %>% 
          select(., subject_ids, user_name, classification_id, workflow_version, annotations) %>%
          as.tbl_json(json.column = "annotations") %>%
          gather_array(column.name = "task_index") %>% # really important for joining later
          spread_values(task = jstring("task"), task_label = jstring("task_label"), value = jstring("value"))
     
     #If the task needs filtering, do it here.
     if (!is.na(survey_task_id)) {
          # For some unknown reason, updating dplyr breaks how filter handles json objects. Hence the super base R filtering.
          out <- flat_to_task[flat_to_task$task == survey_task_id, ]     
     } else {
          out <- flat_to_task
     }
     
     return(out)
}


# GRAB CHOICES

get_choices <- function(survey_data) { 
     # species_key is defined at entry or else defaults to choice
     # produces one row per species/submission per classification. If a user selects "lion" twice, this counts as two submissions.
     with_choices <- survey_data %>%
          enter_object("value") %>% # all of the annotation information is contained within this value column, which is named by the call in the previous code chunk.
          json_lengths(column.name = "total_submissions") %>% # Note that if users submit multiple classifications for a single species, this will be off.
          gather_array(column.name = "submission_index") %>% # each classification is an array. so you need to gather up multiple arrays.
          spread_values(choice = jstring("choice")) # "choice" is hardcoded as a key name in the zooniverse data. if this changes, this script will need updating.
     return(with_choices)
}     

# GRAB SINGLE ANSWER QUESTIONS
# Single Choice Questions (e.g. how many, are there young, horns, etc.) These can be Yes/No or have >2 choices. I guess that can be dealt with in aggregation.

#Single Choice Qs

spread_single_choice_values <- function(x, names, values) {
     stopifnot(length(names)==length(values))
     do.call("spread_values", c(list(x), setNames(as.list(values), names)))
}

get_single_choice_Qs <- function(with_choices_data, cols_in, cols_out) {
     if (!exists("single_choice_Qs")) {
          print("You have not entered any Single Choice subquestions, such as 'how many?', 'are there any young?', or 'do you see anything really cool?'")
          single_choices <- NULL
     } else {
          if(missing(cols_out)) cols_out <- cols_in
          print("Getting single-choice questions.")
          print(paste("The subquestion", cols_in, "will be returned as", cols_out)) # paste returns a 1:1 (so three returns for three columns); cat returns all at once
          
          single_choices <- with_choices_data %>% 
               enter_object("answers") %>% 
               spread_single_choice_values(cols_out, 
                                           lapply(cols_in, jstring)
               ) #lapply(strings, jstring) just makes everything a jstring.
     }
     return(single_choices) 
}



# GRAB MULTI ANSWER QUESTIONS
# Multi Choice Questions (eg. behaviors, which sides are visible)
# Annoyingly, you need to separately do this for every multiple choice question and then recombine into a single multi-choice dataset. 
get_multi_choice_Qs <- function(with_choices_data, cols_in, cols_out) {
     
     if (!exists("multi_choice_Qs")) {
          print("You have not entered any Multiple Choice subquestions, such as 'what behaviors do you see?', 'what sides of the animal are visible?'")
          combined <- NULL
     } else {
          if(missing(cols_out)) cols_out <- cols_in
          print("Getting multi-choice questions.")
          print(paste("The subquestion", cols_in, "will be returned as", cols_out)) # paste returns a 1:1 (so three returns for three columns); cat returns all at once
          
          stopifnot(length(cols_in) == length(cols_out))
          combined <- with_choices_data
          
          for (m in 1:length(cols_in)) {
               #col_in    <- multi_choice_Qs[m]
               #col_out   <- multi_choice_colnames[m]
               
               col_in    <- cols_in[m]
               col_out   <- cols_out[m]
               array_ind <- paste(col_out, "ind", m, sep=".")
               prepend   <- as.character(col_out)
               
               multi_choice_m <- with_choices_data %>% 
                    enter_object("answers") %>%
                    enter_object(col_in) %>%
                    gather_array(column.name = array_ind) %>%
                    append_values_string(col_out) 
               
               # Spread columns
               multi_choice_m_flat <- multi_choice_m %>% data.frame %>%
                    select_(., paste0("-", array_ind)) %>% #have to paste these for standard evaluation
                    mutate(., value_present = 1, pre_col = prepend) %>%
                    unite_(., "out", c("pre_col", col_out)) %>%
                    spread_(., key = "out", value = "value_present", fill = 0)
               
               
               # Need to left_join after creation of each new columns because there might be multiple rows per classification, and this could vary.
               combined <- left_join(combined, multi_choice_m_flat)
          }
     
     return(combined)     
     }
}



## COMBINE ALL THE THINGS - doesn't actually seem to work. for some reason keeps returning a list??
combine_answers <- function(flat_data, choices_data = NULL, single_data = NULL, multi_data = NULL) { #also passed as null from previous functions if empty.
     full_data <- flat_data
     if(!is.null(choices_data)) full_data <- left_join(flat_data, choices_data)
     if(!is.null(single_data)) full_data <- left_join(full_data, single_data)
     if(!is.null(multi_data))  full_data <- left_join(full_data, multi_data)
     return(full_data)
}

