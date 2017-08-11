rm(list = ls())

## This script requires a cleaned and filtered csv file, with only the relevant workflow versions. The versions are specifid in the project-specs file.

source(file = "scripts/flattening_functions.R") 
source(file = "scripts/project-specs.R")

jdata <- read.csv(paste0("data/", classifications_file), stringsAsFactors = F)
jdata <- jdata %>% filter(., workflow_id == workflow_id_num, workflow_version == workflow_version_num)

# Run data input checks and assign variables as necessary.

if(!exists("jdata")) print("You need to give me some data.") 
if(!exists("survey_id")) print("You need to specify the task ID for the survey task.")

if(!exists("single_choice_Qs")) print("You have not specified any single choice questions")
if(!exists("single_choice_colnames")) {
     if(exists("single_choice_Qs")) 
          single_choice_colnames <- single_choice_Qs
} 

if(!exists("multi_choice_Qs")) print("You have not specified any multi choice questions")
if(!exists("multi_choice_colnames")) {
     if(exists("multi_choice_Qs")) 
          multi_choice_colnames <- multi_choice_Qs
} 


# Flatten, grab all the bits and bobs

flattened <- flatten_to_task(jdata, survey_task_id = survey_id) #Produces one row per classification. Might be useful when wanting to recombine other, potentially breaking, task types.
choices_only <- get_choices(flattened)
single_choice_answers <- get_single_choice_Qs(choices_only, cols_in = single_choice_Qs, cols_out = single_choice_colnames) #cols_out is optional
multi_choice_answers <- get_multi_choice_Qs(choices_only, cols_in = multi_choice_Qs, cols_out = multi_choice_colnames) #cols_out is optional



# Recombine everything into one final dataset, with >1 row per classification.
full_data <- flattened

if (!is.null(choices_only)){
     full_data <- left_join(flattened, choices_only)
}
if (!is.null(single_choice_answers)) {
     full_data <- left_join(full_data, single_choice_answers)
}
if (!is.null(multi_choice_answers)) {
     full_data <- left_join(full_data, multi_choice_answers)
}

View(full_data)
write.csv(full_data, file = paste0("data/", project_name, "-flattened.csv"))
