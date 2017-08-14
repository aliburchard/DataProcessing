
## This script requires a cleaned and filtered csv file, with only the relevant workflow versions. The versions are specified in the project-specs file.
rm(list = ls())

source(file = "projects/in-development/generalized/flattening_functions.R") # All the flattening functions
source(file = "projects/in-development/project-specs.R") ## Need to enter or read in project specs

jdata <- read.csv(classifications_file, stringsAsFactors = F)
jdata <- jdata %>% filter(., workflow_id == workflow_id_num, workflow_version == workflow_version_num)


# Run checks on data input (assigns column out so that you don't need to tweak it in the next section)

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



# Flatten, grab all the answers to any questions
flattened <- flatten_to_task(jdata, survey_task_id = survey_id) #Produces one row per classification. Might be useful when wanting to recombine other, potentially breaking, task types.
choices_only <- get_choices(flattened)
single_choice_answers <- get_single_choice_Qs(choices_only, cols_in = single_choice_Qs, cols_out = single_choice_colnames) #cols_out is optional
multi_choice_answers <- get_multi_choice_Qs(choices_only, cols_in = multi_choice_Qs, cols_out = multi_choice_colnames) #cols_out is optional
final_data <- combine_answers() #sequentailly left-joins all the dataframes if they exist.

View(final_data)
write.csv(final_data, file = paste0(project_name, "-flattened.csv"))
