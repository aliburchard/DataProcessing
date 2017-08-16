rm(list = ls())

library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

source(file = "projects/survey-tasks/generalized/flattening_functions.R") 

############## INTERACTIVE - CLEANING OF CLASSIFICATION DATA AND SPECIFYING FIELDS ###################

# Specify Project
project_name <- "michigan"
classifications_file <- "data/michigan-zoomin-classifications.csv"


# Examine data
jdata <- read.csv(classifications_file, stringsAsFactors = F)

# Set project-specific details
check_workflow(jdata) %>% View
workflow_id_num <- 2276
workflow_version_num <- 463.55

# Identify task-specific details
View_json(jdata)
survey_id <- c("T3")
single_choice_Qs <-  c("HOWMANYANIMALSDOYOUSEE")
single_choice_colnames  <-  c("how_many")
multi_choice_Qs <- c("WHATISTHEANIMALSDOING")
multi_choice_colnames <- c("behavior")

# limit to relevant workflow id and version
#jdata <- read.csv(classifications_file, stringsAsFactors = F)
jdata <- jdata %>% filter(., workflow_id == workflow_id_num, workflow_version == workflow_version_num)


########## Flattening is less interactive - this should probably be a separate script instead of a bunch of functions, but, well, it is what it is. #########

# All of these functions take variables passed in above. I'm sure there's a less circular way to write this.
flattened <- flatten_to_task(jdata, survey_task_id = survey_id) #Produces one row per classification. Might be useful when wanting to recombine other, potentially breaking, task types.
choices_only <- get_choices(flattened) # grabs all of the choices. Can produce >1 row per classification.
single_choice_answers <- get_single_choice_Qs(choices_only, cols_in = single_choice_Qs, cols_out = single_choice_colnames) #cols_out is optional
multi_choice_answers <- get_multi_choice_Qs(choices_only, cols_in = multi_choice_Qs, cols_out = multi_choice_colnames) #cols_out is optional
final_data <- combine_answers() #sequentailly left-joins all the dataframes if they exist.



View(final_data)
write.csv(final_data, file = paste0(project_name, "-flattened.csv"))
