rm(list = ls())

library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

source(file = "projects/survey-tasks/generalized/flattening_script.R") 

############## INTERACTIVE - CLEANING OF CLASSIFICATION DATA AND SPECIFYING FIELDS ###################

# You will need to define variables that will be used in the run_json_parsing function. They need the following names:

# REQUIRED VARIABLES
# jdata
# survey_id
# workflow_id_num 
# workflow_version_num 

# OPTIONAL VARIABLES
# single_choice_Qs 
# single_choice_colnames 
# multi_choice_Qs 
# multi_choice_colnames


# Specify Project
project_name <- "chicago"
classifications_file <- "data/chicago-wildlife-watch-classifications.csv"

# Examine data
jdata <- read.csv(classifications_file, stringsAsFactors = F)

# Set project-specific details
workflow_id_num <- 4377
workflow_version_num <- 2.20

# Chicago also needs to deal wth these versions too, possibly with separate calls to the flattening script, depending on the task structures. 
# workflow_id_num <- 2334
# workflow_version_num <- c(397.41, 406.45)
# workflow_id_num <- 3054
# workflow_version_num <- 5.70


# limit to relevant workflow id and version
jdata <- jdata %>% filter(., workflow_id == workflow_id_num, workflow_version == workflow_version_num)


# Identify task-specific details. These variable names are important, because I haven't figured out how to define them in the function call 
# (there's some weird referencing. I don't know. The function definitions and scripts could be improved, but things seem to generally work.)
View_json(jdata)

survey_id <- c("T0")
single_choice_Qs <-  c("HOWMANY")
single_choice_colnames  <-  c("how_many")
question_id <- c("T1")

# Flatten by calling the code from the flattening_functions file. This isn't the cleanest approach, but it'll have to do.
# If you want to combine multiple workflows or multiple tasks before aggregating, this is the time to do it.
survey_data <- run_json_parsing(data = jdata)

View(final_data)
write.csv(final_data, file = paste0(project_name, "-flattened.csv"))

# Now grab and flatten the question task (this should actually be called inside of the parsing script, as right now it's duplicating effort)
T1_data <- jdata %>% flatten_to_task %>% filter_to_task(task_id = question_id)
write.csv(T1_data, file = paste0(project_name, "-", question_task_id, "-flattened.csv"))

