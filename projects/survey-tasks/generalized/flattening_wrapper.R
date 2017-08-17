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

########## Flatten by calling the code from the flattening_functions file. 

final_data <- run_json_parsing(data = jdata)

View(final_data)
write.csv(final_data, file = paste0(project_name, "-flattened.csv"))
