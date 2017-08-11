
### MUST SPECIFY FIELDS SO FLATTENING WILL WORK. Examples included for Michigan Zoomin, Colorado Corridors, Wildcam Darien


## If you have single or multiple choice questions and do not specify them, the flattener will ignore them. 
## If you do not specify column out values, the flattener will use column in values.

# REQUIRED FIELDS 
project_name <- "michigan-zoomin"
classifications_file <- "michigan-zoomin-filtered.csv"
survey_id <- c("T3")
workflow_id_num <- 2276
workflow_version_num <- 463.55
    
# Optional Fields
single_choice_Qs <-  c("HOWMANYANIMALSDOYOUSEE")
single_choice_colnames  <-  c("how_many")
multi_choice_Qs <- c("WHATISTHEANIMALSDOING")
multi_choice_colnames <- c("behavior")
     

## Colorado Corridors
# project_name <- "colorado-corridors"
# classifications_file <- "colorado-corridors-project-classifications.csv"
# survey_id <- c("T0")
# workflow_id_num <- 1187
# workflow_version_num <- 1158.155
# single_choice_Qs <- c("HOWMANYANIMALSDOYOUSEE") 


# Wildcam Darien
# project_name <- "wildcam-darien"
# classifications_file <- "wildcam-darien-classifications.csv"
# survey_id <- c("T0")
# workflow_id_num <- 3033
# workflow_version_num <- 579.9
# single_choice_Qs <- c("HOWMANY")
# single_choice_colnames <- c("how_many")

