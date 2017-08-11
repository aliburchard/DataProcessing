

# #MichiganZoomin
# jdata <- read.csv("data/michigan-zoomin-filtered.csv", stringsAsFactors = F)
# survey_id <- c("T3")
# single_choice_Qs <-  c("HOWMANYANIMALSDOYOUSEE")
# single_choice_colnames  <-  c("how_many")
# multi_choice_Qs <- c("WHATISTHEANIMALSDOING")
# multi_choice_colnames <- c("behavior")
#
#
# Wildcam Darien
# jdata <- read.csv("data/wildcam-darien-classifications.csv", stringsAsFactors = F)
# single_choice_Qs <- c("HOWMANY")
# single_choice_colnames <- c("how_many")
# survey_id <- c("T0")
#

# Camera Catalogue
project_name <- "camera catalogue"
name_dat <- read.csv("data/wildcam_gorongosa_test_raw.csv", stringsAsFactors = F)
jdata <- read.csv("data/se.angola.classifications_Ali.csv", stringsAsFactors = F, header = F)
names(jdata) <- names(name_dat)
survey_id <- c("T0")
single_choice_Qs <- c("HWMN", "WHCHSDFTHNMLSVSBL")
single_choice_colnames <- c("how_many", "side")



# # Wildcam Gorongosa
# jdata <- read.csv("data/wildcam_gorongosa_test_raw.csv", stringsAsFactors = F)
# survey_id <- c("T1")
# single_choice_Qs <- c("HWMN", "DSNHRNS", "RTHRNNGPRSNT")
# single_choice_colnames <- c("count_string", "horns", "young")
# multi_choice_Qs <- c("WHTBHVRSDS")
# multi_choice_colnames <- c("behavior")
#


# # Chicago Wildlife Watch
# jdata <- read.csv("data/chicago-wildlife-watch-classifications.csv", stringsAsFactors = F)
# check_workflow(jdata)
# jdata <- jdata %>% filter(., workflow_id == 3054, workflow_version == 5.70)
# survey_id <- c("T0")
# single_choice_Qs <- c("HWMN", "CLCKSFTHDGSFFLSH")
# single_choice_colnames <- c("how_many", "young")
#

# # Snapshots At Sea
# jdata <- read.csv("data/snapshots-survey-classifications.csv", stringsAsFactors = F)
# survey_id <- c("T0")
# single_choice_Qs <- c("ISTHEUNDERSIDEOFTHETAILVISIBLE", "IFYESDOESTHETAILBELONGTOAHUMPBACKWHALE")
# single_choice_colnames <- c("tail", "humpback")

# # Wisconsin
# project_name <- "snapshot-wisconsin.csv"
# classifications_file <- "wisconsin-classifications-subset.csv"
# survey_id <- c("T1")
# single_choice_Qs <- c("NG", "DLTHDNTVSBL", "NGPRSNT")
# multi_choice_Qs <- c("BHVR")
# multi_choice_colnames <- c("behavior")
#
# workflow_id <- 1717
# workflow_version <- 304.78
#


#Wildwatch Keyna
workflow_dat <- read.csv("data/wildwatch-kenya-workflows.csv", stringsAsFactors = F)

head(workflow_dat)
names(workflow_dat)

for (i in 1:1) {
     workflow_dat$tasks[i] %>% prettify %>% print
}
     
# 
# # Colorado Corridors
# project_name <- "colorado-corridors"
# classifications_file <- "colorado-corridors-project-classifications.csv"
# survey_id <- c("T0")
# single_choice_Qs <- c("HWMNNMLSDS", "STDRBL", "RTHRNNGPRSNT")
# workflow_id <- 1187
# workflow_version <- 1158.155


check_workflow(jdata) %>% View()
View_json(jdata)


workflow_dat$tasks[i] %>% gather_keys() %>% gather_keys() %>% append_values_string()

