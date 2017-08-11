library(dplyr)
library(magrittr)

raw_data <- read.csv("data/wildcam-darien-flattened.csv")

data %>% summarise(n_distinct(subject_ids), n_distinct(classification_id)) 

data %<>% select(., -X, -workflow_version, -task, -total_species, -species_index) %>% #these get in the way, and the counts are wrong
     group_by(subject_ids) %>% # count up the number of distinct classification IDs
     mutate(., num_class = n_distinct(classification_id)) %>% #because there will be >1 row per classification_id if >1 spp
     arrange(., subject_ids, classification_id) 
data %>% View

# First, Identify the number of species in each subject by taking the median number of species reported across all volunteers. 
# This has to clean up times when volunteers submitted multiple classifications for a given species.

########### CLEAN UP MULTIPLE VOTES PER USER ###############
#number of different species should match the number of submissions per user. This will pull out any erroneous classification (not just the duplicate spp)

bad_counts <- check_dups(raw_data)

if(dim(bad_counts)[1] > 0) {
     # need to figure out how to merge the different column groups, or else this will break.
     print(paste("Dropping", dim(check_dups(raw_data))[1], "duplicate classifications"))
     
     cleaned_classifications <- raw_data %>% group_by(subject_ids, classification_id) %>% 
          mutate(., num_species = n_distinct(choice)) %>%
          group_by(., subject_ids, classification_id, num_class, num_species, choice) %>% 
          #summarise_all(., sum) # adds up counts for duplicates of spp, only works if everything is numeric
          summarise_all(., first) # takes the first record per user per species classification
} else {
     cleaned_classifications <- check_spp_counts
}


check_dups(cleaned_classifications)

# now we have a cleaned raw classification file!! # let's aggregate the number of species in a subject
# for a given subject, we take the number of species in the image as the median number of species reported, rounded up

### CRAP, the aggregate number of species needs to be decided at the subject x classification id level. Well, only count one species count per user, anyway.
# Calculate subject-level metrics 
subject_metrics <- cleaned_classifications %>% ungroup %>%
     group_by(., subject_ids) %>%
     mutate(., num_votes = n(), # if a  user ids >1 spp, there will be more votes than classifications
            diff_species = n_distinct(choice)) # count the total number of different species reported by different users, for pielous score

# Calculate aggregate number of species per subject
species_counts <- cleaned_classifications %>% ungroup %>%
     group_by(subject_ids, classification_id) %>%
     summarise(total_spp_by_user = mean(num_species)) %>% #Need to select only one row per classification_id, then look across those.
     summarise(., agg_num_species = round(median(total_spp_by_user), 0)) #aggregate species count, which is median rounded up

classifications <- left_join(subject_metrics, species_counts)



### For each species, aggregate counts and behavior votes. ###
# okay, so there's a difference between the proportion of VOTES and the proportion of classifications. 
# If some users ID >1 species in a single species image, there will be more votes than classifications. 
# The opposite is true for when some users only ID 1 species in a multi-species image.

#Need to Identify behavior columns, how many columns, etc. Maybe characterize different types of columns that should be treated differently?
subject_columns <- c("subject_ids", "classification_id", "choice")
subject_metrics <- c("num_votes", "num_class", "agg_num_species", "diff_species")
#howmany_column <- "how_many"
single_q_columns <- "how_many"
multi_q_columns <- c("behavior_EATING","behavior_GROOMING", "behavior_INTERACTING", "behavior_RUNNING", "behavior_SITTING", "behavior_STANDING", "behavior_WALKING")

# Calculate the proportion of T from a list of T/F/NA. Should work on non-required q's as well.

#this provides one row per species ID per classification. We actually don't really need all the grouping variables... could just pull them apart and save for later.
grouped_classifications <- 


#Tally the votes for each species ID'd within a subject
species_votes <- classifications %>% ungroup %>%
     select_(., .dots = c(subject_columns, subject_metrics)) %>%
     group_by(., subject_ids, num_class, num_votes, agg_num_species, diff_species, choice) %>%
     summarise(., votes = n_distinct(classification_id)) %>% #count up the number of votes per species choice
     mutate(propvote = votes/sum(votes), #calculate proportion of votes for this species
            propclass = votes/num_class) #calculate proportion of classifications for this species

# Aggregate counts (how many) - IFF NUMERIC MAPPING
howmany_votes <- classifications %>% 
     ungroup %>%
     select_(., .dots = c(subject_columns, howmany_column)) %>%
     summarise_at(., .cols = howmany_column, funs(mean_count = mean, med_count = median, min_count = min, max_count = max))

# Aggregate counts or other single questions
howmany_votes <- classifications %>% 
     ungroup %>%
     select_(., .dots = c(subject_columns, howmany_column)) %>%
     summarise_at(., .cols = howmany_column, funs(mean_count = mean, med_count = median, min_count = min, max_count = max))


# Okay, so the full dataset has all of the aggregate votes per species. The only thing left is to select the top n species for each subject.
all_data <- species_votes %>% full_join(howmany_votes) 

# probably calculate the pielous score as well in here.



########### CHOOSE ONLY CONSENSUS SPECIES AND CLEAN UP DATA ##############
#full_dataset <- read.csv(file = "data/jdata-flattened-testing-ties.csv")


choose_top_species <- function(aggregated_data) {
     most_species <- max(aggregated_data$agg_num_species)
     out <- list()
     
     for (i in 1:most_species) {
          temp <- aggregated_data %>% ungroup(.) %>%
               filter(., agg_num_species == i) %>% # filter to i number of species
               group_by(., subject_ids) %>%
               top_n(n = i, wt = votes) %>% #just select top i species based on vote
               mutate(., consensus_species = choice) %>% #add this as the consensus species column
               mutate(., resolve = ifelse(n() == i, "ok", "tie")) %>% #if more than i answers per subject ID, it's a tie. this works when i >1!!!!
               top_n(n = i, wt = choice) #take the last value (or last alphabetically)
          out[[i]] <- temp 
     }
     
     consensus_data <- do.call(what = rbind, args = out) %>% 
          arrange(., subject_ids)
     return(consensus_data)
}


clean_up_consensus_data <- function(consensus_dataset, 
                                    subject_level_cols = c("subject_ids", "num_class", "num_votes", "agg_num_species"),
                                    agg_cols = c("consensus_species", "votes", "propvote", "propclass", "resolve"),
                                    how_many_cols = c("mean_count", "med_count", "min_count", "max_count"),
                                    yesno_columns = c("young", "horns"),
                                    behavior_cols = c()) {
     cleaned_dataset <- consensus_dataset %>% select_(., .dots = c(subject_level_cols, agg_cols, how_many_cols, yesno_columns, behavior_cols))
     return(cleaned_dataset)
}

consensus_data <- choose_top_species(all_data)

write.csv(final_dat, file = "data/wildD_aggregation.csv")
