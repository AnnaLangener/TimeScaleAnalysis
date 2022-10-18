##### Data matching Timon ####
##############################


library('dplyr')
library("tidyr")

# BEHAPP ID's of participants with at least 3 weeks of Behapp data
BEHAPP_ID = c(117113,117114,117119,117121,117130,117129,117131,117134,117135,117137)

###### Load dataframes #########
################################
passive_match <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/Data_PassiveMeasure2709_24hmissing.csv") # created with ESM match function

ESM_data <-  read.csv('/Users/annalangener/Nextcloud/BEHAPP data/SocialContext.csv') # Based on dataCleaned but with BEHAPP ID

####### Merge dataframes/ select variables #######
##################################################
###### Affect data
# Only keep participants with at least 3 weeks of behapp data
ESM_data = ESM_data[ESM_data$BEHAPP_ID %in% BEHAPP_ID,]

#Create affect mean & sum scores
ESM_data['pa_mean'] <- ESM_data %>% select(c("pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos")) %>% rowMeans()
ESM_data['na_mean'] <-  ESM_data %>% select(c("na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos")) %>% rowMeans()

ESM_data['pa_sum'] <- ESM_data %>% select(c("pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos")) %>% rowSums()
ESM_data['na_sum'] <-  ESM_data %>% select(c("na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos")) %>% rowSums()

# Remove questionnaires with missing affect data (e.g., experience questionnaires, one participant also had one morning questionnaire with only missing)
ESM_data <- ESM_data[!is.na(ESM_data$pa_mean),]
ESM_data <- ESM_data[!is.na(ESM_data$na_mean),]

# Select variables
ESM_affect <- ESM_data  %>% select(c("BEHAPP_ID","index_time","questionListName", "pa_mean","na_mean","pa_sum","na_sum","pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos","na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos",
                                     "interact_energy_cost_sliderNegPos","interact_energy_give_sliderNegPos","interact_enjoy_sliderNegPos","interact_meaning_sliderNegPos","interact_mode_multipleChoice_string","interact_myself_sliderNegPos","interact_other_enjoy_sliderNegPos","interact_time_multipleChoice_string"))     


# Change colnames 
colnames(ESM_affect)[colnames(ESM_affect) == 'BEHAPP_ID'] <- 'ParticipantNumber'
colnames(ESM_affect)[colnames(ESM_affect) == 'index_time'] <- 'Date'

# Merge dataframe
passive_match <- passive_match[passive_match$timescale_beforeESM == "1h",]

Affect_Passive <- merge(ESM_affect,passive_match, by = c("ParticipantNumber","Date"), all = FALSE)

# Remove double column (one participant filled morning questionnaire out twice but with one NA)
Affect_Passive_subset = Affect_Passive %>% select(c("Date","ParticipantNumber","questionListName", "pa_mean","na_mean","pa_sum","na_sum","pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos","na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos",
                                                   "interact_energy_cost_sliderNegPos","interact_energy_give_sliderNegPos","interact_enjoy_sliderNegPos","interact_meaning_sliderNegPos","interact_mode_multipleChoice_string","interact_myself_sliderNegPos","interact_other_enjoy_sliderNegPos","interact_time_multipleChoice_string",'COMMUNICATION_min','APP_USAGE_min','APPS_OPENED_number',"timescale_beforeESM"))     

  
Affect_Passive_subset <- Affect_Passive_subset[!duplicated(Affect_Passive_subset),]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Daily Assessment",]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Morning Assessment",]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Morning assessment 1",]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Evening Assessment",]


Affect_Passive_subset_20p <- Affect_Passive_subset[sample(nrow(Affect_Passive_subset), nrow(Affect_Passive_subset)*0.2),]


#write.csv(Affect_Passive,"/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_220422.csv")
write.csv(Affect_Passive_subset_20p,"/Users/annalangener/Nextcloud/BEHAPP data/Timon_20percent_24hmissing_1haggregation.csv")


### Only ESM #####
##################

ESM_data <-  read.csv('/Users/annalangener/Nextcloud/BEHAPP data/SocialContext.csv') # Based on dataCleaned but with BEHAPP ID

#Create affect mean & sum scores
ESM_data['pa_mean'] <- ESM_data %>% select(c("pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos")) %>% rowMeans()
ESM_data['na_mean'] <-  ESM_data %>% select(c("na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos")) %>% rowMeans()

ESM_data['pa_sum'] <- ESM_data %>% select(c("pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos")) %>% rowSums()
ESM_data['na_sum'] <-  ESM_data %>% select(c("na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos")) %>% rowSums()

# Remove questionnaires with missing affect data (e.g., experience questionnaires, one participant also had one morning questionnaire with only missing)
ESM_data <- ESM_data[!is.na(ESM_data$pa_mean),]
ESM_data <- ESM_data[!is.na(ESM_data$na_mean),]

# Select variables
ESM_affect <- ESM_data  %>% select(c("BEHAPP_ID","index_time","questionListName", "pa_mean","na_mean","pa_sum","na_sum","pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos","na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos",
                                     "interact_energy_cost_sliderNegPos","interact_energy_give_sliderNegPos","interact_enjoy_sliderNegPos","interact_meaning_sliderNegPos","interact_mode_multipleChoice_string","interact_myself_sliderNegPos","interact_other_enjoy_sliderNegPos","interact_time_multipleChoice_string"))     


# Change colnames 
colnames(ESM_affect)[colnames(ESM_affect) == 'BEHAPP_ID'] <- 'ParticipantNumber'
colnames(ESM_affect)[colnames(ESM_affect) == 'index_time'] <- 'Date'

# Merge dataframe

Affect_Passive <- ESM_affect

# Remove double column (one participant filled morning questionnaire out twice but with one NA)
Affect_Passive_subset = Affect_Passive %>% select(c("Date","ParticipantNumber","questionListName", "pa_mean","na_mean","pa_sum","na_sum","pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos","na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos",
                                                    "interact_energy_cost_sliderNegPos","interact_energy_give_sliderNegPos","interact_enjoy_sliderNegPos","interact_meaning_sliderNegPos","interact_mode_multipleChoice_string","interact_myself_sliderNegPos","interact_other_enjoy_sliderNegPos","interact_time_multipleChoice_string"))     


Affect_Passive_subset <- Affect_Passive_subset[!duplicated(Affect_Passive_subset),]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Daily Assessment",]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Morning Assessment",]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Morning assessment 1",]
Affect_Passive_subset <- Affect_Passive_subset[Affect_Passive_subset$questionListName != "Evening Assessment",]


Affect_Passive_subset_20p <- Affect_Passive_subset[sample(nrow(Affect_Passive_subset), nrow(Affect_Passive_subset)*0.2),]


#write.csv(Affect_Passive,"/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_220422.csv")
write.csv(Affect_Passive_subset_20p,"/Users/annalangener/Nextcloud/BEHAPP data/Timon_20percent_onlyESM.csv")









