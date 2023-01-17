
library('dplyr')
library("tidyr")

# BEHAPP ID's of participants with at least 3 weeks of Behapp data
BEHAPP_ID = c(117113,117114,117119,117121,117130,117129,117131,117134,117135,117137)

###### Load dataframes #########
################################

passive_match <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/Data_PassiveMeasure2709_24hmissing.csv") # created with "Match with ESM data.py" (we did the same for the other two datasets)
#passive_match <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/Data_PassiveMeasure2709_18hmissing.csv") # created with "Match with ESM data.py" (we did the same for the other two datasets)
#passive_match <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/Data_PassiveMeasure2709_12hmissing.csv") # created with "Match with ESM data.py" (we did the same for the other two datasets)

ESM_data <-  read.csv('/Users/annalangener/Nextcloud/BEHAPP data/SocialContext.csv') # CleaningNew.R was used to clean the ESM data, SocialContext_BehappID.py was used to add the Behapp ID

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
ESM_affect <- ESM_data  %>% select(c("BEHAPP_ID","index_time", "pa_mean","na_mean","pa_sum","na_sum","questionListName"))     

# Remove backlog questionnaires (Otherwise the affect measure would be twice included)
ESM_affect <- ESM_affect[ESM_affect$questionListName != "Backlog Interaction",]

# Change colnames 
colnames(ESM_affect)[colnames(ESM_affect) == 'BEHAPP_ID'] <- 'ParticipantNumber'
colnames(ESM_affect)[colnames(ESM_affect) == 'index_time'] <- 'Date'

# Merge dataframe
Affect_Passive <- merge(ESM_affect,passive_match, by = c("ParticipantNumber","Date"), all = FALSE)

# Remove double column (one participant filled morning questionnaire out twice but with one NA)
Affect_Passive_subset = Affect_Passive %>% select(c("Date", "pa_mean","na_mean","questionListName", "ParticipantNumber","timescale_beforeESM",'SOCIAL_min','COMMUNICATION_min','APP_USAGE_min','APPS_OPENED_number','Cluster_HOME_min','UNIQUE_STAYPOINTS_number','TIME_STATIONARY_min','TOTAL_MACHASHES_number','UNIQUE_MACHASHES_number','CALL_TOTAL_min',"BLUETOOTH_TOTAL_MACHASHES_number","BLUETOOTH_UNIQUE_MACHASHES_number", "CALL_TOTAL_min" ))
Affect_Passive <- Affect_Passive[!duplicated(Affect_Passive_subset),]

#### Save dataframes

write.csv(Affect_Passive,"/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_2709_24missing.csv")
#write.csv(Affect_Passive,"/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_2709_18missing.csv")
#write.csv(Affect_Passive,"/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_2709_12missing.csv")

