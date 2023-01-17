##########################################
##### Data cleaning overall Dataset ######
##########################################
#hi
library(dplyr)
library(tidyr)

#Read the raw data (Data) and the order in which the questionnaires were filled out (Order)
Data <- read.csv2('/Users/annalangener/Nextcloud/Shared/Testing Methods to Capture Social Context/Cleaning ESM/basic (1).csv',na.strings = c("NA",""))
Order <- read.csv2('/Users/annalangener/Nextcloud/Shared/Testing Methods to Capture Social Context/Cleaning ESM/order (1).csv',na.strings = c("NA",""))
#sum(!is.na(Data$timeStampStart)) #4195
#The ordered dataset has less observations, because questionnaires that the participants did not start do no have an order

#Join with ordered data for the interact_partner item (we will remove those variables later)
Data <- left_join(Data, Order, by = c('connectionId','alias','sentBeepId','timeStampStart','timeStampStop','questionListName'), suffix = c('','.order'))

######## General Data Cleaning ###########
##########################################

#Remove practice questionnaires and participants
Data <- Data[Data$alias != "AnnaL", ]
#Remove practice questionnaires and participants
Data <- Data[Data$alias != "MaSt", ]

# Diana had the wrong button, thus I renamed all but the first one and copied the names to the correct item
# Diana could only add one new person, per interaction (because of the wrong button)
Data$questionListName[Data$questionListName == "Interaction Practice" & Data$alias == "Diana (107)"][-1] <- 'Interaction Assessment'
Data$questionListName[Data$questionListName == "Interaction Practice" & Data$alias == "Diana (107)"][-1] <- 'Interaction Assessment'
Data$interact_partner_multipleChoice_string[Data$questionListName == "Interaction Assessment" & Data$alias == "Diana (107)"] <- Data$interact_partner1_multipleChoice_string[Data$questionListName == "Interaction Assessment" & Data$alias == "Diana (107)"]
Data$interact_partner1_multipleChoice_string[Data$questionListName == "Interaction Assessment" & Data$alias == "Diana (107)"] <- NA

#remove practice questionnaires
Data <- Data[Data$questionListName != "Interaction Practice" & Data$questionListName != "Daily Practice" & Data$questionListName != "Add an interaction 🥳", ]

#remove all columns that only contain NA
omit = c()
for (i in 1:ncol(Data)){
if (length(na.omit(Data[,i])) == 0){
  name <- colnames(Data)[i]
  omit = c(omit, name)
}
}

Data <- Data[!(colnames(Data) %in% omit)]

#Combine na_irritated (because it had two different names)
na_irritate_sliderNegPos <- rowSums(data.frame(Data$na_irritate_sliderNegPos, Data$NA_irritate_sliderNegPos), na.rm = TRUE)
na_irritate_sliderNegPos[is.na(Data$na_irritate_sliderNegPos) & is.na(Data$NA_irritate_sliderNegPos)] <- NA
Data$na_irritate_sliderNegPos <- na_irritate_sliderNegPos

# remove unnecessary variables
Data <- Data[!(colnames(Data) %in% c('scheduledBeepId', 'sentBeepId', 'button_count', 'NA_irritate_sliderNegPos'))]

#remove multiple choice index for interaction partner
Data <- Data[!(colnames(Data) %in% c('interact_partner_multipleChoice_index','interact_partner_multipleChoice_index_1','interact_partner_multipleChoice_index_2', 'interact_partner_multipleChoice_index_3', 'interact_partner_multipleChoice_index_4', 'interact_partner_multipleChoice_index_5', 'interact_partner_multipleChoice_index_6', 'interact_partner_multipleChoice_index_7','interact_partner_multipleChoice_index_8'))]

#Combine other_new_yesno to one variable (just take the sum, so we know how many other people where added)
Data['other_new_yesno'] <- rowSums(Data[colnames(Data) %in% c('other_new_1_yesno','other_new_1_yesno_1','other_new_1_yesno_2','other_new_2_yesno','other_new_2_yesno_1','other_new_2_yesno_2','other_new_3_yesno', 'other_new_4_yesno', 'other_new_5_yesno','other_new_6_yesno', 'other_new_7_yesno', 'other_new_8_yesno')], na.rm = TRUE)
Data <- Data[!(colnames(Data) %in% c('other_new_1_yesno','other_new_1_yesno_1','other_new_1_yesno_2','other_new_2_yesno','other_new_2_yesno_1','other_new_2_yesno_2','other_new_3_yesno', 'other_new_4_yesno', 'other_new_5_yesno','other_new_6_yesno', 'other_new_7_yesno', 'other_new_8_yesno'))]

#Combine add_interact_yesno to one variable (just take the sum, so we know how many interactions were backlogged)
Data['add_interact_yesno'] <- rowSums(Data[colnames(Data) %in% c('add_interact_1_yesno','add_interact_2_yesno','add_interact_3_yesno')], na.rm = TRUE)
Data <- Data[!(colnames(Data) %in% c('add_interact_1_yesno','add_interact_2_yesno','add_interact_3_yesno'))]

#### Remove variables that probably occur because of the button back

#length(na.omit(Data$alone_multipleChoice_index_1)) #1 (104) had this item two times in the evening questionnaire
#length(na.omit(Data$alone_multipleChoice_index_2))
#length(na.omit(Data$pa_relax_sliderNegPos_1))
#length(na.omit(Data$pa_relax_sliderNegPos_2))
#length(na.omit(Data$activity_open_1)) #1: "chillin" but the same is written in activity_open

Data <- Data[!(colnames(Data) %in% c('activity_open_1','alone_multipleChoice_index_1', 'alone_multipleChoice_index_2','alone_multipleChoice_string_1', 'alone_multipleChoice_string_2','pa_relax_sliderNegPos_1','pa_relax_sliderNegPos_2'))]

########### Interaction Partner #############
#############################################
# The following code checks to which interaction the interact_partner item belongs
# We check whether the interact_partner items are answered before or after a new interaction is backlogged or the online interaction item is filled in.
# Thus we know if it belong to adding a new interaction partner or a new interaction.

Data$interact_online_yesno.order[is.na(Data$interact_online_yesno.order)] <- 10000 # I set this here to 1000 so that non-evening questionnaires still fulfill the comparison

##### First Backlog Interaction
Data$interact_partner_firstint <- NA
firstint <- which(Data$interact_partner_multipleChoice_string.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- Data$interact_partner_multipleChoice_string[firstint]

firstint <- which(Data$interact_partner_multipleChoice_string_1.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_1.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_1'),sep = ",", na.rm = TRUE)$interact_partner_firstint

firstint <- which(Data$interact_partner_multipleChoice_string_2.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_2.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_2'),sep = ",", na.rm = TRUE)$interact_partner_firstint

firstint <- which(Data$interact_partner_multipleChoice_string_3.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_3.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_3'),sep = ",", na.rm = TRUE)$interact_partner_firstint

firstint <- which(Data$interact_partner_multipleChoice_string_4.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_4.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_4'),sep = ",", na.rm = TRUE)$interact_partner_firstint

firstint <- which(Data$interact_partner_multipleChoice_string_5.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_5.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_5'),sep = ",", na.rm = TRUE)$interact_partner_firstint

firstint <- which(Data$interact_partner_multipleChoice_string_6.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_6.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_6'),sep = ",", na.rm = TRUE)$interact_partner_firstint

firstint <- which(Data$interact_partner_multipleChoice_string_7.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_7.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_7'),sep = ",", na.rm = TRUE)$interact_partner_firstint

firstint <- which(Data$interact_partner_multipleChoice_string_8.order < Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string_8.order < Data$interact_online_yesno.order) #1st interaction
Data$interact_partner_firstint[firstint] <- unite(Data[firstint,],'interact_partner_firstint',c('interact_partner_firstint','interact_partner_multipleChoice_string_8'),sep = ",", na.rm = TRUE)$interact_partner_firstint

##### Second Backlog Interaction
Data$interact_partner_secondint <- NA
secondint <- which(Data$interact_partner_multipleChoice_string.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- Data$`interact_partner_multipleChoice_string `[secondint]

secondint <- which(Data$interact_partner_multipleChoice_string_1.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_1.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_1'),sep = ",", na.rm = TRUE)$interact_partner_secondint

secondint <- which(Data$interact_partner_multipleChoice_string_2.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_2.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_2'),sep = ",", na.rm = TRUE)$interact_partner_secondint

secondint <- which(Data$interact_partner_multipleChoice_string_3.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_3.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_3'),sep = ",", na.rm = TRUE)$interact_partner_secondint

secondint <- which(Data$interact_partner_multipleChoice_string_4.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_4.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_4'),sep = ",", na.rm = TRUE)$interact_partner_secondint

secondint <- which(Data$interact_partner_multipleChoice_string_5.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_5.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_5'),sep = ",", na.rm = TRUE)$interact_partner_secondint

secondint <- which(Data$interact_partner_multipleChoice_string_6.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_6.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_6'),sep = ",", na.rm = TRUE)$interact_partner_secondint

secondint <- which(Data$interact_partner_multipleChoice_string_7.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_7.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_7'),sep = ",", na.rm = TRUE)$interact_partner_secondint

secondint <- which(Data$interact_partner_multipleChoice_string_8.order > Data$add_interact_2_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_8.order < Data$interact_online_yesno.order) 
Data$interact_partner_secondint[secondint] <- unite(Data[secondint,],'interact_partner_secondint',c('interact_partner_secondint','interact_partner_multipleChoice_string_8'),sep = ",", na.rm = TRUE)$interact_partner_secondint

##### Third Backlog Interaction
Data$interact_partner_thirdint <- NA
thirdint <- which(Data$interact_partner_multipleChoice_string.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- Data$`interact_partner_multipleChoice_string `[thirdint]

thirdint <- which(Data$interact_partner_multipleChoice_string_1.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_1.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_1'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

thirdint <- which(Data$interact_partner_multipleChoice_string_2.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_2.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_2'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

thirdint <- which(Data$interact_partner_multipleChoice_string_3.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_3.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_3'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

thirdint <- which(Data$interact_partner_multipleChoice_string_4.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_4.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_4'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

thirdint <- which(Data$interact_partner_multipleChoice_string_5.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_5.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_5'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

thirdint <- which(Data$interact_partner_multipleChoice_string_6.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_6.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_6'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

thirdint <- which(Data$interact_partner_multipleChoice_string_7.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_7.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_7'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

thirdint <- which(Data$interact_partner_multipleChoice_string_8.order > Data$add_interact_3_yesno.order & Data$interact_partner_multipleChoice_string_8.order < Data$interact_online_yesno.order) 
Data$interact_partner_thirdint[thirdint] <- unite(Data[thirdint,],'interact_partner_thirdint',c('interact_partner_thirdint','interact_partner_multipleChoice_string_8'),sep = ",", na.rm = TRUE)$interact_partner_thirdint

##### Online Interaction
Data$interact_partner_onlineint <- NA
onlineint <- which(Data$interact_partner_multipleChoice_string.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- Data$`interact_partner_multipleChoice_string`[onlineint]

onlineint <- which(Data$interact_partner_multipleChoice_string_1.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_1'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

onlineint <- which(Data$interact_partner_multipleChoice_string_2.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_2'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

onlineint <- which(Data$interact_partner_multipleChoice_string_3.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_3'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

onlineint <- which(Data$interact_partner_multipleChoice_string_4.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_4'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

onlineint <- which(Data$interact_partner_multipleChoice_string_5.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_5'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

onlineint <- which(Data$interact_partner_multipleChoice_string_6.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_6'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

onlineint <- which(Data$interact_partner_multipleChoice_string_7.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_7'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

onlineint <- which(Data$interact_partner_multipleChoice_string_8.order > Data$interact_online_yesno.order) 
Data$interact_partner_onlineint[onlineint] <- unite(Data[onlineint,],'interact_partner_onlineint',c('interact_partner_onlineint','interact_partner_multipleChoice_string_8'),sep = ",", na.rm = TRUE)$interact_partner_onlineint

Data <- Data[,-c(97:244)]

##### Create new Questionnaire type: backlog interaction ######
###############################################################

# Each backlogged interaction will appear as separate Questionnaire type, this makes it easier to analyse and understand the data
# I'm sure that there is an easier way to summarize this, but I didn't know how and still wanted it to work if the variable order changes

# Loop through each row of the dataset and check if an interaction was backlogged
# If one interaction is backlogged we will create a new row and add it to the dataset

for(i in 1:nrow(Data)){
  if(!is.na(Data$add_interact_yesno[i]) && Data$add_interact_yesno[i] >= 1) {
    
    interact_partner_multipleChoice_string =  Data$interact_partner_firstint[i]
    
    NewRow <-  data.frame(#copy standard variables  
                          connectionId = Data$connectionId[i], alias = Data$alias[i], reminderForOriginalSentBeepId = Data$reminderForOriginalSentBeepId[i],
                          questionListName = 'Backlog Interaction', timeStampScheduled = Data$timeStampScheduled[i], timeStampSent = Data$timeStampSent[i],
                          timeStampStart = Data$timeStampStart[i], timeStampStop = Data$timeStampStop[i], originalTimeStampSent = Data$originalTimeStampSent[i], add_interact_yesno = Data$add_interact_yesno[i],
                          other_new_yesno = Data$other_new_yesno[i],
                          #copy affect items (but we have to keep in mind that these are the daily questionnaire ones)
                          pa_relax_sliderNegPos = Data$pa_relax_sliderNegPos[i], pa_happy_sliderNegPos = Data$pa_happy_sliderNegPos[i], pa_energy_sliderNegPos = Data$pa_energy_sliderNegPos[i] ,
                          na_anx_sliderNegPos = Data$na_anx_sliderNegPos[i], na_irritate_sliderNegPos = Data$na_irritate_sliderNegPos[i], na_sad_sliderNegPos = Data$na_sad_sliderNegPos[i],
                          na_stress_sliderNegPos = Data$na_stress_sliderNegPos[i],
                          # copy interaction items
                          interact_partner_multipleChoice_string = interact_partner_multipleChoice_string, interact_time_multipleChoice_index = Data$interact_time_multipleChoice_index[i],interact_time_multipleChoice_string = Data$interact_time_multipleChoice_string[i],
                          interact_mode_multipleChoice_index = Data$interact_mode_multipleChoice_index[i], interact_mode_multipleChoice_string = Data$interact_mode_multipleChoice_string[i],
                          interact_dur_multipleChoice_index = Data$interact_dur_multipleChoice_index[i], interact_dur_multipleChoice_string =Data$interact_dur_multipleChoice_string[i],
                          interact_cont_open = Data$interact_cont_open[i],interact_enjoy_sliderNegPos = Data$interact_enjoy_sliderNegPos[i],interact_other_enjoy_sliderNegPos =  Data$interact_other_enjoy_sliderNegPos[i],
                          interact_meaning_sliderNegPos = Data$interact_meaning_sliderNegPos[i], interact_myself_sliderNegPos = Data$interact_myself_sliderNegPos[i], interact_energy_give_sliderNegPos =
                          Data$interact_energy_give_sliderNegPos[i], interact_energy_cost_sliderNegPos = Data$interact_energy_cost_sliderNegPos[i])
   
     Data <- full_join(Data, NewRow)
  } 
  
  if(!is.na(Data$add_interact_yesno[i]) && Data$add_interact_yesno[i] >= 2) {
    interact_partner_multipleChoice_string =  Data$interact_partner_secondint[i]
    
    #same as above 
    NewRow1 <- data.frame(connectionId = Data$connectionId[i], alias = Data$alias[i], reminderForOriginalSentBeepId = Data$reminderForOriginalSentBeepId[i],
      questionListName = 'Backlog Interaction', timeStampScheduled = Data$timeStampScheduled[i], timeStampSent = Data$timeStampSent[i],
      timeStampStart = Data$timeStampStart[i], timeStampStop = Data$timeStampStop[i], originalTimeStampSent = Data$originalTimeStampSent[i], add_interact_yesno = Data$add_interact_yesno[i],
      other_new_yesno = Data$other_new_yesno[i],  pa_relax_sliderNegPos = Data$pa_relax_sliderNegPos[i], pa_happy_sliderNegPos = Data$pa_happy_sliderNegPos[i], pa_energy_sliderNegPos = Data$pa_energy_sliderNegPos[i] ,
      na_anx_sliderNegPos = Data$na_anx_sliderNegPos[i], na_irritate_sliderNegPos = Data$na_irritate_sliderNegPos[i], na_sad_sliderNegPos = Data$na_sad_sliderNegPos[i],
      na_stress_sliderNegPos = Data$na_stress_sliderNegPos[i],interact_time_multipleChoice_index = Data$interact_time_multipleChoice_index_1[i],interact_partner_multipleChoice_string = interact_partner_multipleChoice_string, interact_time_multipleChoice_string = Data$interact_time_multipleChoice_string_1[i],
      interact_mode_multipleChoice_index = Data$interact_mode_multipleChoice_index_1[i], interact_mode_multipleChoice_string = Data$interact_mode_multipleChoice_string_1[i],
      interact_dur_multipleChoice_index = Data$interact_dur_multipleChoice_index_1[i], interact_dur_multipleChoice_string =Data$interact_dur_multipleChoice_string_1[i],
      interact_cont_open = Data$interact_cont_open_1[i],interact_enjoy_sliderNegPos = Data$interact_enjoy_sliderNegPos_1[i],interact_other_enjoy_sliderNegPos =  Data$interact_other_enjoy_sliderNegPos_1[i],
      interact_meaning_sliderNegPos = Data$interact_meaning_sliderNegPos_1[i], interact_myself_sliderNegPos =Data$interact_myself_sliderNegPos_1[i], interact_energy_give_sliderNegPos =
      Data$interact_energy_give_sliderNegPos_1[i], interact_energy_cost_sliderNegPos = Data$interact_energy_cost_sliderNegPos_1[i])
    Data <- full_join(Data, NewRow1)
    

  } 
  if(!is.na(Data$add_interact_yesno[i]) & Data$add_interact_yesno[i] >= 3){
    interact_partner_multipleChoice_string =  Data$interact_partner_thirdint[i]
    
    NewRow2 <-  data.frame(connectionId = Data$connectionId[i], alias = Data$alias[i], reminderForOriginalSentBeepId = Data$reminderForOriginalSentBeepId[i],
                 questionListName = 'Backlog Interaction', timeStampScheduled = Data$timeStampScheduled[i], timeStampSent = Data$timeStampSent[i],
                 timeStampStart = Data$timeStampStart[i], timeStampStop = Data$timeStampStop[i], originalTimeStampSent = Data$originalTimeStampSent[i], add_interact_yesno = Data$add_interact_yesno[i],
                 other_new_yesno = Data$other_new_yesno[i],
                 pa_relax_sliderNegPos = Data$pa_relax_sliderNegPos[i], pa_happy_sliderNegPos = Data$pa_happy_sliderNegPos[i], pa_energy_sliderNegPos = Data$pa_energy_sliderNegPos[i] ,
                 na_anx_sliderNegPos = Data$na_anx_sliderNegPos[i], na_irritate_sliderNegPos = Data$na_irritate_sliderNegPos[i], na_sad_sliderNegPos = Data$na_sad_sliderNegPos[i],
                 na_stress_sliderNegPos = Data$na_stress_sliderNegPos[i],interact_partner_multipleChoice_string = interact_partner_multipleChoice_string,
                 interact_time_multipleChoice_index = Data$interact_time_multipleChoice_index_2[i],interact_time_multipleChoice_string = Data$interact_time_multipleChoice_string_2[i],
                 interact_mode_multipleChoice_index = Data$interact_mode_multipleChoice_index_2[i], interact_mode_multipleChoice_string = Data$interact_mode_multipleChoice_string_2[i],
                 interact_dur_multipleChoice_index = Data$interact_dur_multipleChoice_index_2[i], interact_dur_multipleChoice_string =Data$interact_dur_multipleChoice_string_2[i],
                 interact_cont_open = Data$interact_cont_open_2[i],interact_enjoy_sliderNegPos = Data$interact_enjoy_sliderNegPos_2[i],interact_other_enjoy_sliderNegPos =  Data$interact_other_enjoy_sliderNegPos_2[i],
                 interact_meaning_sliderNegPos = Data$interact_meaning_sliderNegPos_2[i], interact_myself_sliderNegPos =Data$interact_myself_sliderNegPos_2[i], interact_energy_give_sliderNegPos =
                   Data$interact_energy_give_sliderNegPos_2[i], interact_energy_cost_sliderNegPos = Data$interact_energy_cost_sliderNegPos_2[i])
    Data <- full_join(Data, NewRow2)

  }
}

#### Omit the variables that occured because of the backlogged interaction
# Each is now stored in the same as the ones in the "normal" interaction assessment

omit <- c('interact_time_multipleChoice_index_2', 'interact_time_multipleChoice_string_2', 'interact_mode_multipleChoice_index_2','interact_mode_multipleChoice_string_2','interact_string_multipleChoice_index_2', 'interact_dur_multipleChoice_index_2', 
          'interact_dur_multipleChoice_string_2','interact_cont_open_2','interact_enjoy_sliderNegPos_2','interact_other_enjoy_sliderNegPos_2',
          'interact_meaning_sliderNegPos_2', 'interact_myself_sliderNegPos_2', 'interact_energy_give_sliderNegPos_2', 'interact_energy_cost_sliderNegPos_2')

Data <- Data[!(colnames(Data) %in% omit)]

omit <- c('interact_time_multipleChoice_index_1', 'interact_time_multipleChoice_string_1', 'interact_mode_multipleChoice_index_1','interact_string_multipleChoice_index_1','interact_mode_multipleChoice_string_1', 'interact_dur_multipleChoice_index_1', 
          'interact_dur_multipleChoice_string_1','interact_cont_open_1','interact_enjoy_sliderNegPos_1','interact_other_enjoy_sliderNegPos_1',
          'interact_meaning_sliderNegPos_1', 'interact_myself_sliderNegPos_1', 'interact_energy_give_sliderNegPos_1', 'interact_energy_cost_sliderNegPos_1')

Data <- Data[!(colnames(Data) %in% omit)]

omit <- c('interact_partner_multipleChoice_string_1','interact_partner_multipleChoice_string_2','interact_partner_multipleChoice_string_3','interact_partner_multipleChoice_string_4','interact_partner_multipleChoice_string_5','interact_partner_multipleChoice_string_6','interact_partner_multipleChoice_string_7','interact_partner_multipleChoice_string_8')
Data <- Data[!(colnames(Data) %in% omit)]

omit <- c('interact_partner_multipleChoice_index_1','interact_partner1_multipleChoice_index','interact_partner1_multipleChoice_index_1','interact_partner_multipleChoice_index_2','interact_partner_multipleChoice_index_3','interact_partner_multipleChoice_index_4','interact_partner_multipleChoice_index_5','interact_partner_multipleChoice_index_6','interact_partner_multipleChoice_index_7','interact_partner_multipleChoice_index_8', 'interact_partner_firstint', 'interact_partner_secondint','interact_partner_thirdint')
Data <- Data[!(colnames(Data) %in% omit)]

# We have each backlogged interaction as new questionnaire type. Thus we can label those variables to NA in the daily quesionnaire (otherwise it would be double included)

NA_labeling <- c('interact_partner_multipleChoice_string', 'interact_time_multipleChoice_index', 'interact_time_multipleChoice_string', 'interact_mode_multipleChoice_index','interact_mode_multipleChoice_string', 'interact_dur_multipleChoice_index', 
          'interact_dur_multipleChoice_string','interact_cont_open','interact_enjoy_sliderNegPos','interact_other_enjoy_sliderNegPos',
          'interact_meaning_sliderNegPos', 'interact_myself_sliderNegPos', 'interact_energy_give_sliderNegPos', 'interact_energy_cost_sliderNegPos')

Data[Data$questionListName != "Interaction Assessment" & Data$questionListName != "Backlog Interaction", (colnames(Data) %in% NA_labeling)] <- NA


####### SAVE DATA ########
####IMPORTANT
# This dataset will include a new questionnaire type "Backlogged interaction". This type has the same variables as the regular "Interaction Assessment". Please take in mind that the affect items were not measured directly after/before the interaction.
# Affect items are now sometimes double (or even more often) included!!  In the "Backlogged interaction" & in the "Daily Assessment/ Evening Assessment/ Morning Assessment"
# Questionnaires besides "Interaction Assessment" and "Backlogged interaction" do not contain the interaction variables anymore
# Please note that Diana (107) had the wrong, thus she could only add one partner per interaction assessment.

writexl::write_xlsx(Data, '/Users/annalangener/Nextcloud/Shared/Testing Methods to Capture Social Context/Cleaning ESM/DataCleaned.xlsx')
write.csv(Data, '/Users/annalangener/Nextcloud/Shared/Testing Methods to Capture Social Context/Cleaning ESM/DataCleaned.csv')
