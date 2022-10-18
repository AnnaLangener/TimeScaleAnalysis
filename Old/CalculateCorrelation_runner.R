Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_220422.csv")
Affect_Passive <- Affect_Passive[Affect_Passive$questionListName != "Morning assessment 1" & Affect_Passive$questionListName != "Morning Assessment",]

### Bringing data to wide-format
calculate_runnner_dataset <- function(Affect_Passive, TIME = "3h"){

Features <- c('SOCIAL_min',
              'COMMUNICATION_min',
              'APP_USAGE_min',
              'APPS_OPENED_number',
              'Cluster_HOME_min',
              "Cluster_1_min",
              "Cluster_2_min",
              "Cluster_3_min",
              "Cluster_4_min",
              "Cluster_5_min",
              'UNIQUE_STAYPOINTS_number',
              'TIME_STATIONARY_min',
              'TOTAL_MACHASHES_number',
              'UNIQUE_MACHASHES_number',
              "BLUETOOTH_TOTAL_MACHASHES_number",
              "BLUETOOTH_UNIQUE_MACHASHES_number",
              'CALL_TOTAL_min',
              "CALL_incoming_min",
              "CALL_outgoing_min",
              "CALL_TOTAL_number",                                                   
              "CALL_incoming_number",                                                
              "CALL_outgoing_number",                                               
              "CALL_UNIQUE_CONTACTS_number",                                         
              "LIGHT_LUX_mean",                                                      
              "LIGHT_LUX_std",                                                       
              "SCREEN_onLocked_number" ,                                             
              "SCREEN_onUnlocked_number", 
              "SMS_received_number" ,                                                
              "SMS_sent_number" ,                                                    
              "SMS_UNIQUE_CONTACTS_number")


Affect_Passive_3h <- Affect_Passive[Affect_Passive$timescale_beforeESM == TIME,]
BEHAPP_ID = c(117113,117114,117119,117121,117130,117129,117131,117134,117135,117137)


for( n in 1:length(Features)){
counter = 0
for (i in 1:length(BEHAPP_ID)) {

  ParticipantNumber = BEHAPP_ID[i]
  for (k in c(2,3,4,5,6,7,8,9,10,11,12)) {
    counter = counter + 1
    
    rolling2 = runner(Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, Features[n]], k = k, function(x)
      var(x, na.rm = TRUE), na_pad = TRUE)
    
    rolling2 <- as.data.frame(rolling2)
    colnames(rolling2) <- Features[n]
    
    if (counter == 1) {
      data = data.frame(
        "Date" = Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, "Date"],
        "pa_mean" = Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, "pa_mean"],
        rolling2,
        "ParticipantNumber" = ParticipantNumber,
        "timescale_beforeESM" = k
      )
      
    } else{
      data = rbind(
        data,
        data.frame(
          "Date" = Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, "Date"],
          "pa_mean" = Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, "pa_mean"],
          rolling2,
          "ParticipantNumber" = ParticipantNumber,
          "timescale_beforeESM" = k
        )
      )
    }
  }
}
if(n == 1){
  Overalldata = data
  
}else{
  Overalldata <- merge(data, Overalldata)
}
}
return(Overalldata)
}

### Test function

test <- calculate_runnner_dataset(Affect_Passive)

test2 <- calculateCorr_pa(Affect_Passive = test)


## Bringing data to wide-format
calculate_runnner_dataset_passive <- function(Affect_Passive, TIME = "per hour"){
  
  Features <- c("CLUSTER_HOME_min")
  
  
  Affect_Passive_3h <- passive[passive$timescale_index ==TIME,]
  BEHAPP_ID = c(117113,117114,117119,117121,117130,117129,117131,117134,117135,117137)
  
  
  for( n in 1:length(Features)){
    counter = 0
    for (i in 1:length(BEHAPP_ID)) {
      
      ParticipantNumber = BEHAPP_ID[i]
      for (k in c(2,3,4,5,6,7,8,9,10,11,12)) {
        counter = counter + 1
        
        rolling2 = runner(Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, Features[n]], k = k, function(x)
          var(x, na.rm = TRUE), na_pad = TRUE)
        
        rolling2 <- as.data.frame(rolling2)
        colnames(rolling2) <- Features[n]
        
        if (counter == 1) {
          data = data.frame(
            "Date" = Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, "Date"],
            rolling2,
            "ParticipantNumber" = ParticipantNumber,
            "timescale_beforeESM" = k
          )
          
        } else{
          data = rbind(
            data,
            data.frame(
              "Date" = Affect_Passive_3h[Affect_Passive_3h$ParticipantNumber == ParticipantNumber, "Date"],
              rolling2,
              "ParticipantNumber" = ParticipantNumber,
              "timescale_beforeESM" = k
            )
          )
        }
      }
    }
    if(n == 1){
      Overalldata = data
      
    }else{
      Overalldata <- merge(data, Overalldata)
    }
  }
  return(Overalldata)
}

