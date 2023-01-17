library('dplyr')
library('ggplot2')
library('plotly')
library("runner")
library("hrbrthemes")
library("viridis")
library("ggpubr")
library("tidyr")
library("knitr")
library("dotwhisker")
library("stargazer")
library("lme4")


# This function is used to calculate the correlation for various features for different particpants and levels of aggregation.
# It was used to create the plots displayed in the supplementary material.


calculateCorr_pa <- function(Affect_Passive){ # Affect_Passive is dataset
  
  
# Create a subset of the dataset that includes the variables we are intrested in
  Affect_Passive_subset <- subset(Affect_Passive, select = c(ParticipantNumber, Date , pa_mean , timescale_beforeESM ,
                                              SOCIAL_min ,
                                              COMMUNICATION_min ,
                                              APP_USAGE_min ,
                                              APPS_OPENED_number ,
                                              Cluster_HOME_min ,
                                              Cluster_1_min ,
                                              Cluster_2_min ,
                                              Cluster_3_min ,
                                              Cluster_4_min ,
                                              Cluster_5_min ,
                                              UNIQUE_STAYPOINTS_number ,
                                              TIME_STATIONARY_min ,
                                              TOTAL_MACHASHES_number ,
                                              UNIQUE_MACHASHES_number ,
                                              BLUETOOTH_TOTAL_MACHASHES_number ,
                                              BLUETOOTH_UNIQUE_MACHASHES_number ,
                                              CALL_TOTAL_min ,
                                              CALL_incoming_min ,
                                              CALL_outgoing_min ,
                                              CALL_TOTAL_number ,                                                   
                                              CALL_incoming_number ,                                                
                                              CALL_outgoing_number ,                                               
                                              CALL_UNIQUE_CONTACTS_number ,                                         
                                              LIGHT_LUX_mean ,                                                      
                                              LIGHT_LUX_std ,                                                       
                                              SCREEN_onLocked_number  ,                                             
                                              SCREEN_onUnlocked_number , 
                                              SMS_received_number  ,                                                
                                              SMS_sent_number  ,                                                    
                                              SMS_UNIQUE_CONTACTS_number ))
  
 Affect_Passive_subset$timescale_beforeESM_num <- recode(Affect_Passive_subset$timescale_beforeESM, "1h" = 1, "3h" = 3,"6h" = 6,"9h" = 9,"12h" = 12, "24h" = 24 )
  
# transform the dataset to wide format
  wide_orginal = Affect_Passive_subset %>% 
    gather('SOCIAL_min',
           'COMMUNICATION_min',
           'APP_USAGE_min',
           'APPS_OPENED_number',
           'Cluster_HOME_min',
           "Cluster_1_min",
           "Cluster_2_min",
           "Cluster_3_min",
           "Cluster_4_min",
           "Cluster_5_min" ,
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
           "SMS_UNIQUE_CONTACTS_number" ,
           key = variable, value = number) %>% 
    unite(combi, variable,timescale_beforeESM ) %>% 
    spread(combi, number)
  

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
  

###### POSITIVE AFFECT

counter = 0

# loop through all features on different timescales and calculate the correlation
  for (i in 1:length(Features)) {
    Feature <- Features[i]
    
    for (timescale in unique(Affect_Passive$timescale_beforeESM)) {
      counter = counter + 1

        # Save true correlation once
        wide_orginal[, "Passivecopy"] <-
          wide_orginal[, colnames(wide_orginal) == paste(Feature,"_", timescale, sep = "")]
        
        CorrelationTrue = wide_orginal %>%
          group_by(ParticipantNumber) %>%
          summarize(
            "Feature" = Feature,
            "timescale_beforeESM" = timescale,
            "Correlation" = ifelse(
              sum(Passivecopy, na.rm = TRUE) != 0,
              as.numeric(cor.test(as.formula(
                paste("~ pa_mean + ", Feature,"_", timescale, sep = "")
              ))$estimate),
              NA
            ),
            "p.value" = ifelse(
              sum(Passivecopy, na.rm = TRUE) != 0,
              round(cor.test(as.formula(
                paste("~ pa_mean + ", Feature,"_", timescale, sep = "")
              ))$p.value, 2),
              NA
            ),
            "Size" = "thin"
          )
        
        CorrelationTrue$ParticipantNumber <-
          as.character(CorrelationTrue$ParticipantNumber)
        
        OverallCorrelationTrue = wide_orginal %>%
          summarize(
            "Feature" = Feature,
            "timescale_beforeESM" = timescale,
            "ParticipantNumber" = "Overall",
            Correlation = as.numeric(cor.test(as.formula(
              paste(
                "~ wide_orginal$pa_mean + wide_orginal$",
                Feature,"_",
                timescale,
                sep = ""
              )
            ))$estimate),
            "p.value" = round(cor.test(as.formula(
              paste(
                "~ wide_orginal$pa_mean + wide_orginal$",
                Feature,"_",
                timescale,
                sep = ""
              )
            ))$p.value, 2),
            "Size" = "thick"
          )
        
        
        if (counter == 1) {
          CorrelationSampleTrue = rbind(CorrelationTrue, OverallCorrelationTrue)
        } else{
          CorrelationSampleTrue = rbind(CorrelationSampleTrue,
                                        CorrelationTrue,
                                        OverallCorrelationTrue)
          
        }
        
      }
  }


# Add things for plot

CorrelationSampleTrue$sign <- "non-sign"
CorrelationSampleTrue[!is.na(CorrelationSampleTrue$p.value) &
                           CorrelationSampleTrue$p.value <= 0.05, ]$sign <- "sign"

CorrelationSampleTrue$star = ""
CorrelationSampleTrue[!is.na(CorrelationSampleTrue$p.value) &
                           CorrelationSampleTrue$p.value <= 0.05, ]$star = "*"

CorrelationSampleTrue$timescale_beforeESM_num <- recode(CorrelationSampleTrue$timescale_beforeESM,"1h" = 1, "3h" = 3,"6h" = 6,"9h" = 9,"12h" = 12, "24h" = 24 )

CorrelationSampleTrue$timescale_beforeESM <- ordered(CorrelationSampleTrue$timescale_beforeESM,levels = c("1h","3h","6h","9h","12h","24h"))

# return the results

return(CorrelationSampleTrue)

}
