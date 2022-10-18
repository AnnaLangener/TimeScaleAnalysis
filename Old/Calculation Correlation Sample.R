Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_130422.csv")
Affect_Passive <- Affect_Passive[Affect_Passive$questionListName != "Morning assessment 1" & Affect_Passive$questionListName != "Morning Assessment",]

# 
# 
# Webb, S. S., & Demeyere, N. (2021, July 21). Multiverse Analysis: A Method to Determine Researcher Degrees of Freedom in Test Validation. https://doi.org/10.31234/osf.io/nhrwq
# 
# "Following Simonsohn et al. (2020), the p-value for significance of a difference between medians from the observed and null effects multiverse was extracted by calculating the proportion of the 500 permutated null analyses which had a test statistic that was the exact same median value or greater as in the observed data, and divided that by two (significant if below.<05 or if no null values are equal or greater we report p <.002). For this median comparison we rounded median coefficients to two decimal places to maximize the chance of matching medians rather than inflating the risk of non-matching medians if they are >.001 decimal place out."
# 
# - Bring data in wide format
# - Shuffle outcome variable with replacement across columns
# - check if there is a sign. result for 3h,6h,9h,12h
# 
# 
# Other strategy:
#   - Simulate Data under H0 (ysim = y-correlation coefficient), shuffle dataset with replacement
# 
# 
# How can correlation be missing on broader time scales?
#   - bluetooth connections do always have the same value --> 1 and only once 0 (rest is labeled as missing)

### Bringing data to wide-format

Affect_Passive_subset <- subset(Affect_Passive,
                                select = c("ParticipantNumber","Date","na_mean","pa_mean","timescale_beforeESM",
                                           'SOCIAL_min',
                                           'COMMUNICATION_min',
                                           "HEALTH_FITNESS_min",
                                           "ENTERTAINMENT_min",   
                                           'APP_USAGE_min',
                                           'APPS_OPENED_number',
                                           'Cluster_HOME_min',
                                           "Cluster_0_min",
                                           "Cluster_1_min",
                                           "Cluster_2_min",
                                           "Cluster_3_min",
                                           "Cluster_4_min",
                                           'UNIQUE_STAYPOINTS_number',
                                           'TIME_STATIONARY_min',
                                           'TOTAL_MACHASHES_number',
                                           'UNIQUE_MACHASHES_number',
                                           "BLUETOOTH_TOTAL_MACHASHES_number",
                                           "BLUETOOTH_UNIQUE_MACHASHES_number",
                                           'CALL_TOTAL_min',
                                           "CALL_incoming_min",
                                           "CALL_outgoing_min",
                                           "MISSED_CALLS_number",
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
                                           "SMS_UNIQUE_CONTACTS_number"))

wide_orginal = Affect_Passive_subset %>% 
  gather('SOCIAL_min',
         'COMMUNICATION_min',
         "HEALTH_FITNESS_min",
         "ENTERTAINMENT_min",   
         'APP_USAGE_min',
         'APPS_OPENED_number',
         'Cluster_HOME_min',
         "Cluster_0_min",
         "Cluster_1_min",
         "Cluster_2_min",
         "Cluster_3_min",
         "Cluster_4_min",
         'UNIQUE_STAYPOINTS_number',
         'TIME_STATIONARY_min',
         'TOTAL_MACHASHES_number',
         'UNIQUE_MACHASHES_number',
         "BLUETOOTH_TOTAL_MACHASHES_number",
         "BLUETOOTH_UNIQUE_MACHASHES_number",
         'CALL_TOTAL_min',
         "CALL_incoming_min",
         "CALL_outgoing_min",
         "MISSED_CALLS_number",
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
  unite(combi, variable, timescale_beforeESM) %>% 
  spread(combi, number)

set.seed(12361488)

Features <- c('SOCIAL_min',
              'COMMUNICATION_min',
              'APP_USAGE_min',
              'APPS_OPENED_number',
              'Cluster_HOME_min',
              "Cluster_0_min",
              "Cluster_1_min",
              "Cluster_2_min",
              "Cluster_3_min",
              "Cluster_4_min",
              'UNIQUE_STAYPOINTS_number',
              'TIME_STATIONARY_min',
              'TOTAL_MACHASHES_number',
              'UNIQUE_MACHASHES_number',
              "BLUETOOTH_TOTAL_MACHASHES_number",
              "BLUETOOTH_UNIQUE_MACHASHES_number",
              'CALL_TOTAL_min',
              "CALL_incoming_min",
              "CALL_outgoing_min",
              "MISSED_CALLS_number",
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

counter = 0
k = 0

while(k < 10) {
  print(k)
  k = k + 1
  # Shuffle data sets
  wide_sample <- wide_orginal
  wide_sample$pa_mean <-
    wide_sample$pa_mean[sample(nrow(wide_orginal), replace = TRUE)]
  
  
  for (i in 1:length(Features)) {
    Feature <- Features[i]
    
    for (timescale in c("1h","3h", "6h", "9h", "12h","24h")) {
      counter = counter + 1
      wide_sample[, "Passivecopy"] <-
        wide_sample[, colnames(wide_sample) == paste(Feature,"_", timescale, sep = "")]
      
      CorrelationSample = wide_sample %>%
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
          "sample" = k,
          "Size" = "thin"
        )
      
      CorrelationSample$ParticipantNumber <-
        as.character(CorrelationSample$ParticipantNumber)
      
      OverallCorrelationSample = wide_sample %>%
        summarize(
          "Feature" = Feature,
          "timescale_beforeESM" = timescale,
          "ParticipantNumber" = "Overall",
          Correlation = as.numeric(cor.test(as.formula(
            paste(
              "~ wide_sample$pa_mean + wide_sample$",
              Feature,"_",
              timescale,
              sep = ""
            )
          ))$estimate),
          "p.value" = round(cor.test(as.formula(
            paste(
              "~ wide_sample$pa_mean + wide_sample$",
              Feature,"_",
              timescale,
              sep = ""
            )
          ))$p.value, 2),
          "sample" = k,
          "Size" = "thick"
        )
      if (counter == 1) {
        CorrelationSampleCombined = rbind(CorrelationSample, OverallCorrelationSample)
      } else{
        CorrelationSampleCombined = rbind(CorrelationSampleCombined,
                                          CorrelationSample,
                                          OverallCorrelationSample)
        
      }
      if (k == 1) {
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
  }
}


#write.csv(CorrelationSampleTrue,"CorrelationSampleTrue.csv")
#write.csv(CorrelationSampleCombined,"CorrelationSampleCombined.csv")

set.seed(12361488)

Features <- c(
  'SOCIAL_min',
  'COMMUNICATION_min',
  'APP_USAGE_min',
  'APPS_OPENED_number',
  'Cluster_HOME_min',
  'UNIQUE_STAYPOINTS_number',
  'TIME_STATIONARY_min',
  'TOTAL_MACHASHES_number',
  'UNIQUE_MACHASHES_number',
  "BLUETOOTH_TOTAL_MACHASHES_number",
  "BLUETOOTH_UNIQUE_MACHASHES_number",
  'CALL_TOTAL_min',
  "CALL_incoming_min",
  "CALL_outgoing_min")

counter = 0
k = 0

while(k < 2) {
  print(k)
  k = k + 1
  # Shuffle data sets
  wide_sample <- wide_orginal
  wide_sample$na_mean <-
    wide_sample$na_mean[sample(nrow(wide_orginal), replace = TRUE)]
  
  
  for (i in 1:length(Features)) {
    Feature <- Features[i]
    
    for (timescale in c("1h","3h", "6h", "9h", "12h","24h")) {
      counter = counter + 1
      wide_sample[, "Passivecopy"] <-
        wide_sample[, colnames(wide_sample) == paste(Feature,"_", timescale, sep = "")]
      
      CorrelationSample = wide_sample %>%
        group_by(ParticipantNumber) %>%
        summarize(
          "Feature" = Feature,
          "timescale_beforeESM" = timescale,
          "Correlation" = ifelse(
            sum(Passivecopy, na.rm = TRUE) != 0,
            as.numeric(cor.test(as.formula(
              paste("~ na_mean + ", Feature,"_", timescale, sep = "")
            ))$estimate),
            NA
          ),
          "p.value" = ifelse(
            sum(Passivecopy, na.rm = TRUE) != 0,
            round(cor.test(as.formula(
              paste("~ na_mean + ", Feature,"_", timescale, sep = "")
            ))$p.value, 2),
            NA
          ),
          "sample" = k,
          "Size" = "thin"
        )
      
      CorrelationSample$ParticipantNumber <-
        as.character(CorrelationSample$ParticipantNumber)
      
      OverallCorrelationSample = wide_sample %>%
        summarize(
          "Feature" = Feature,
          "timescale_beforeESM" = timescale,
          "ParticipantNumber" = "Overall",
          Correlation = as.numeric(cor.test(as.formula(
            paste(
              "~ wide_sample$na_mean + wide_sample$",
              Feature,"_",
              timescale,
              sep = ""
            )
          ))$estimate),
          "p.value" = round(cor.test(as.formula(
            paste(
              "~ wide_sample$na_mean + wide_sample$",
              Feature,"_",
              timescale,
              sep = ""
            )
          ))$p.value, 2),
          "sample" = k,
          "Size" = "thick"
        )
      if (counter == 1) {
        CorrelationSampleCombined = rbind(CorrelationSample, OverallCorrelationSample)
      } else{
        CorrelationSampleCombined = rbind(CorrelationSampleCombined,
                                          CorrelationSample,
                                          OverallCorrelationSample)
        
      }
      if (k == 1) {
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
                paste("~ na_mean + ", Feature,"_", timescale, sep = "")
              ))$estimate),
              NA
            ),
            "p.value" = ifelse(
              sum(Passivecopy, na.rm = TRUE) != 0,
              round(cor.test(as.formula(
                paste("~ na_mean + ", Feature,"_", timescale, sep = "")
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
                "~ wide_orginal$na_mean + wide_orginal$",
                Feature,"_",
                timescale,
                sep = ""
              )
            ))$estimate),
            "p.value" = round(cor.test(as.formula(
              paste(
                "~ wide_orginal$na_mean + wide_orginal$",
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
  }
}


#write.csv(CorrelationSampleTrue,"CorrelationSampleTrue_na.csv")
#write.csv(CorrelationSampleCombined,"CorrelationSampleCombined_na.csv")



#### make data ready for table

### read results
CorrelationSampleTrue <- read.csv("CorrelationSampleTrue.csv")
CorrelationSampleCombined <- read.csv("CorrelationSampleCombined.csv")
CorrelationSampleTrue_na <- read.csv("CorrelationSampleTrue_na.csv")
CorrelationSampleCombined_na <- read.csv("CorrelationSampleCombined_na.csv")

CombinedSample  <- left_join(CorrelationSampleTrue, CorrelationSampleCombined, by = c("ParticipantNumber","Feature","timescale_beforeESM"), suffix = c("_True", "_Random"))
CombinedSample_na  <- left_join(CorrelationSampleTrue_na, CorrelationSampleCombined_na, by = c("ParticipantNumber","Feature","timescale_beforeESM"), suffix = c("_True", "_Random"))

CombinedSampleTest <- CombinedSample %>%
  group_by(ParticipantNumber,timescale_beforeESM, Feature) %>%
  summarize(
    "count_sign" = p.value_True >= p.value_Random
  )

CombinedSampleTest_na <- CombinedSample_na %>%
  group_by(ParticipantNumber,timescale_beforeESM, Feature) %>%
  summarize(
    "count_sign" = p.value_True >= p.value_Random
  )


SignDifferenceRandom <- CombinedSampleTest[CombinedSampleTest$count_sign == TRUE,]  %>% 
  group_by(ParticipantNumber,timescale_beforeESM, Feature)%>%
  summarize(
    "SignDifferenceRandom" = n()/500/2
  )

SignDifferenceRandom_na <- CombinedSampleTest_na[CombinedSampleTest_na$count_sign == TRUE,]  %>% 
  group_by(ParticipantNumber,timescale_beforeESM, Feature)%>%
  summarize(
    "SignDifferenceRandom" = n()/500/2
  )

CorrelationSampleTrue <-  left_join(CorrelationSampleTrue,SignDifferenceRandom, by = c("ParticipantNumber","Feature","timescale_beforeESM"),suffix = c("", ""))

CorrelationSampleTrue_na <-  left_join(CorrelationSampleTrue_na,SignDifferenceRandom_na, by = c("ParticipantNumber","Feature","timescale_beforeESM"),suffix = c("", ""))

# Add things for plot



CorrelationSampleTrue$sign = "non-sign"
CorrelationSampleTrue[!is.na(CorrelationSampleTrue$p.value) &
                        CorrelationSampleTrue$p.value <= 0.05, ]$sign = "sign"

CorrelationSampleTrue_na$sign = "non-sign"
CorrelationSampleTrue_na[!is.na(CorrelationSampleTrue_na$p.value) &
                           CorrelationSampleTrue_na$p.value <= 0.05, ]$sign = "sign"

CorrelationSampleTrue$star = ""
CorrelationSampleTrue[!is.na(CorrelationSampleTrue$p.value) &
                        CorrelationSampleTrue$p.value <= 0.05, ]$star = "*"

CorrelationSampleTrue_na$star = ""
CorrelationSampleTrue_na[!is.na(CorrelationSampleTrue_na$p.value) &
                           CorrelationSampleTrue_na$p.value <= 0.05, ]$star = "*"

CorrelationSampleTrue$star_random = ""
CorrelationSampleTrue[!is.na(CorrelationSampleTrue$SignDifferenceRandom) &
                        CorrelationSampleTrue$SignDifferenceRandom <= 0.05, ]$star_random = "x"

CorrelationSampleTrue_na$star_random = ""
CorrelationSampleTrue_na[!is.na(CorrelationSampleTrue_na$SignDifferenceRandom) &
                           CorrelationSampleTrue_na$SignDifferenceRandom <= 0.05, ]$star_random = "x"

CorrelationSampleTrue$star_combined <- paste(CorrelationSampleTrue$star,CorrelationSampleTrue$star_random)

CorrelationSampleTrue_na$star_combined <- paste(CorrelationSampleTrue_na$star,CorrelationSampleTrue_na$star_random)

CorrelationSampleTrue$timescale_beforeESM_num <- recode(CorrelationSampleTrue$timescale_beforeESM,"1h" = 1, "3h" = 3,"6h" = 6,"9h" = 9,"12h" = 12, "24h" = 24 )

CorrelationSampleTrue_na$timescale_beforeESM_num <- recode(CorrelationSampleTrue_na$timescale_beforeESM,"1h" = 1, "3h" = 3,"6h" = 6,"9h" = 9,"12h" = 12, "24h" = 24 )

CorrelationSampleTrue$timescale_beforeESM <- ordered(CorrelationSampleTrue$timescale_beforeESM,levels = c("1h","3h","6h","9h","12h","24h"))

CorrelationSampleTrue_na$timescale_beforeESM <- ordered(CorrelationSampleTrue_na$timescale_beforeESM,levels = c("1h","3h","6h","9h","12h","24h"))


CorrelationSampleCombined$sign = "non-sign"
CorrelationSampleCombined[!is.na(CorrelationSampleCombined$p.value) &
                            CorrelationSampleCombined$p.value <= 0.05, ]$sign = "sign"

CorrelationSampleCombined_na$sign = "non-sign"
CorrelationSampleCombined_na[!is.na(CorrelationSampleCombined_na$p.value) &
                               CorrelationSampleCombined_na$p.value <= 0.05, ]$sign = "sign"

CorrelationSampleCombined$star = ""
CorrelationSampleCombined[!is.na(CorrelationSampleCombined$p.value) &
                            CorrelationSampleCombined$p.value <= 0.05, ]$star = "*"

CorrelationSampleCombined_na$star = ""
CorrelationSampleCombined_na[!is.na(CorrelationSampleCombined_na$p.value) &
                               CorrelationSampleCombined_na$p.value <= 0.05, ]$star = "*"

CorrelationSampleCombined$timescale_beforeESM_num <- recode(CorrelationSampleCombined$timescale_beforeESM, "1h" = 1, "3h" = 3,"6h" = 6,"9h" = 9,"12h" = 12, "24h" = 24  )

CorrelationSampleCombined_na$timescale_beforeESM_num <- recode(CorrelationSampleCombined_na$timescale_beforeESM, "1h" = 1, "3h" = 3,"6h" = 6,"9h" = 9,"12h" = 12, "24h" = 24  )

CorrelationSampleCombined$timescale_beforeESM <- ordered(CorrelationSampleCombined$timescale_beforeESM,levels = c("1h","3h","6h","9h","12h","24h"))

CorrelationSampleCombined_na$timescale_beforeESM <- ordered(CorrelationSampleCombined_na$timescale_beforeESM,levels = c("1h","3h","6h","9h","12h","24h"))

#####
CorrelationSampleCombinedSign <- CorrelationSampleCombined %>%
  group_by(ParticipantNumber,timescale_beforeESM, Feature, sign) %>%
  summarize(
    "count_sign" = n()/500
  )


CorrelationSampleCombined_na <- CorrelationSampleCombined_na %>%
  group_by(ParticipantNumber,timescale_beforeESM, Feature, sign) %>%
  summarize(
    "count_sign" = n()/500
  )

CorrelationSampleCombinedSign <- CorrelationSampleCombinedSign[CorrelationSampleCombinedSign$sign == "sign",]

CorrelationSampleCombined_na <- CorrelationSampleCombined_na[CorrelationSampleCombined_na$sign == "sign",]

