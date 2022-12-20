### Prediciton Models (for affect, because here we can do individualized models?)
library('dplyr')
library("tidyr")
library("caret")
library("doParallel")


Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_220422.csv")
#Affect_Passive <- read.csv("/home2/p301669/FullDataset_220422.csv")
Affect_Passive <- Affect_Passive[Affect_Passive$questionListName != "Morning assessment 1" & Affect_Passive$questionListName != "Morning Assessment",]


#### CHANGE HERE THE LEVEL OF AGGREGATION ######
################################################
# 117134: Example participant
# 117121, we also deleted bluetooth features
# 117113 117114 117119 117121 117129 117130 117131 117134 117135 117137
PNumbers <- c(117113, 117114, 117119, 117121, 117129, 117130, 117131, 117135, 117137)
#timescale_beforeESM <- c("1h", "3h", "6h", "9h", "12h", "24h")
timescale_beforeESM <- c("1h","3h", "6h", "9h", "12h", "24h")


#parallel cluster
#Here I use one node with several clusters
#cpu <- Sys.getenv("SLURM_CPUS_ON_NODE", 24) # Number of cores requested (use 1 core if running outside a job).
#hosts <- rep("localhost",cpu)
#cl <- makeCluster(hosts, type = "SOCK")
#print(cl)


cl <- makePSOCKcluster(detectCores()-1)

registerDoParallel(cl)


start_time <- Sys.time()

for(n in timescale_beforeESM){

Participant1 = Affect_Passive[Affect_Passive["ParticipantNumber"] == 117121 & Affect_Passive["timescale_beforeESM"] == n,  ]

### Feature Selection
# Participant1 %>% select(pa_mean, SOCIAL_min ,
#                COMMUNICATION_min ,
#                com.whatsapp_min,
#                APP_USAGE_min ,
#                APPS_OPENED_number ,
#                Cluster_HOME_min ,
#                Cluster_1_min ,
#                Cluster_2_min ,
#                Cluster_3_min ,
#                Cluster_4_min ,
#                Cluster_5_min ,
#                UNIQUE_STAYPOINTS_number ,
#                TIME_STATIONARY_min ,
#                TOTAL_MACHASHES_number ,
#                UNIQUE_MACHASHES_number ,
#                BLUETOOTH_TOTAL_MACHASHES_number ,
#                BLUETOOTH_UNIQUE_MACHASHES_number ,
#                CALL_TOTAL_min ,
#                CALL_incoming_min ,
#                CALL_outgoing_min ,
#                CALL_TOTAL_number ,                                                   
#                CALL_incoming_number ,                                                
#                CALL_outgoing_number ,                                               
#                CALL_UNIQUE_CONTACTS_number ,                                         
#                LIGHT_LUX_mean ,                                                      
#                LIGHT_LUX_std ,                                                       
#                SCREEN_onLocked_number  ,                                             
#                SCREEN_onUnlocked_number , 
#                SMS_received_number  ,                                                
#                SMS_sent_number,                                                    
#                SMS_UNIQUE_CONTACTS_number) %>% dfSummary()

# Almost no SMS and Calls, thus I deleted those features


# Which formula to use
formula2 <- as.formula(paste("pa_mean ~  SOCIAL_min +
               COMMUNICATION_min +
               com.whatsapp_min +
               APP_USAGE_min +
               APPS_OPENED_number +
               Cluster_HOME_min +
               Cluster_1_min +
               Cluster_2_min +
               Cluster_3_min +
               Cluster_4_min +
               Cluster_5_min +
               TIME_STATIONARY_min +
               TOTAL_MACHASHES_number +
               UNIQUE_MACHASHES_number +
               BLUETOOTH_TOTAL_MACHASHES_number +
               BLUETOOTH_UNIQUE_MACHASHES_number +
               LIGHT_LUX_mean +                                                      
               SCREEN_onLocked_number  +                                             
               SCREEN_onUnlocked_number"))

Features <- c("SOCIAL_min","COMMUNICATION_min","com.whatsapp_min","APP_USAGE_min","APPS_OPENED_number","Cluster_HOME_min","Cluster_1_min","Cluster_2_min",
"Cluster_3_min","Cluster_4_min","Cluster_5_min","TIME_STATIONARY_min","TOTAL_MACHASHES_number","UNIQUE_MACHASHES_number","BLUETOOTH_TOTAL_MACHASHES_number","BLUETOOTH_UNIQUE_MACHASHES_number",
"LIGHT_LUX_mean","SCREEN_onLocked_number ","SCREEN_onUnlocked_number")


### 117121, we remove bluetooth because otherwise only 8 observations are left
Features <- c("SOCIAL_min","COMMUNICATION_min","com.whatsapp_min","APP_USAGE_min","APPS_OPENED_number","Cluster_HOME_min","Cluster_1_min","Cluster_2_min",
              "Cluster_3_min","Cluster_4_min","Cluster_5_min","TIME_STATIONARY_min","TOTAL_MACHASHES_number","UNIQUE_MACHASHES_number",
              "LIGHT_LUX_mean","SCREEN_onLocked_number ","SCREEN_onUnlocked_number")


pa_mean <- Participant1$pa_mean

Participant1 <- Participant1[,which(colnames(Participant1) %in% Features)]

Participant1 <- Participant1[,colSums(Participant1, na.rm = TRUE) != 0]
Participant1$pa_mean = pa_mean

Participant1 <- Participant1[rowSums(Participant1[,which(colnames(Participant1) %in% Features)], na.rm = TRUE) != 0,]

Participant1 <- na.omit(Participant1)


formula2 <- as.formula(paste("pa_mean ~", paste(colnames(Participant1), collapse = "+")))

counter <- 0
# https://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series

#### k is the moving window size ####
#####################################

for(k in c(6:42)){
  start_time2 <- Sys.time()
  
  set.seed(12361488)
  counter = counter + 1  
  timeSlices <- createTimeSlices(1:nrow(Participant1), 
                                 initialWindow = k, horizon = 1, fixedWindow = TRUE)
  
  trainSlices <- timeSlices[[1]]
  testSlices <- timeSlices[[2]]
  
  fitControl <- trainControl(method = "LOOCV")
  
  pred <- rep(NA,length(trainSlices))
  true <- rep(NA,length(trainSlices))
  w <- rep(NA,length(trainSlices))
  index <- rep(NA,length(testSlices))
  
  for(i in 1:length(trainSlices)){
    
    model<- train(formula2, data=Participant1[trainSlices[[i]],],trControl = fitControl, method="rf", na.action = na.pass) 
    

    pred[i] <- predict(model,Participant1[testSlices[[i]],]) #final model automatically used with predict
    true[i] <- Participant1$pa_mean[testSlices[[i]]]
    w[i] <- k
    index[i] <- as.numeric(testSlices[[i]])
  
  }
  if(counter == 1){
    end_time2 <- Sys.time()
    results <- cbind(pred,true,w,index)
    OverallResults <- results
    print(paste("Completed Time Window:",w[1],", Running time:", paste(end_time2 - start_time2)))
  }else{
    end_time2 <- Sys.time()
    results <- cbind(pred,true,w,index)
    OverallResults <- rbind(OverallResults, results)
    print(paste("Completed Time Window:",w[1],", Running time:",paste(end_time2 - start_time2)))
  }
}

end_time <- Sys.time()
end_time - start_time

OverallResults <- as.data.frame(OverallResults)

##### SELECT HERE THE LEVEL OF AGGREGATION #######

string <- paste("OverallResults_", n,"_",PNumbers[4], ".csv", sep = "")

print(string)
write.csv(OverallResults,string)
}

stopCluster(cl)
#write.csv(OverallResults,"OverallResults_3h_117113.csv")
#write.csv(OverallResults,"OverallResults_6h_117113.csv")
#write.csv(OverallResults,"OverallResults_9h.csv")
#write.csv(OverallResults,"OverallResults_12h.csv")
#write.csv(OverallResults,"OverallResults_24h_117113.csv")

#write.csv(OverallResults,"/home2/p301669/OverallResults_24h.csv")


############### Just mean predictions ################
######################################################

# Here the level of aggregation is not important (because we only use positive affect). Thus, we could have also used a different level than 6h

# 117134

PNumbers <- c(117113, 117114, 117119, 117121, 117129, 117130, 117131, 117135, 117137)

PNumber <- PNumbers[3]
Participant1 = Affect_Passive[Affect_Passive["ParticipantNumber"] == PNumber & Affect_Passive["timescale_beforeESM"] == "1h",  ] 
counter = 0

for(k in c(6:42)){
  set.seed(12361488)
  counter = counter + 1  
  timeSlices <- createTimeSlices(1:nrow(Participant1), 
                                 initialWindow = k, horizon = 1, fixedWindow = TRUE)
  
  trainSlices <- timeSlices[[1]]
  testSlices <- timeSlices[[2]]
  

  pred <- rep(NA,length(trainSlices))
  true <- rep(NA,length(trainSlices))
  w <- rep(NA,length(trainSlices))
  index <- rep(NA,length(testSlices))
  
  for(i in 1:length(trainSlices)){
    
    pred[i] <- mean(Participant1$pa_mean[trainSlices[[i]]]) #only mean pred
    true[i] <- Participant1$pa_mean[testSlices[[i]]]
    w[i] <- k
    index[i] <- as.numeric(testSlices[[i]])
    
  }
  if(counter == 1){
    end_time2 <- Sys.time()
    results <- cbind(pred,true,w,index)
    OverallResults <- results
  }else{
    end_time2 <- Sys.time()
    results <- cbind(pred,true,w,index)
    OverallResults <- rbind(OverallResults, results)
  }
}


string <- paste("OverallResults_MEAN_",PNumber, ".csv", sep = "")

write.csv(OverallResults,string)

