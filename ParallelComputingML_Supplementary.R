
##################### Prediction Model  ####################
# Prediction Model for Participant in Supplementary Material

library('dplyr')
library("tidyr")
library("caret")
library("doParallel")

Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/BEHAPP data/FullDataset_2709_24missing.csv") #Data is labelled as missing if not present for 24h
Affect_Passive <- Affect_Passive[Affect_Passive$questionListName != "Morning assessment 1" & Affect_Passive$questionListName != "Morning Assessment",]


# 117121: Participant used in supplementary material
PNumbers <- c(117134, 117113, 117114, 117119, 117121, 117129, 117130, 117131, 117135, 117137)
timescale_beforeESM <- c("1h","3h", "6h", "9h", "12h", "24h") # Level of aggregation


#### Start parallel computing
cl <- makePSOCKcluster(detectCores()-1)

registerDoParallel(cl)

start_time <- Sys.time()



# **** We loop through the different levels of aggregation (n) ****
for(n in timescale_beforeESM){
  
  OverallResults <- list() #create a list to store the results
  
  Participant1 = Affect_Passive[Affect_Passive["ParticipantNumber"] == PNumbers[5] & Affect_Passive["timescale_beforeESM"] == n,  ]
  
  
  ### 117121, we remove bluetooth because otherwise only 8 observations are left
  Features <- c("SOCIAL_min","COMMUNICATION_min","com.whatsapp_min","APP_USAGE_min","APPS_OPENED_number","Cluster_HOME_min","Cluster_1_min","Cluster_2_min",
                "Cluster_3_min","Cluster_4_min","Cluster_5_min","TIME_STATIONARY_min","TOTAL_MACHASHES_number","UNIQUE_MACHASHES_number",
                "LIGHT_LUX_mean","SCREEN_onLocked_number","SCREEN_onUnlocked_number")
  
  
  ### remove features that are not present
  pa_mean <- Participant1$pa_mean
  Participant1 <- Participant1[,which(colnames(Participant1) %in% Features)] # We remove all other variables
  Participant1 <- Participant1[,colSums(Participant1, na.rm = TRUE) != 0] # We remove all variables that are 0 over the study period
  Participant1$pa_mean <- pa_mean
  Participant1 <- Participant1[rowSums(Participant1[,which(colnames(Participant1) %in% Features)], na.rm = TRUE) != 0,] # We remove all observations that only include missing values
  Participant1 <- na.omit(Participant1)
  formula2 <- as.formula(paste("pa_mean ~", paste(colnames(Participant1[,colnames(Participant1) != "pa_mean"]), collapse = "+")))

  
  # https://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series
  
  
  # **** We loop through the different moving window sizes (k) ****
  
  for(k in c(6:42)){
    start_time2 <- Sys.time()
    
    set.seed(12361488)
    timeSlices <- createTimeSlices(1:nrow(Participant1), 
                                   initialWindow = k, horizon = 1, fixedWindow = TRUE) # We use the createTimeSlices to split our dataframe into a train and test set
    
    trainSlices <- timeSlices[[1]]
    testSlices <- timeSlices[[2]]
    
    fitControl <- trainControl(method = "LOOCV") # to use leave-one-out-cross-validation to choose the best hyperparameters (= number of predictors)  in the training set. 
    
    pred <- rep(NA,length(trainSlices)) #we create empty vectors to store the results
    true <- rep(NA,length(trainSlices))
    w <- rep(NA,length(trainSlices))
    index <- rep(NA,length(testSlices))
    
    
    for(i in 1:length(trainSlices)){
      
      model<- train(formula2, data=Participant1[trainSlices[[i]],],trControl = fitControl, method="rf", na.action = na.omit) 
      
      # Next we store the results from the prediction model by making predictions for our test set. The predict() function automatically uses the "best" model (based on loocv from the trains set)
      
      pred[i] <- predict(model,Participant1[testSlices[[i]],]) #final model automatically used with predict
      true[i] <- Participant1$pa_mean[testSlices[[i]]]
      w[i] <- k
      index[i] <- as.numeric(testSlices[[i]])
      
    }
    end_time2 <- Sys.time()
    results <- cbind(pred,true,w,index)
    OverallResults[[k-5]] <- results # store results in our empty list
    print(paste("Completed Time Window:",w[1],", Running time:", paste(end_time2 - start_time2)))
    
  }
  
  end_time <- Sys.time()
  end_time - start_time
  
  OverallResults <- as.data.frame(do.call("rbind",OverallResults)) #Combine lists into one dataframe
  
  
  # Save the results
  
  string <- paste("OverallResults_", n,"_tes",PNumbers[5], ".csv", sep = "")
  print(string)
  write.csv(OverallResults,string)
}

stopCluster(cl)

############### Just mean predictions ################
######################################################

# Here the level of aggregation is not important (because we only use positive affect). Thus, we could have also used a different level than 6h

PNumbers <- c(117134, 117113, 117114, 117119, 117121, 117129, 117130, 117131, 117135, 117137)

PNumber <- PNumbers[5]
Participant1 = Affect_Passive[Affect_Passive["ParticipantNumber"] == PNumber & Affect_Passive["timescale_beforeESM"] == "1h",  ] 

OverallResults <- list() #create a list to store the results


for(k in c(6:42)){
  set.seed(12361488)
  timeSlices <- createTimeSlices(1:nrow(Participant1), 
                                 initialWindow = k, horizon = 1, fixedWindow = TRUE)
  
  trainSlices <- timeSlices[[1]]
  testSlices <- timeSlices[[2]]
  
  
  pred <- rep(NA,length(trainSlices))
  true <- rep(NA,length(trainSlices))
  w <- rep(NA,length(trainSlices))
  index <- rep(NA,length(testSlices))
  
  for(i in 1:length(trainSlices)){
    
    pred[i] <- mean(Participant1$pa_mean[trainSlices[[i]]]) #we use the rolling mean as prediction
    true[i] <- Participant1$pa_mean[testSlices[[i]]]
    w[i] <- k
    index[i] <- as.numeric(testSlices[[i]])
    
  }
  end_time2 <- Sys.time()
  results <- cbind(pred,true,w,index)
  OverallResults[[k-5]] <- results # store results in our empty list
}

OverallResults <- as.data.frame(do.call("rbind",OverallResults)) #Combine lists into one dataframe


string <- paste("OverallResults_MEAN_",PNumber, ".csv", sep = "")

write.csv(OverallResults,string)

