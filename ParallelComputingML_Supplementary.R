
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
  
  string <- paste("OverallResults_", n,"_",PNumbers[5], ".csv", sep = "")
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


###### Plots #######
####################

library(forcats)
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

Pnumber <- 117121

OverallResults_1h <- read.csv(paste("Results/OverallResults_1h_",Pnumber, ".csv",sep = ""))
OverallResults_3h <- read.csv(paste("Results/OverallResults_3h_",Pnumber, ".csv",sep = ""))
OverallResults_6h <- read.csv(paste("Results/OverallResults_6h_",Pnumber, ".csv",sep = ""))
OverallResults_9h <- read.csv(paste("Results/OverallResults_9h_",Pnumber, ".csv",sep = ""))
OverallResults_12h <- read.csv(paste("Results/OverallResults_12h_",Pnumber, ".csv",sep = ""))
OverallResults_24h <- read.csv(paste("Results/OverallResults_24h_",Pnumber, ".csv",sep = ""))
MEAN_OverallResults_6h <- read.csv(paste("Results/OverallResults_MEAN_",Pnumber, ".csv",sep = ""))



#### Calculate Performance Measures ####
perfromanceCalc <- function(OverallResults){
  counter = 0
  OverallResults <- OverallResults[OverallResults$index >= 43,]
  for(i in unique(OverallResults$w)){
    counter = counter + 1
    results <- OverallResults[OverallResults$w == i,]
    
    cor = cor.test(results$pred,results$true)$estimate
    cor.int_1 = cor.test(results$pred,results$true)$conf.int[1]
    cor.int_2 = cor.test(results$pred,results$true)$conf.int[2]
    rss <- sum((results$pred - results$true) ^ 2)  ## residual sum of squares
    tss <- sum((results$true - mean(results$true)) ^ 2)  ## total sum of squares
    rsq <- 1 - rss/tss
    RMSE <- RMSE(results$pred,results$true)
    
    if(counter == 1){
      resultsPerformance <- cbind(i,cor,cor.int_1,cor.int_2,rsq,RMSE)
      OverallResultsPerformance <- resultsPerformance
    }else{
      resultsPerformance <- cbind(i,cor,cor.int_1,cor.int_2,rsq,RMSE)
      OverallResultsPerformance <- rbind(OverallResultsPerformance, resultsPerformance)
    }
    OverallResultsPerformance <- as.data.frame(OverallResultsPerformance)
  }
  return(OverallResultsPerformance)
}

OverallResultsPerformance_1h <- perfromanceCalc(OverallResults_1h)
OverallResultsPerformance_3h <- perfromanceCalc(OverallResults_3h)
OverallResultsPerformance_6h <- perfromanceCalc(OverallResults_6h)
OverallResultsPerformance_9h <- perfromanceCalc(OverallResults_9h)
OverallResultsPerformance_12h <- perfromanceCalc(OverallResults_12h)
OverallResultsPerformance_24h <- perfromanceCalc(OverallResults_24h)
MEAN_OverallResultsPerformance_6h <- perfromanceCalc(MEAN_OverallResults_6h)

plot2 <- ggplot() +
  geom_line(aes(y = OverallResultsPerformance_24h$rsq, x = OverallResultsPerformance_24h$i, color = "24h")) +
  geom_line(aes(y = OverallResultsPerformance_12h$rsq, x = OverallResultsPerformance_12h$i, color = "12h")) +
  geom_line(aes(y = OverallResultsPerformance_9h$rsq, x = OverallResultsPerformance_9h$i, color = "9h")) +
  geom_line(aes(y = OverallResultsPerformance_6h$rsq, x = OverallResultsPerformance_6h$i, color = "6h")) +
  geom_line(aes(y = OverallResultsPerformance_3h$rsq, x = OverallResultsPerformance_3h$i, color = "3h")) +
  geom_line(aes(y = OverallResultsPerformance_1h$rsq, x = OverallResultsPerformance_1h$i, color = "1h")) +
  geom_line(aes(y = MEAN_OverallResultsPerformance_6h$rsq, x = MEAN_OverallResultsPerformance_6h$i, color = "rolling mean"), size = 1.3) +
  theme_minimal() +
  xlab("Moving Window: Number of observations used for building the prediction model") +
  ylab("R-squared") +
  scale_color_viridis(discrete=TRUE,limits = c("24h","12h","9h","6h","3h","1h","rolling mean")) +
  labs(color = "Level of Aggregation") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)


ggsave(
  "RSquared_sup.jpg",
  plot = plot2,
  width = 23,
  height = 10,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL)


#### Level of Aggregation

#Correlation 1h
max <- which(OverallResultsPerformance_1h$rsq == max(OverallResultsPerformance_1h$rsq))
min <- which(OverallResultsPerformance_1h$rsq == min(OverallResultsPerformance_1h$rsq))

paste("(m = ",round(mean(OverallResultsPerformance_1h$rsq),2), ", max = ",round(OverallResultsPerformance_1h$rsq[max],2),", min = ",round(OverallResultsPerformance_1h$rsq[min],2),")", sep = "")

#Correlation 3h
max <- which(OverallResultsPerformance_3h$rsq == max(OverallResultsPerformance_3h$rsq))
min <- which(OverallResultsPerformance_3h$rsq == min(OverallResultsPerformance_3h$rsq))

paste("(m = ",round(mean(OverallResultsPerformance_3h$rsq),2), ", max = ",round(OverallResultsPerformance_3h$rsq[max],2),", min = ",round(OverallResultsPerformance_3h$rsq[min],2),")", sep = "")

#Correlation 6h
max <- which(OverallResultsPerformance_6h$cor == max(OverallResultsPerformance_6h$cor))
min <- which(OverallResultsPerformance_6h$cor == min(OverallResultsPerformance_6h$cor))

paste("(m = ",round(mean(OverallResultsPerformance_6h$rsq),2), ", max = ",round(OverallResultsPerformance_6h$rsq[max],2),", min = ",round(OverallResultsPerformance_6h$rsq[min],2),")", sep = "")

#Correlation 9h
max <- which(OverallResultsPerformance_9h$rsq == max(OverallResultsPerformance_9h$rsq))
min <- which(OverallResultsPerformance_9h$rsq == min(OverallResultsPerformance_9h$rsq))

paste("(m = ",round(mean(OverallResultsPerformance_9h$rsq),2), ", max = ",round(OverallResultsPerformance_9h$rsq[max],2),", min = ",round(OverallResultsPerformance_9h$rsq[min],2),")", sep = "")

#Correlation 12h
max <- which(OverallResultsPerformance_12h$rsq == max(OverallResultsPerformance_12h$rsq))
min <- which(OverallResultsPerformance_12h$rsq == min(OverallResultsPerformance_12h$rsq))


paste("(m = ",round(mean(OverallResultsPerformance_12h$rsq),2), ", max = ",round(OverallResultsPerformance_12h$rsq[max],2),", min = ",round(OverallResultsPerformance_12h$rsq[min],2),")", sep = "")

#Correlation 24h
max <- which(OverallResultsPerformance_24h$rsq == max(OverallResultsPerformance_24h$rsq))
min <- which(OverallResultsPerformance_24h$rsq == min(OverallResultsPerformance_24h$rsq))

paste("(m = ",round(mean(OverallResultsPerformance_24h$rsq),2), ", max = ",round(OverallResultsPerformance_24h$rsq[max],2),", min = ",round(OverallResultsPerformance_24h$rsq[min],2),")", sep = "")

