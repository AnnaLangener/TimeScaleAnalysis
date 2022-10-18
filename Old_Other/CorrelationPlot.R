CorrelationPlot <- function(Feature,PerHour,PerDay,PerWeek,PerStudy,PerHourNA,PerDayNA,PerWeekNA,PerStudyNA){
  ### Being Home & Unique Number of Places Visited
  CorPerHour = PerHour %>%
    group_by(ParticipantNumber) %>%
    summarize(Correlation = as.numeric(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$estimate),p.value=round(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$p.value,2)) %>% as.data.frame()
  
  CorPerHour <- rbind(data.frame("ParticipantNumber" = "Overall", "Correlation" = as.numeric(cor.test(PerHour[,Feature[1]],PerHour[,Feature[2]])$estimate), "p.value" = round(cor.test(PerHour[,Feature[1]],PerHour[,Feature[2]])$p.value)), CorPerHour)
  
  CorPerDay = PerDay %>%
    group_by(ParticipantNumber) %>%
    summarize(Correlation = as.numeric(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$estimate),p.value=round(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$p.value,2)) %>% as.data.frame()
  
  CorPerDay <- rbind(data.frame("ParticipantNumber" = "Overall", "Correlation" = as.numeric(cor.test(PerDay[,Feature[1]],PerDay[,Feature[2]])$estimate), "p.value" = round(cor.test(PerDay[,Feature[1]],PerDay[,Feature[2]])$p.value)), CorPerDay)
  
  CorPerWeek = PerWeek %>%
    group_by(ParticipantNumber) %>%
    summarize(Correlation = as.numeric(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$estimate),p.value=round(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$p.value,2)) %>% as.data.frame()
  
  CorPerWeek <- rbind(data.frame("ParticipantNumber" = "Overall", "Correlation" = as.numeric(cor.test(PerWeek[,Feature[1]],PerWeek[,Feature[2]])$estimate), "p.value" = round(cor.test(PerWeek[,Feature[1]],PerWeek[,Feature[2]])$p.value)), CorPerWeek)
  
  #Correlation <- merge(CorPerHour,CorPerDay, by = "ParticipantNumber")
  #Correlation <- merge(Correlation,CorPerWeek, by = "ParticipantNumber")
  
  plot1 <- ggplot() +
    geom_point(aes(x = as.factor(CorPerHour[CorPerHour$p.value > 0.05,"ParticipantNumber"]), y = CorPerHour[CorPerHour$p.value > 0.05,"Correlation"], color = "Per Hour", shape = "non-significant"), ) +
    geom_point(aes(x = as.factor(CorPerHour[CorPerHour$p.value <= 0.05,"ParticipantNumber"]), y = CorPerHour[CorPerHour$p.value <= 0.05,"Correlation"], color = "Per Hour", shape = "significant")) +
    geom_hline(yintercept=CorPerHour$Correlation[CorPerHour$ParticipantNumber == "Overall"], linetype="dashed", color = "#0B3C69") +
    geom_point(aes(x = as.factor(CorPerDay[CorPerDay$p.value > 0.05,"ParticipantNumber"]), y = CorPerDay[CorPerDay$p.value > 0.05,"Correlation"], color = "Per Day", shape = "non-significant")) +
    geom_point(aes(x = as.factor(CorPerDay[CorPerDay$p.value <= 0.05,"ParticipantNumber"]), y = CorPerDay[CorPerDay$p.value <= 0.05,"Correlation"], color = "Per Day", shape = "significant")) +
    geom_hline(yintercept=CorPerDay$Correlation[CorPerDay$ParticipantNumber == "Overall"], linetype="dashed", color = "#708FF9") +
    geom_point(aes(x = as.factor(CorPerWeek[CorPerWeek$p.value > 0.05,"ParticipantNumber"]), y = CorPerWeek[CorPerWeek$p.value > 0.05,"Correlation"], color = "Per Week", shape = "non-significant")) +
    geom_point(aes(x = as.factor(CorPerWeek[CorPerWeek$p.value <= 0.05,"ParticipantNumber"]), y = CorPerWeek[CorPerWeek$p.value <= 0.05,"Correlation"],color = "Per Week",shape = "significant")) +
    geom_hline(yintercept=CorPerWeek$Correlation[CorPerWeek$ParticipantNumber == "Overall"], linetype="dashed", color = "#FFCD3C") +
    ylab(element_blank()) +
    xlab("Participants") +
    scale_color_manual(name = "Resolution", values = c("Per Hour" = "#0B3C69", "Per Day" = "#708FF9", "Per Week" = "#FFCD3C")) +
    guides(shape = guide_legend("Significance")) +
    theme_minimal() +
    theme(axis.text.x=element_blank(), panel.grid.minor.y = element_blank()) +
    ggtitle("Excluding missing values") +
    ylim(-1,1)
  
  ### Without removing NA
  
  CorPerHourNA = PerHourNA %>%
    group_by(ParticipantNumber) %>%
    summarize(Correlation = as.numeric(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$estimate),p.value=round(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$p.value,2)) %>% as.data.frame()
  
  CorPerHourNA <- rbind(data.frame("ParticipantNumber" = "Overall", "Correlation" = as.numeric(cor.test(PerHourNA[,Feature[1]],PerHourNA[,Feature[2]])$estimate), "p.value" = round(cor.test(PerHourNA[,Feature[1]],PerHourNA[,Feature[2]])$p.value)), CorPerHourNA)
  
  CorPerDayNA = PerDayNA %>%
    group_by(ParticipantNumber) %>%
    summarize(Correlation = as.numeric(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$estimate),p.value=round(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$p.value,2)) %>% as.data.frame()
  
  CorPerDayNA <- rbind(data.frame("ParticipantNumber" = "Overall", "Correlation" = as.numeric(cor.test(PerDayNA[,Feature[1]],PerDayNA[,Feature[2]])$estimate), "p.value" = round(cor.test(PerDayNA[,Feature[1]],PerDayNA[,Feature[2]])$p.value)), CorPerDayNA)
  
  CorPerWeekNA = PerWeekNA %>%
    group_by(ParticipantNumber) %>%
    summarize(Correlation = as.numeric(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$estimate),p.value=round(cor.test(as.formula(paste("~",Feature[1], "+",Feature[2])))$p.value,2)) %>% as.data.frame()
  
  CorPerWeekNA <- rbind(data.frame("ParticipantNumber" = "Overall", "Correlation" = as.numeric(cor.test(PerWeekNA[,Feature[1]],PerWeekNA[,Feature[2]])$estimate), "p.value" = round(cor.test(PerWeekNA[,Feature[1]],PerWeekNA[,Feature[2]])$p.value)), CorPerWeekNA)
  
  plot2 <- ggplot() +
    geom_point(aes(x = as.factor(CorPerHourNA[CorPerHourNA$p.value > 0.05,"ParticipantNumber"]), y = CorPerHourNA[CorPerHourNA$p.value > 0.05,"Correlation"], color = "Per Hour", shape = "non-significant"), ) +
    geom_point(aes(x = as.factor(CorPerHourNA[CorPerHourNA$p.value <= 0.05,"ParticipantNumber"]), y = CorPerHourNA[CorPerHourNA$p.value <= 0.05,"Correlation"], color = "Per Hour", shape = "significant")) +
    geom_hline(yintercept=CorPerHourNA$Correlation[CorPerHourNA$ParticipantNumber == "Overall"], linetype="dashed", color = "#0B3C69") +
    geom_point(aes(x = as.factor(CorPerDayNA[CorPerDayNA$p.value > 0.05,"ParticipantNumber"]), y = CorPerDayNA[CorPerDayNA$p.value > 0.05,"Correlation"], color = "Per Day", shape = "non-significant")) +
    geom_point(aes(x = as.factor(CorPerDayNA[CorPerDayNA$p.value <= 0.05,"ParticipantNumber"]), y = CorPerDayNA[CorPerDayNA$p.value <= 0.05,"Correlation"], color = "Per Day", shape = "significant")) +
    geom_hline(yintercept=CorPerDayNA$Correlation[CorPerDayNA$ParticipantNumber == "Overall"], linetype="dashed", color = "#708FF9") +
    geom_point(aes(x = as.factor(CorPerWeekNA[CorPerWeekNA$p.value > 0.05,"ParticipantNumber"]), y = CorPerWeekNA[CorPerWeekNA$p.value > 0.05,"Correlation"], color = "Per Week", shape = "non-significant")) +
    geom_point(aes(x = as.factor(CorPerWeekNA[CorPerWeekNA$p.value <= 0.05,"ParticipantNumber"]), y = CorPerWeekNA[CorPerWeekNA$p.value <= 0.05,"Correlation"],color = "Per Week",shape = "significant")) +
    geom_hline(yintercept=CorPerWeekNA$Correlation[CorPerWeekNA$ParticipantNumber == "Overall"], linetype="dashed", color = "#FFCD3C") +
    ylab(element_blank()) +
    xlab("Participants") +
    scale_color_manual(name = "Resolution", values = c("Per Hour" = "#0B3C69", "Per Day" = "#708FF9", "Per Week" = "#FFCD3C")) +
    guides(shape = guide_legend("Significance")) +
    theme_minimal() +
    theme(axis.text.x=element_blank(), panel.grid.minor.y = element_blank()) +
    ggtitle("Including missing values") +
    ylim(-1,1)
  
  # Differences
  CorPerHourNA$diff = CorPerHourNA$Correlation - CorPerHour$Correlation
  CorPerDayNA$diff = CorPerDayNA$Correlation - CorPerDay$Correlation
  CorPerWeekNA$diff = CorPerWeekNA$Correlation - CorPerWeek$Correlation
  
  plot3 <- ggplot() +
    geom_point(aes(x = as.factor(CorPerHourNA[,"ParticipantNumber"]), y = CorPerHourNA[,"diff"], color = "Per Hour")) + 
    geom_hline(yintercept=CorPerHourNA$diff[CorPerHourNA$ParticipantNumber == "Overall"], linetype="dashed", color = "#0B3C69") +
    geom_point(aes(x = as.factor(CorPerDayNA[,"ParticipantNumber"]), y = CorPerDayNA[,"diff"], color = "Per Day")) +
    geom_hline(yintercept=CorPerDayNA$diff[CorPerDayNA$ParticipantNumber == "Overall"], linetype="dashed", color = "#708FF9") +
    geom_point(aes(x = as.factor(CorPerWeekNA[,"ParticipantNumber"]), y = CorPerWeekNA[,"diff"], color = "Per Week")) +
    geom_hline(yintercept=CorPerWeekNA$diff[CorPerWeekNA$ParticipantNumber == "Overall"], linetype="dashed", color = "#FFCD3C") +
    ylab(element_blank()) +
    xlab("Participants") +
    scale_color_manual(name = "Resolution", values = c("Per Hour" = "#0B3C69", "Per Day" = "#708FF9", "Per Week" = "#FFCD3C")) +
    theme_minimal() +
    theme(axis.text.x=element_blank(), panel.grid.minor.y = element_blank()) +
    ggtitle("Difference missing values")
  
  plot <- ggarrange(plot1,plot3, common.legend = TRUE)
  #plot <- ggarrange(plot,plot3,ncol=1,nrow=2,common.legend = TRUE, heights = c(3,2))
  plot <- annotate_figure(plot, top = text_grob(paste("Correlation:", Feature[1], "&", Feature[2], "(per Participant)"),color = "black", size = 14))
  
  return(plot) 
}