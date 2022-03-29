library('dplyr')
library('ggplot2')
library('plotly')
library("runner")


data <- read.csv('/Users/annalangener/Nextcloud/BEHAPP data/SocialContext.csv')
passive <- read.csv('/Users/annalangener/Nextcloud/BEHAPP data/TotalSocial.csv')


BEHAPP_ID = c(117113,117114,117119,117121,117130,117129,117131,117134,117135,117137)

passive = passive[passive$ParticipantNumber %in% BEHAPP_ID,]

Interactions <- read.csv('/Users/annalangener/Nextcloud/BEHAPP data/Interactions.csv')

IntPerDay = Interactions[Interactions$time_scale == 'per day',]
IntPerDay$Date <- as.Date(IntPerDay$Date)

passiveDay = passive[passive$timescale_index == 'per day',]
passiveDay$Date <- as.Date(passiveDay$Date)
passiveDay$index_time <- as.Date(passiveDay$index_time)


PerDay <- right_join(passiveDay, IntPerDay, by = c("ParticipantNumber" = "BEHAPP_ID","index_time" = "Date"),suffix = c("",""))
PerWeek <- full_join(passive[passive$timescale_index == "per week",], Interactions[Interactions$time_scale == "per week",],  by = c("ParticipantNumber" = "BEHAPP_ID","index_time" = "Date"),suffix = c("",""))
PerStudy <- full_join(passive[passive$timescale_index == "per study",], Interactions[Interactions$time_scale == "per study",], by = c("ParticipantNumber" = "BEHAPP_ID"),suffix = c("",""))



### Correlation different windows

Feature <- c('All_Interactions', "SOCIAL_APPS_min")
counter = 0


for(i in 1:length(BEHAPP_ID)){
  ParticipantNumbr = BEHAPP_ID[i]
for(k in 1:10){
counter = counter + 1

rolling2 = runner(PerDay[PerDay$ParticipantNumber == ParticipantNumbr,Feature[2]], k = k,function(x) mean(x, na.rm = TRUE), na_pad = TRUE)

if(counter == 1){
data = data.frame(Date = PerDay[PerDay$ParticipantNumber == ParticipantNumbr,"Date"],Feature1 = PerDay[PerDay$ParticipantNumber == ParticipantNumbr,Feature[1]],Feature2 = rolling2, ParticipantNumbr = ParticipantNumbr, k = k)
}else{
data = rbind(data, data.frame(Date = PerDay[PerDay$ParticipantNumber == ParticipantNumbr,"Date"], Feature1 = PerDay[PerDay$ParticipantNumber == ParticipantNumbr,Feature[1]],Feature2 = rolling2, ParticipantNumbr = ParticipantNumbr, k = k))
}
}
}


### Prepare Correlation
Correlation = data %>%
  group_by(ParticipantNumbr, k) %>%
  summarize(
    "Size" = "thin",
    "Correlation" = ifelse(sum(Feature2, na.rm = TRUE) != 0,as.numeric(cor.test(Feature1, Feature2)$estimate),NA),
    "p.value" = ifelse(sum(Feature2, na.rm = TRUE) != 0,round(cor.test(Feature1, Feature2)$p.value,2), NA)
  )

Correlation$ParticipantNumbr <- as.character(Correlation$ParticipantNumbr)

OverallCorrelation = data %>%
  group_by(k) %>%
  summarize(
    "Size" = "thick",
    "ParticipantNumbr" = "Overall",
    Correlation = as.numeric(cor.test(Feature1, Feature2)$estimate),
    "p.value" = round(cor.test(Feature1, Feature2)$p.value, 2)
  )

Correlation = rbind(Correlation,OverallCorrelation)

Correlation$sign = "non-sign"
Correlation[!is.na(Correlation$p.value) & Correlation$p.value <= 0.05,]$sign = "sign"

Correlation$star = ""
Correlation[!is.na(Correlation$p.value) & Correlation$p.value <= 0.05,]$star = "*"

#### Descriptive Plot

data[data$k != 1, ]$Feature1 = NA
max = max(data$Feature1, na.rm = TRUE)
maxy = max(data$Feature2,na.rm = TRUE)

if(max > maxy){
  scaleplot = 0.1*maxy
}else{
  scaleplot = maxy/max
}

plot2 = ggplot(data, aes(x = Date)) + 
  geom_bar(stat = 'identity',aes(y = Feature1*scaleplot)) +
  geom_line(aes(y = Feature2,color = as.factor(k))) +
  scale_color_manual(values = c("#f95d6a","#90F1B3", "#255e7e", "#3CD8DA", "#5383a1", "#3CD8DA", "#7faac6", "#218CB3", "#abd2ec", "#c1e7ff")) +
  ylab(Feature[2]) +
  theme_minimal() +
  scale_y_continuous(name = paste(Feature[2]), limits = c(0,1),sec.axis = sec_axis(~./scaleplot, name = 'Number of Interactions')) +
  facet_wrap(~ ParticipantNumbr, ncol = 2, scales = 'free')

plot2




#ggplotly()
cor1 =  ggplot(Correlation, aes(y= as.factor(ParticipantNumbr),x = as.factor(k), fill= Correlation)) + 
    geom_tile() +
    scale_fill_gradient2(low="#D7191C", mid="white", high="#2C7BB6") + 
    geom_text(aes(label=star), color="black", size=4) +
    ylab("") +
    xlab("") +
    ggtitle(paste("Correlation between", Feature[1], "&", Feature[2])) +
    theme_minimal()


cor2 = ggplot(Correlation, aes(x = k)) + 
  geom_line(aes(y = Correlation, color = as.factor(ParticipantNumbr), size = Size)) +
  geom_point(aes(y = Correlation, color = as.factor(ParticipantNumbr),shape = sign)) +
  scale_size_manual(values = c(1, 0.5)) +
  scale_shape_manual(values = c("sign" = 8, "non-sign" = 16)) +
  xlab("Rolling Window") +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank()) +
  scale_x_discrete(limits = c("k = 1",paste(rep("k =",9),2:10))) +
  ggtitle(paste("Correlation between", Feature[1], "&", Feature[2])) +
  guides(shape = guide_legend(""), size = FALSE, color = guide_legend("")) +
  scale_color_manual(values = c("#08324f","#90F1B3", "#255e7e", "#3CD8DA", "#5383a1", "#3CD8DA", "#7faac6", "#218CB3", "#abd2ec", "#c1e7ff", "#f95d6a"))

ggarrange(cor1,cor2,ncol = 1)

### Regression temporal resolution




### Other things
passive[passive$timescale_index == "per day",] %>%
  group_by(ParticipantNumber) %>%
  summarize(Count = n()) %>% as.data.frame()





data %>% 
  group_by(interact_dur_multipleChoice_string) %>%
  summarise(Count = n())


data %>% 
  group_by(interact_time_multipleChoice_string) %>%
  summarise(Count = n())


prob = data[which(data$interact_dur_multipleChoice_string == "Longer than three hours"),]

prob %>% 
  group_by(alias) %>%
  summarise(Count = n())

Int <- data[data$questionListName == "Interaction Assessment",]
Int %>% 
  group_by(interact_dur_multipleChoice_string) %>%
  summarise(Count = n())


Back <- data[data$questionListName == "Backlog Interaction",]

Back %>% 
  group_by(interact_time_multipleChoice_string) %>%
  summarise(Count = n())


Int$interact_time_multipleChoice_string

daily <- data[data$questionListName != "Interaction Assessment",]
daily <- daily[daily$questionListName != "Backlog Interaction",]


daily %>% 
  group_by(alone_multipleChoice_string, alias) %>%
  summarise(Count = n()) -> test2




##### Plots #####
plot = ggplot(PerDay, aes(x = Date)) + 
  geom_line(aes(y = All_Interactions, color = as.factor(ParticipantNumber)))

plot = ggplot(PerDay[PerDay$ParticipantNumber == 117113,], aes(x = Date)) + 
  geom_bar(stat="identity",aes(y = All_Interactions)) +
  geom_line(aes(y = CLUSTER_HOME_pct)) 

plot = ggplot(PerDay[PerDay$ParticipantNumber == 117113,], aes(x = Date)) + 
  geom_bar(stat="identity",aes(y = All_Interactions)) +
  geom_line(aes(y = APP_USAGE_pct)) 

PerDay <- PerDay[!is.na(PerDay$ParticipantNumber),]

