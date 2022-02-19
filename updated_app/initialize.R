library(dplyr)
library(readr)

#----------------------Load Dataset----------------------#
#data_path = "Insert datapath"
data_path = ""

BXTdata <- read.csv(paste0(data_path, "processed_cross_sectional_data.csv"))

BXTdata$country <- NULL 

#----------------------Encode participants----------------------#
BXTdata$NewEncode <- ifelse(BXTdata$NewStatus == "Always Stay", "AS",
                           ifelse(BXTdata$NewStatus == "Always Leave", "AL",
                                  ifelse(BXTdata$NewStatus == "Switch to Leave", "SL","SS")))

#----------------------Encode weights----------------------#
Weightsdata <- read.csv(paste0(data_path, "cleaned.csv"))

Weightsdata <- select(Weightsdata,c("id","wave","wt","country"))

BXTdata<- merge(BXTdata, Weightsdata, by=c("id","wave"))

#----------------------Convert Votes to Numbers----------------------#

##Leave is encoded as 1, Stay is encoded as 0
BXTdata$NewLeaveVote<-ifelse(BXTdata$NewVote == "Leave",1,0)
BXTdata$NewStayVote<-ifelse(BXTdata$NewVote == "Stay",1,0)


##Compute the weighted votes
BXTdata<-BXTdata %>% mutate(WeightedNewLeaveVote = NewLeaveVote * wt)
BXTdata<-BXTdata %>% mutate(WeightedNewStayVote = NewStayVote * wt)


#----------------------Convert Votes to Numbers----------------------#

##Add weights to AIS scores
BXTdata<-BXTdata %>% mutate(WeightedAIS = AIS * wt)

#----------------------Recode Country/Gender/Marriage----------------------#
#for gender

BXTdata <- BXTdata[!is.na(BXTdata$gender),]

#for marriage
BXTdata <- BXTdata[!is.na(BXTdata$p_marital),]


BXTdata$Marriage <- ifelse(BXTdata$p_marital == "Married", "Married",
                            ifelse(BXTdata$p_marital == "Separated but still legally married or in a civil partnership", "Married",
                                   ifelse(BXTdata$p_marital == "In a civil partnership", "Married","Single")))

#for country
BXTdata <- BXTdata[!is.na(BXTdata$country),]


#----------------------------Aggregate Dataset----------------------------#
BXTdata$NewEncode <- as.factor(BXTdata$NewEncode)
BXTdata$wave <- as.numeric(as.character(BXTdata$wave))
BXTdata$NewVote <- as.factor(BXTdata$NewVote)
BXTdata$NewStatus <- as.factor(BXTdata$NewStatus)


#agg1 <- BXTdata %>% 
#     group_by(wave,NewEncode) %>%
#     summarise(AIS_mean=mean(AIS, na.rm=T),AIS_sd=sd(AIS, na.rm=T))

#agg2 <- BXTdata %>% 
#  group_by(wave,NewEncode) %>%
#  summarise(Count = n()) %>%
#  mutate(Percentage = prop.table(Count/sum(Count)))

#agg <- merge(agg1,agg2)


agg1 <- BXTdata %>% 
       group_by(wave,NewVote,country,gender,Marriage) %>%
       summarise(TotalLeave=sum(WeightedNewLeaveVote,na.rm=T), 
                 TotalStay=sum(WeightedNewStayVote,na.rm=T),
                 AIS_mean=mean(AIS, na.rm=T),
                 AIS_sd=sd(AIS, na.rm=T)) %>%
       mutate(TotalPar = TotalLeave + TotalStay)%>%
       mutate(Percentage = prop.table(TotalPar/sum(TotalPar))) 

agg1 <- BXTdata %>% 
  group_by(wave,NewVote) %>%
  summarise(TotalLeave=sum(WeightedNewLeaveVote,na.rm=T), 
            TotalStay=sum(WeightedNewStayVote,na.rm=T),
            AIS_mean=mean(AIS, na.rm=T),
            AIS_sd=sd(AIS, na.rm=T)) %>%
  mutate(TotalPar = TotalLeave + TotalStay)%>%
  mutate(Percentage = prop.table(TotalPar/sum(TotalPar)))

agg11 <- agg1[agg1$NewVote=="Leave",]
agg11 <- dplyr::arrange(agg11,wave,NewVote)
agg12 <- agg1[agg1$NewVote=="Stay",]
agg12 <- dplyr::arrange(agg12,wave,NewVote)

library(DataCombine)


# Find percentage change from two periods before
Data <- data.frame(select(agg11, wave, TotalPar))

Out <- PercChange(Data, Var = 'TotalPar',
                  type = 'proportion',
                  NewVar = 'PercentChange',
                  slideBy = -1)

agg11 <- merge(x = agg11, y = select(Out,wave,PercentChange), by = "wave", all.x = TRUE)

agg11 <- agg11 %>% mutate(ChangeInPercent = Percentage - lag(Percentage,1))
agg11 <- agg11 %>% mutate(TotalChange = TotalPar - lag(TotalPar,1))


Data <- data.frame(select(agg12, wave, TotalPar))

Out <- PercChange(Data, Var = 'TotalPar',
                  type = 'proportion',
                  NewVar = 'PercentChange',
                  slideBy = -1)

agg12 <- merge(x = agg12, y = select(Out,wave,PercentChange), by = "wave", all.x = TRUE)

agg12 <- agg12 %>% mutate(ChangeInPercent = Percentage - lag(Percentage,1))
agg12 <- agg12 %>% mutate(TotalChange = TotalPar - lag(TotalPar,1))

agg1 <- rbind(agg11, agg12) 



aggData <- dplyr::arrange(agg1,wave,NewVote)

##Rename TotalPar as Count
colnames(aggData)[10] <- "Count"

##Rename percentage as proportion
colnames(aggData)[11] <- "Proportion"

#----------------------Convert Wave to Date----------------------#

BXTDates <- read.csv(paste0(data_path, "BrexitWaveDates.csv"))

colnames(BXTDates) <- c("wave","date")

BXTDates$date <- as.Date(BXTDates$date, format = "%m/%d/%Y")

#GTDdata <- merge(x = aggData, y = BXTDates, by = "wave", all.x = TRUE)
GTDdata <- merge(x = BXTdata, y = BXTDates, by = "wave", all.x = TRUE)

#----------------------Compute AgeGroup----------------------#

GTDdata$AgeGroup <- ifelse(GTDdata$age <=35 , "Age19-35",
                           ifelse(GTDdata$age >35 & GTDdata$age <=45, "Age36-45",
                                  ifelse(GTDdata$age >45 & GTDdata$age <=65, "Age46-65",
                                         ifelse(GTDdata$age >65 & GTDdata$age <=85, "Age66-85","AgeOver85"))))




#----------------------Pre-define options to the users----------------------#


# Options
#For X-axis
XAxisOptions <- c("All" = "all",
                  "Stay" = "Stay",
                  "Leave" = "Leave")

#Temporal display options
TemporalOptions <- c("Proportion" = "Proportion",
               "Count" = "Count",
               "Percent Change" = "PercentChange",
               "Change in Percent"  = "ChangeInPercent",
               "Total Change"  = "TotalChange"
               )

FilterOptions <- c("All" = "all",
                   "Gender" = "Gender",
                   "Country" = "Country",
                   "Marriage" = "Marriage",
                   "Age"="AgeGroup")


colorOptions <- c("None" = "none",
                  "Region" = "Region",
                  "Religion" = "Religion")


#Custom Colors for graphs
customColors <- c("#a6cee3", "#1f78b4", "#b2df84", "#33a02c",
                  "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")


# Options
regionOptions <- c("All" = "all",
                   "Middle East & North Africa" = "Middle East & N. Africa",
                   "Sub-Saharan Africa" = "Sub-Saharan Africa",
                   "East Asia & Pacific" = "East Asia & Pacific",
                   "South Asia" = "South Asia",
                   "Europe & Central Asia" = "Europe & Central Asia",
                   "North America" = "North America",
                   "Latin America & Caribbean" = "Latin America"  
)