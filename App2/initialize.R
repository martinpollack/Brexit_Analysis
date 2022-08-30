library(dplyr)
library(readr)
library(DataCombine)

#----------------------Load Dataset----------------------#
#data_path = "Insert datapath"
data_path = ""
#data_path = paste0(getwd(), "/Brexit Analysis/BrexitApp2/")

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


#----------------------Recode GrossIncome----------------------#

#BXTdata$p_gross_household <- as.character(BXTdata$p_gross_household)

BXTdata$GrossIncome <- ifelse(grepl("under",BXTdata$p_gross_household, fixed=TRUE)
                              | grepl("?5,000",BXTdata$p_gross_household, fixed=TRUE)
                              | grepl("?10,000",BXTdata$p_gross_household, fixed=TRUE)
                              | grepl("?15,000",BXTdata$p_gross_household, fixed=TRUE)
                              , "Level 1 0~19k",
                              ifelse(
                                grepl("?20,000",BXTdata$p_gross_household, fixed=TRUE)
                                | grepl("?25,000",BXTdata$p_gross_household, fixed=TRUE)
                                | grepl("?30,000",BXTdata$p_gross_household, fixed=TRUE)
                                | grepl("?35,000",BXTdata$p_gross_household, fixed=TRUE)
                                , "Level 2 20~39k",
                                ifelse(
                                  grepl("?40,000",BXTdata$p_gross_household, fixed=TRUE)
                                  | grepl("?45,000",BXTdata$p_gross_household, fixed=TRUE)
                                  | grepl("?50,000",BXTdata$p_gross_household, fixed=TRUE)
                                  , "Level 3 40~59k",
                                  ifelse(
                                    grepl("?60,000",BXTdata$p_gross_household, fixed=TRUE)
                                    | grepl("?70,000",BXTdata$p_gross_household, fixed=TRUE)
                                    , "Level 4 60~70k",
                                    ifelse(
                                      grepl("?100,000",BXTdata$p_gross_household, fixed=TRUE)
                                      | grepl("?150,000",BXTdata$p_gross_household, fixed=TRUE)
                                      , "Level 5 100k or above",NA
                                    )
                                  )
                                )
                              )
                              
)
                              
                              
                            

#----------------------Recode Education----------------------#

BXTdata$p_education_age <- as.character(BXTdata$p_education_age)

BXTdata$Education <- ifelse(BXTdata$p_education_age =="15 or under"
                            | BXTdata$p_education_age == "16"
                            , "16 or under",
                            ifelse(BXTdata$p_education_age =="17-18"
                                   | BXTdata$p_education_age == "19"
                                   , "17-19",
                                   ifelse(BXTdata$p_education_age =="20+", "20 or above",NA)))

#----------------------------Aggregate Dataset----------------------------#
BXTdata$NewEncode <- as.factor(BXTdata$NewEncode)
BXTdata$wave <- as.numeric(as.character(BXTdata$wave))
BXTdata$NewVote <- as.factor(BXTdata$NewVote)
BXTdata$NewStatus <- as.factor(BXTdata$NewStatus)


#----------------------Convert Wave to Date----------------------#

BXTDates <- read.csv(paste0(data_path, "BrexitWaveDates.csv"))

colnames(BXTDates) <- c("wave","date")

BXTDates$date <- as.Date(BXTDates$date, format = "%m/%d/%Y")

#GTDdata <- merge(x = aggData, y = BXTDates, by = "wave", all.x = TRUE)
GTDdata <- merge(x = BXTdata, y = BXTDates, by = "wave", all.x = TRUE)

#----------------------Compute AgeGroup----------------------#

GTDdata$AgeGroup <- ifelse(GTDdata$age ==35 , "Age19-35",
                           ifelse(GTDdata$age >35 & GTDdata$age <=45, "Age36-45",
                                  ifelse(GTDdata$age >45 & GTDdata$age <=65, "Age46-65",
                                         ifelse(GTDdata$age >65 & GTDdata$age <=85, "Age66-85","AgeOver85"))))


#----------------------Remove people with NAs in Education and Income---------------------#
GTDdata <- GTDdata[!is.na(GTDdata$GrossIncome),]
GTDdata <- GTDdata[!is.na(GTDdata$Education),]

#----------------------Load & Encode the skill variable---------------------#

SkillData <- read.csv(paste0(data_path, "Brexit_skill.csv"))

colnames(SkillData)[which(names(SkillData) == "?..id")] <- "id"

SkillData$FinalOccupation <- ifelse(!is.na(SkillData$ns_sec_analyticW20), SkillData$ns_sec_analyticW20,
                                    ifelse(!is.na(SkillData$ns_sec_analyticW19), SkillData$ns_sec_analyticW19,
                                           ifelse(!is.na(SkillData$ns_sec_analyticW16W17W18), SkillData$ns_sec_analyticW16W17W18,
                                                  ifelse(!is.na(SkillData$ns_sec_analyticW6W7W8W9),SkillData$ns_sec_analyticW6W7W8W9,
                                                         ifelse(!is.na(SkillData$ns_sec_analyticW1W2W3W4W5),SkillData$ns_sec_analyticW1W2W3W4W5,NA)))))

SkillData <- SkillData[!is.na(SkillData$FinalOccupation),]

SkillData$Skill <- ifelse(SkillData$FinalOccupation == 70, "Level 1",
                               ifelse(SkillData$FinalOccupation == 60, "Level 2",
                                      ifelse(SkillData$FinalOccupation == 50, "Level 3",
                                             ifelse(SkillData$FinalOccupation == 40, "Level 4",
                                                    ifelse(SkillData$FinalOccupation == 30, "Level 5",
                                                           ifelse(SkillData$FinalOccupation == 20, "Level 6",
                                                                  ifelse(SkillData$FinalOccupation == 12, "Level 7","Level 8")))))))

GTDdata <- merge(x = GTDdata, y = SkillData, by = "id", all.x = TRUE)

GTDdata <- GTDdata[!is.na(GTDdata$Skill),]


#----------------------Pre-define options to the users----------------------#


# Options
#For X-axis
XAxisOptions <- c("Education" = "Education",
                  "Gross Household Income" = "GrossIncome",
                  "Skill" = "Skill")

#Temporal display options
TemporalOptions <- c("Proportion" = "Proportion",
               "Count" = "Count")

FilterOptions <- c("None" = "None",
                   "Gender" = "Gender",
                   "Country" = "Country",
                   "Marriage" = "Marriage",
                   "Age"="AgeGroup",
                   "Wave"="WaveGroup")


colorOptions <- c("None" = "none",
                  "Region" = "Region",
                  "Religion" = "Religion")

SESOptions <- c("None" = "none",
                  "Income" = "GrossIncome",
                  "Education" = "Education")


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