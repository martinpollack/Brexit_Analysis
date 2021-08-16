library(dplyr)
library(readr)

# Load Dataset
#data_path = "Insert datapath"
data_path = "~/Documents/Brexit_Analysis/updated_app/"

BXTdata <- read.csv(paste0(data_path, "processed_cross_sectional_data.csv"))


BXTdata$NewEncode <- ifelse(BXTdata$NewStatus == "Always Stay", "AS",
                           ifelse(BXTdata$NewStatus == "Always Leave", "AL",
                                  ifelse(BXTdata$NewStatus == "Switch to Leave", "SL","SS")))


BXTdata$NewEncode <- as.factor(BXTdata$NewEncode)
BXTdata$wave <- as.numeric(as.character(BXTdata$wave))
BXTdata$NewVote <- as.factor(BXTdata$NewVote)
BXTdata$NewStatus <- as.factor(BXTdata$NewStatus)


agg1 <- BXTdata %>% 
     group_by(wave,NewEncode) %>%
     summarise(AIS_mean=mean(AIS, na.rm=T),AIS_sd=sd(AIS, na.rm=T))

agg2 <- BXTdata %>% 
  group_by(wave,NewEncode) %>%
  summarise(n = n()) %>%
  mutate(Percentage = prop.table(n/sum(n)))



agg <- merge(agg1,agg2)



aggData <- dplyr::arrange(agg,wave,NewEncode)

GTDdata <- aggData


# Options
#For X-axis
XAxisOptions <- c("All" = "all",
                  "Always Stay" = "AS",
                  "Always Leave" = "AL",
                  "Switch to Stay" = "SS",
                  "Switch to Leave" = "SL")

#Temporal display options
TemporalOptions <- c("Percentage" = "Percentage",
               "Count" = "n")







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