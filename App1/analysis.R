library(dplyr)
library(ggplot2)
library(corrplot)

setwd("~/Documents/Brexit_Analysis")

### read in data
data <- read.csv("cleaned.csv")[,-1]


### Create Anti-Immigrantion Sentiment (AIS) variable

# Four survey questions seem to contain important information about immigration sentiment
# - immigEcon
# - immigCultural
# - immigrantsWelfareState
# - changeImmig

# We will incorporate these four values into one AIS score, averaging sort of "Z-scores" from each

# Since the variables have quite high correlations with one another,
#   we will handle missing values by averaging values from variables that an individual does have
corr_matrix = cor(data[c("immigEcon", "immigCultural", "immigrantsWelfareState", "immigrationLevel")], use = "pairwise.complete.obs")

data$Z_Econ = -(data$immigEcon - 4) / 3
data$Z_Cult = -(data$immigCultural - 4) / 3
data$Z_Welf =  (data$immigrantsWelfareState - 3) / 2
data$Z_Lvl  = -(data$immigrationLevel - 3) / 2

data$AIS = apply(data[(c("Z_Econ", "Z_Cult", "Z_Welf", "Z_Lvl"))],
                 1,
                 mean,
                 na.rm=T)

### Get weighted AIS score
data$AIS.wt <- data$wt * data$AIS

### Look at aggregate statistics for each year
data$newRefVote = case_when(data$euRefVote == "Rejoin the EU" ~ "Stay/remain in the EU",
                            data$euRefVote == "Stay out of the EU" ~ "Leave the EU",
                            TRUE ~ data$euRefVote)

### Look at aggregate statistics for each participant

# Calculate AIS standard deviation by id
agg = data %>% 
  group_by(id) %>%
  summarise(AIS_sd=sd(AIS, na.rm=T),
            AIS_mean=mean(AIS, na.rm=T),
            num_waves=n())

# Calculate the percentage of times an id switched, and how long they were in each camp
for (id_num in agg$id) {
  subset = data %>%
    filter(id == id_num, (newRefVote == "Stay/remain in the EU") | (newRefVote == "Leave the EU")) %>%
    arrange(wave) %>%
    select(newRefVote)
  
  n = nrow(subset)
  
  if (n >= 2) {
    
    switched = 0
    stay = 0
    leave = 0
    
    last_ref = subset[[1, 1]]
    if (last_ref == "Stay/remain in the EU") {
      stay = stay + 1
    }
    else {
      leave = leave + 1
    }

    for (cur_ref in subset[-1, 1]) {
      if (cur_ref != last_ref) {
        switched = switched + 1
      }
      
      if (cur_ref == "Stay/remain in the EU") {
        stay = stay + 1
      }
      else {
        leave = leave + 1
      }
      
      last_ref = cur_ref
    }
    
    agg[agg$id == id_num, "Switch_prop"] = switched / (n - 1)
    agg[agg$id == id_num, "Stay_prop"] = stay / n
    agg[agg$id == id_num, "Leave_prop"] = leave / n
      
  }
    
}
  
