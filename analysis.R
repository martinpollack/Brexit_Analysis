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

### Look at aggregate statistics for each participant
data %>% 
  group_by(id) %>%
  count() %>%
  ggplot(aes(n)) + 
  geom_histogram(binwidth=1) + 
  labs(x="# of waves participated")
