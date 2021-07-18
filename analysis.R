library(dplyr)
library(ggplot2)
library(corrplot)

setwd("~/Documents/Brexit_Analysis")

### read in data
data <- read.csv("cleaned.csv")[,-1]

### look at how many waves people are in
data %>% 
  group_by(id) %>%
  count() %>%
  ggplot(aes(n)) + 
  geom_histogram(binwidth=1) + 
  labs(x="# of waves participated")


### Create Anti-Immigrantion Sentiment (AIS) variable
valid_immig <- data[(data$immigEcon > 0) & (data$immigCultural > 0) & (data$immigrantsWelfareState > 0), c("immigEcon", "immigCultural", "immigrantsWelfareState")]

# with changeImmig
eigen(cor(data[(data$immigEcon > 0) & (data$immigCultural > 0) & (data$immigrantsWelfareState > 0) & (data$changeImmig > 0), c("immigEcon", "immigCultural", "immigrantsWelfareState", "changeImmig")], use = "pairwise.complete.obs"))

# without changeImmig
corr_matrix = cor(valid_immig, use = "pairwise.complete.obs")
pca = eigen(corr_matrix)

model_welf = lm(immigrantsWelfareState ~ immigEcon + immigCultural, valid_immig)

model_econ = lm(immigEcon ~ immigrantsWelfareState, valid_immig)
model_cult = lm(immigCultural ~ immigrantsWelfareState, valid_immig)

#data$AIS = ifelse(is.na(data$immigrantsWelfareState),
                  #pca$vectors[1,1]*scale(data$immigEcon) + pca$vectors[2,1]*scale(data$immigCultural) + pca$vectors[3,1]*scale(data$immigrantsWelfareState),
                  #pca$vectors[1,1]*scale(data$immigEcon) + pca$vectors[2,1]*scale(data$immigCultural) + pca$vectors[3,1]*scale(data$immigrantsWelfareState))
