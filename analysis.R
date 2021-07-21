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
corr_matrix = cor(data[c("immigEcon", "immigCultural", "immigrantsWelfareState", "changeImmig")], use = "pairwise.complete.obs")
pca = eigen(corr_matrix)

# Build regressors for data imputation
model_welf = lm(immigrantsWelfareState ~ immigEcon + immigCultural, valid_immig)
model_econ = lm(immigEcon ~ immigrantsWelfareState, valid_immig)
model_cult = lm(immigCultural ~ immigrantsWelfareState, valid_immig)

