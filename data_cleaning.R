library(tidyverse)

setwd("~/Documents/Brexit_Analysis")

### Read in raw data
raw <- read.csv("BES_2019_W20_Panel_Final.csv")

### Keep important variables
smaller <- raw %>%
  select("id",
         starts_with("country"),
         starts_with("euRefVoteW"), "euRefVoteAfterW20",
         starts_with("immigrationLevel"),
         starts_with("immigEcon"),
         starts_with("immigCultural"),
         starts_with("inequalityChangeW"),
         starts_with("immigrantsWelfareState"),
         starts_with("changeImmigW"),
         starts_with("p_country_birth"),
         "gender",
         starts_with("age"),
         starts_with("p_education_age"),
         starts_with("p_housing"),
         starts_with("p_marital"),
         starts_with("p_ethnicity"),
         starts_with("p_gross_household"),
         starts_with("p_gross_personal"),
         starts_with("p_past_vote"),
         starts_with("p_hh_children"),
         # daily newspaper
         # religiosity
         starts_with("p_religion"),
         # type of organization worked for
         starts_with("wave"),
         # weight variables
         )

### Make wave a variable, making data longer
smaller <- smaller %>% 
  pivot_longer(starts_with("wave"), names_to="wave", values_to="in_wave")%>% 
  filter(in_wave==1) %>% 
  select(!c(in_wave, ageGroup, Age))
smaller$wave <- parse_number(smaller$wave)

### Create one column for each predictor, 
###    taking the value from the correct wave

# fix inconsistency
colnames(smaller)[colnames(smaller)=="euRefVoteAfterW20"] <- "euRefVoteW20"
col_names = colnames(smaller)

# get new column names
new_column_names <- col_names[!((col_names %in% c("id", "gender", "wave")) | (grepl("^p_past_vote", col_names)))] %>%
  str_replace("W(\\d+)", "") %>%
  unique()

# fill in new columns
for (new_col in new_column_names) {
  # get wave numbers of old columns
  wave_nums <- parse_number(col_names[grepl(paste0("^", new_col), col_names)])
  
  for (wave in wave_nums) {
    smaller[smaller$wave == wave, new_col] <- smaller[smaller$wave == wave, paste0(new_col, "W", wave)]
  }
}

# remove old columns, reorder new ones
data <- smaller %>%
  select(!matches(".W(\\d+)")) %>%
  select(id, 
         wave,
         country,
         gender,
         age,
         euRefVote,
         starts_with("immig"),
         contains("change"),
         starts_with("p_"))

### Find/replace certain values
data$country[data$country == 1] <- "England"
data$country[data$country == 2] <- "Scotland"
data$country[data$country == 3] <- "Wales"

data$immigrationLevel[data$immigrationLevel == "Decreased a lot"] <- 1
data$immigrationLevel[data$immigrationLevel == "Decreased a little"] <- 2
data$immigrationLevel[data$immigrationLevel == "Left the same as it is now"] <- 3
data$immigrationLevel[data$immigrationLevel == "Increased a little"] <- 4
data$immigrationLevel[data$immigrationLevel == "Increased a lot"] <- 5
data$immigrationLevel[data$immigrationLevel == "Don't know"] <- NA
data$immigrationLevel <- as.numeric(data$immigrationLevel)

data$immigEcon[data$immigEcon == "Bad for economy"] <- 1
data$immigEcon[data$immigEcon == "Good for economy"] <- 7
data$immigEcon[data$immigEcon == "Don't know"] <- NA
data$immigEcon <- as.numeric(data$immigEcon)

data$immigCultural[data$immigCultural == "Undermines cultural life"] <- 1
data$immigCultural[data$immigCultural == "Enriches cultural life"] <- 7
data$immigCultural[data$immigCultural == "Don't know"] <- NA
data$immigCultural <- as.numeric(data$immigCultural)

data$immigrantsWelfareState[data$immigrantsWelfareState == "Strongly disagree"] <- 1
data$immigrantsWelfareState[data$immigrantsWelfareState == "Disagree"] <- 2
data$immigrantsWelfareState[data$immigrantsWelfareState == "Neither agree nor disagree"] <- 3
data$immigrantsWelfareState[data$immigrantsWelfareState == "Agree"] <- 4
data$immigrantsWelfareState[data$immigrantsWelfareState == "Strongly agree"] <- 5
data$immigrantsWelfareState[data$immigrantsWelfareState == "Don't know"] <- NA
data$immigrantsWelfareState <- as.numeric(data$immigrantsWelfareState)

data$changeImmig[data$changeImmig == "Getting a lot lower"] <- 1
data$changeImmig[data$changeImmig == "Getting a little lower"] <- 2
data$changeImmig[data$changeImmig == "Staying about the same"] <- 3
data$changeImmig[data$changeImmig == "Getting a little higher"] <- 4
data$changeImmig[data$changeImmig == "Getting a lot higher"] <- 5
data$changeImmig[data$changeImmig == "Don't know"] <- NA
data$changeImmig <- as.numeric(data$changeImmig)


### Save results
write.csv(data, file="cleaned.csv")

