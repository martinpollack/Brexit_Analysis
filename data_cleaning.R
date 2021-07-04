### Intial set-up
library(tidyverse)

setwd("~/Documents/Brexit_Analysis")

### Read in raw data
data <- read.csv("BES2019_W20_Panel_Final.csv")

### Keep important variables
smaller <- data %>%
  select("id",
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
smaller <- smaller %>%
  select(!matches(".W(\\d+)")) %>%
  select(id, 
         wave, 
         gender, 
         age, 
         euRefVote,
         starts_with("immig"),
         contains("change"),
         starts_with("p_"))

### Save results
write.csv(smaller, file="cleaned.csv")

