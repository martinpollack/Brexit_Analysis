library(tidyverse)
library(shiny)
library(plotly)
ulti_dat <- read.csv("Brexit_data.csv")
ulti_plot <- read.csv("ultimate_plot.csv")
ulti_dat$bi_voter_type <- factor(ulti_dat$bi_voter_type, levels = c("0.Stay/remain in the EU",
                                                                "1.Stay/remain in the EU",
                                                                "1.Leave the EU",
                                                                "0.Leave the EU",
                                                                "0.Don't know",
                                                                "1.Don't know"))


