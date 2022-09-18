#Load the data

Dataset = read.csv(".\\Brexit Analysis\\BrexitApp2\\Education data.csv")

##-----------------Generate Plotly BarChart------------------------##

library(plotly)
library(dplyr)

dataLeave <- Dataset[Dataset$NewVote=="Leave",]
dataStay <- Dataset[Dataset$NewVote=="Stay",]

mergedata <- dataLeave[,c("Education","Proportion")]

colnames(mergedata)[which(names(mergedata) == "Proportion")] <- "Leave"

mergedata <- left_join(mergedata, dataStay[,c("Education","Proportion")])

colnames(mergedata)[which(names(mergedata) == "Proportion")] <- "Stay"

##----------------Process Information ----------------------------##

fig <- plot_ly(mergedata, x = ~Education, y = ~Leave, type = 'bar', name = "Leave")

fig <- fig %>% add_trace(y = ~Stay, name = 'Stay')

fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

fig