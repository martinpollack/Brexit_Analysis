
Selectdata <- select(filteredGTD,c("Education","NewVote","Count","Proportion","WaveGroup"))


currentData <- Selectdata

dat <- currentData

dataLeave <- dat[dat$NewVote=="Leave",]
dataStay <- dat[dat$NewVote=="Stay",]

mergedata <- dataLeave[,c("Education","Proportion")]

colnames(mergedata)[which(names(mergedata) == "Proportion")] <- "Leave"

mergedata <- left_join(mergedata, dataStay[,c("Education","Proportion")])

colnames(mergedata)[which(names(mergedata) == "Proportion")] <- "Stay"



currentVis <- plot_ly(mergedata, x = ~Education, y = ~Leave, type = 'bar', name = "Leave")

currentVis <- currentVis %>% add_trace(y = ~Stay, name = 'Stay')

currentVis <- currentVis %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

currentVis