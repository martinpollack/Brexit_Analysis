library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(plotly)
library(fastDummies)
library(stringr)
#library(DataCombine)

#setwd("~/Brexit Analysis/Brexit Shiny")
source('initialize.R')
print("Finished initializing")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#
ui <- navbarPage("Brexit Data Visualization",
                 
                 #==================
                 #Cross-Sectional Visualization
                 #==================
                 tabPanel(span("Brexit Visualization" , style = "color:#1369bf"), fluidPage(
                   titlePanel(htmlOutput("ScatterTitle")),
                   #div("Brexit Data Time-series - ", style = "color:#1369bf", align = "center")),    
                   fluidRow(uiOutput("formula")), 
                   fluidRow(
                     
                     # Sidebar tabs
                     tabsetPanel(
                       tabPanel("Reduced Models", 
                                column(4, 
                                       wellPanel( #RENAME PANEL TAB
                                         
                                         #X-axis (Display variables, warnings, log option)
                                         fluidRow(column(9, selectInput("ScatterXaxis", "Control Variable", choices = XAxisOptions))),
                                         
                                         
                                         #Y-axis (GTD variables, warnings, log option)
                                         #fluidRow(column(9, selectInput("ScatterYaxis", "Display Option", choices = TemporalOptions))),
                                         
                                         
                                         #Type of scatterplot and conditional faceting
                                         radioButtons("ScatterPlotType", label="Type of Plot", inline=TRUE,
                                                      choices=c("plotly" = "plotly", "ggplot" = "ggplot"),
                                                      selected="plotly"),
                                         
                                         #Filter by variables
                                         #fluidRow(column(6, selectInput("ScatterFilterby", "Facet By", choices = FilterOptions))),
                                         
                                         
                                         #Color by and Success
                                         #fluidRow(column(6, selectInput("ScatterColorby", "Color By", choices = colorOptions))),
                                         
                                         #Sliders for wave
                                         
                                         #sliderInput("ScatterYear", "Year of Survey", 1, 20, 
                                         #           value = c(1, 20), sep=""),
                                         
                                         #Sliders for year
                                         sliderInput("ScatterYear",
                                                     "Dates of Survey:",
                                                     min = as.Date("2014-01-01","%Y-%m-%d"),
                                                     max = as.Date("2020-12-31","%Y-%m-%d"),
                                                     value=as.Date(c("2014-01-01","2020-12-31")),
                                                     timeFormat="%Y-%m-%d"),
                                         
                                         downloadButton("DownloadingData", "Download")
                                       )
                                ),
                                
                                #Output
                                column(8, conditionalPanel(condition="input.ScatterPlotType == 'plotly'",
                                                           plotlyOutput("graph1", height = "500px")),
                                       conditionalPanel(condition="input.ScatterPlotType == 'ggplot'",
                                                        plotOutput("ggplot1", height = "500px"))
                                )
                       ),
                       
                       # HTML("Additional resources for instructors: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/'>Stats2Labs</a>")
                       
                       tabPanel("Full Model", 
                                column(4,
                                       wellPanel(
                                         selectInput("Edu", "Education Years", choices = c("<17", "17-19", ">19")),
                                         selectInput("Gross", "Gross Income", choices = c("0-19k", "20-39k", "40-59k", "60-99k", "100k or above")),
                                         selectInput("Skill", "Skill Level", choices = c("Skill Level 1", "Skill Level 2", "Skill Level 3", "Skill Level 4", "Skill Level 5", "Skill Level 6", "Skill Level 7", "Skill Level 8")),
                                         selectInput("Age", "Age", choices = c("19-35", "36-45", "46-65", "66-85", "Over85")),
                                         selectInput("Marriage", "Marital Status", choices = c("Married", "Single")),
                                         selectInput("Country", "Country", choices = c("England", "Scottland", "Wales")),
                                         selectInput("Gender", "Gender", choices = c("Female", "Male")),
                                         sliderInput("WeightedAIS", "AIS Score", min = -1, max = 1, value = 0, step = 0.02)
                                       )
                                ),
                                
                                column(8, plotlyOutput("full.graph", height = "500px"))
                       )
                       
                     )
                   )
                 ))
                 
)
#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#

CalcTotalByFacet <- function(RiverFacets = "none"){
  #Number of Incidents, Fatalities, and Wounded by Years
  FacetVariables <- "Year"
  if(RiverFacets != "none"){
    FacetVariables <- c(FacetVariables, RiverFacets)
  }
  
  IncidentsByYears <- ddply(GTDdata, FacetVariables, summarise, 
                            total=length(Success))
  FatalitiesByYears <- ddply(GTDdata, FacetVariables, summarise, 
                             total=sum(Fatalities, na.rm = TRUE))
  WoundedByYears <- ddply(GTDdata, FacetVariables, summarise, 
                          total=sum(Wounded, na.rm = TRUE))
  
  VariablesByYear <- list("Incidents" = IncidentsByYears,
                          "Fatalities" = FatalitiesByYears,
                          "Wounded" = WoundedByYears)
}

###############################################################################



###############################################################################
#               DATA PROCESSING AND OUTPUT
# Uses the data from GTD and Gapminder and user input to 
#   output the correct graphics.
###############################################################################


# Define server logic 
server <- function(input, output, clientData, session) {
  #############################################################################  
  #     SCATTERPLOT CODE
  #############################################################################
  
  # A function to filter the data depending on user's input
  prepareCurrentData <- reactive({
    
    #----------------------Construct Bar Chart Data Aquisiton Here-------------------#
    
    #Filtering the GTDdata
    BXTDates <- BXTDates
    
    #Change the colnames in GTDdata
    colnames(GTDdata)[which(names(GTDdata) == "gender")] <- "Gender"
    colnames(GTDdata)[which(names(GTDdata) == "country")] <- "Country"
    
    #Perform the bar chart computation here
    Bardata <- merge(x = GTDdata, y = BXTDates, by = "wave", all.x = TRUE)
    
    
    #Select bar charts data by year
    Bardata <- Bardata[Bardata$date.x >= input$ScatterYear[1] & 
                         Bardata$date.x <= input$ScatterYear[2],]
    
    Bardata <- Bardata[!is.na(Bardata$AgeGroup),]
    
    RegressionData <- select(Bardata, c("Education" = "Education",
                                        "Gross Household Income" = "GrossIncome",
                                        "Skill" = "Skill",
                                        "Age" = "AgeGroup",
                                        "Marrital Status" = "Marriage",
                                        "Country" = "Country",
                                        "Gender" = "Gender",
                                        "WeightedAIS" = "WeightedAIS",
                                        "NewLeaveVote" = "NewLeaveVote"))
    
    DummyRegressData <- dummy_cols(RegressionData,remove_first_dummy = TRUE)
    
    DummyRegressData <- DummyRegressData[ -c(1:7)]
    
    ##Change the variable names for GLM
    colnames(DummyRegressData)[3] <- "Edu2"
    colnames(DummyRegressData)[4] <- "Edu3"
    
    colnames(DummyRegressData)[5] <- "Gross2"
    colnames(DummyRegressData)[6] <- "Gross3"
    colnames(DummyRegressData)[7] <- "Gross4"
    colnames(DummyRegressData)[8] <- "Gross5"
    
    colnames(DummyRegressData)[9] <- "Skill2"
    colnames(DummyRegressData)[10] <- "Skill3"
    colnames(DummyRegressData)[11] <- "Skill4"
    colnames(DummyRegressData)[12] <- "Skill5"
    colnames(DummyRegressData)[13] <- "Skill6"
    colnames(DummyRegressData)[14] <- "Skill7"
    colnames(DummyRegressData)[15] <- "Skill8"
    
    colnames(DummyRegressData)[16] <- "Age2"
    colnames(DummyRegressData)[17] <- "Age3"
    colnames(DummyRegressData)[18] <- "Age4"
    colnames(DummyRegressData)[19] <- "Age5"
    
    colnames(DummyRegressData)[20] <- "MarriageSingle"
    
    colnames(DummyRegressData)[21] <- "Country2"
    colnames(DummyRegressData)[22] <- "Country3"
    
    colnames(DummyRegressData)[23] <- "GenderMale"
    
    
    DummyRegressData
  })
  
  fitReducedModels <- reactive({
    DummyRegressData <- prepareCurrentData()
    
    model0 <- glm(NewLeaveVote ~.,family=binomial(link='logit'),data=DummyRegressData)
    
    column.percentages <- colMeans(DummyRegressData[,3:ncol(DummyRegressData)])
    
    predictData <- data.frame(as.list(column.percentages))
    
    #colnames(predictData) <- str_replace_all(colnames(predictData), "\\.", " ")
    
    #colnames(predictData)[1] <- "Education_17-19"
    
    #colnames(predictData)[14:16] <- c("Age_Age36-45",
    #                                  "Age_Age46-65",
    #                                  "Age_Age66-85")
    
    #colnames(predictData)[3:5] <- c("Gross Household Income_Level 2 20~39k", 
    #                                "Gross Household Income_Level 3 40~59k",
    #                                "Gross Household Income_Level 4 60~99k")
    
    predictData <- do.call("rbind", replicate(101, predictData, simplify = FALSE))
    
    predictData$WeightedAIS <- seq(-1, 1, length.out = 101)
    
    PredictResults <- predict(model0, newdata = predictData, type = "response")
    
    PredictResults <- as.data.frame(PredictResults)
    
    if (input$ScatterXaxis == "None") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS,family=binomial(link='logit'),data=DummyRegressData)
      PredictResults <- predict(model, newdata = predictData, type = "response")
      PredictResults <- as.data.frame(PredictResults)
      
    }
    
    else if(input$ScatterXaxis == "Education") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Edu2 + Edu3,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Edu2"] <- 0
      predictData1[,"Edu3"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Edu2"] <- 1
      predictData2[,"Edu3"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Edu2"] <- 0
      predictData3[,"Edu3"] <- 1
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      PredictResults$Edu1 <- PredictResults11
      PredictResults$Edu2 <- PredictResults12
      PredictResults$Edu3 <- PredictResults13
    }
    
    
    else if(input$ScatterXaxis == "GrossIncome") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Gross2 + Gross3 + Gross4 + Gross5,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Gross2"] <- 0
      predictData1[,"Gross3"] <- 0
      predictData1[,"Gross4"] <- 0
      predictData1[,"Gross5"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Gross2"] <- 1
      predictData2[,"Gross3"] <- 0
      predictData2[,"Gross4"] <- 0
      predictData2[,"Gross5"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Gross2"] <- 0
      predictData3[,"Gross3"] <- 1
      predictData3[,"Gross4"] <- 0
      predictData3[,"Gross5"] <- 0
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      predictData4 <- predictData
      predictData4[,"Gross2"] <- 0
      predictData4[,"Gross3"] <- 0
      predictData4[,"Gross4"] <- 1
      predictData4[,"Gross5"] <- 0
      
      PredictResults14 <- predict(model, newdata = predictData4, type = "response")
      
      predictData5 <- predictData
      predictData5[,"Gross2"] <- 0
      predictData5[,"Gross3"] <- 0
      predictData5[,"Gross4"] <- 0
      predictData5[,"Gross5"] <- 1
      
      PredictResults15 <- predict(model, newdata = predictData5, type = "response")      
      
      PredictResults$Gross1 <- PredictResults11
      PredictResults$Gross2 <- PredictResults12
      PredictResults$Gross3 <- PredictResults13
      PredictResults$Gross4 <- PredictResults14
      PredictResults$Gross5 <- PredictResults15
      
    }
    
    else if(input$ScatterXaxis == "Skill") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Skill2 + Skill3 + Skill4 + Skill5 + Skill6 + Skill7 + Skill8,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Skill2"] <- 0
      predictData1[,"Skill3"] <- 0
      predictData1[,"Skill4"] <- 0
      predictData1[,"Skill5"] <- 0
      predictData1[,"Skill6"] <- 0
      predictData1[,"Skill7"] <- 0
      predictData1[,"Skill8"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Skill2"] <- 1
      predictData2[,"Skill3"] <- 0
      predictData2[,"Skill4"] <- 0
      predictData2[,"Skill5"] <- 0
      predictData2[,"Skill6"] <- 0
      predictData2[,"Skill7"] <- 0
      predictData2[,"Skill8"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Skill2"] <- 0
      predictData3[,"Skill3"] <- 1
      predictData3[,"Skill4"] <- 0
      predictData3[,"Skill5"] <- 0
      predictData3[,"Skill6"] <- 0
      predictData3[,"Skill7"] <- 0
      predictData3[,"Skill8"] <- 0
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      predictData4 <- predictData
      predictData4[,"Skill2"] <- 0
      predictData4[,"Skill3"] <- 0
      predictData4[,"Skill4"] <- 1
      predictData4[,"Skill5"] <- 0
      predictData4[,"Skill6"] <- 0
      predictData4[,"Skill7"] <- 0
      predictData4[,"Skill8"] <- 0
      
      PredictResults14 <- predict(model, newdata = predictData4, type = "response")
      
      predictData5 <- predictData
      predictData5[,"Skill2"] <- 0
      predictData5[,"Skill3"] <- 0
      predictData5[,"Skill4"] <- 0
      predictData5[,"Skill5"] <- 1
      predictData5[,"Skill6"] <- 0
      predictData5[,"Skill7"] <- 0
      predictData5[,"Skill8"] <- 0
      
      PredictResults15 <- predict(model, newdata = predictData5, type = "response")
      
      predictData6 <- predictData
      predictData6[,"Skill2"] <- 0
      predictData6[,"Skill3"] <- 0
      predictData6[,"Skill4"] <- 0
      predictData6[,"Skill5"] <- 0
      predictData6[,"Skill6"] <- 1
      predictData6[,"Skill7"] <- 0
      predictData6[,"Skill8"] <- 0
      
      PredictResults16 <- predict(model, newdata = predictData6, type = "response")
      
      predictData7 <- predictData
      predictData7[,"Skill2"] <- 0
      predictData7[,"Skill3"] <- 0
      predictData7[,"Skill4"] <- 0
      predictData7[,"Skill5"] <- 0
      predictData7[,"Skill6"] <- 0
      predictData7[,"Skill7"] <- 1
      predictData7[,"Skill8"] <- 0
      
      PredictResults17 <- predict(model, newdata = predictData7, type = "response")
      
      predictData8 <- predictData
      predictData8[,"Skill2"] <- 0
      predictData8[,"Skill3"] <- 0
      predictData8[,"Skill4"] <- 0
      predictData8[,"Skill5"] <- 0
      predictData8[,"Skill6"] <- 0
      predictData8[,"Skill7"] <- 0
      predictData8[,"Skill8"] <- 1
      
      PredictResults18 <- predict(model, newdata = predictData8, type = "response")
      
      PredictResults$Skill1 <- PredictResults11
      PredictResults$Skill2 <- PredictResults12
      PredictResults$Skill3 <- PredictResults13
      PredictResults$Skill4 <- PredictResults14
      PredictResults$Skill5 <- PredictResults15
      PredictResults$Skill6 <- PredictResults16
      PredictResults$Skill7 <- PredictResults17
      PredictResults$Skill8 <- PredictResults18
      
    }
    
    else if(input$ScatterXaxis == "AgeGroup") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Age2 + Age3 + Age4 + Age5,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Age2"] <- 0
      predictData1[,"Age3"] <- 0
      predictData1[,"Age4"] <- 0
      predictData1[,"Age5"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Age2"] <- 1
      predictData2[,"Age3"] <- 0
      predictData2[,"Age4"] <- 0
      predictData2[,"Age5"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Age2"] <- 0
      predictData3[,"Age3"] <- 1
      predictData3[,"Age4"] <- 0
      predictData3[,"Age5"] <- 0
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      predictData4 <- predictData
      predictData4[,"Age2"] <- 0
      predictData4[,"Age3"] <- 0
      predictData4[,"Age4"] <- 1
      predictData4[,"Age5"] <- 0
      
      PredictResults14 <- predict(model, newdata = predictData4, type = "response")
      
      predictData5 <- predictData
      predictData5[,"Age2"] <- 0
      predictData5[,"Age3"] <- 0
      predictData5[,"Age4"] <- 0
      predictData5[,"Age5"] <- 1
      
      PredictResults15 <- predict(model, newdata = predictData5, type = "response")      
      
      PredictResults$Age1 <- PredictResults11
      PredictResults$Age2 <- PredictResults12
      PredictResults$Age3 <- PredictResults13
      PredictResults$Age4 <- PredictResults14
      PredictResults$Age5 <- PredictResults15
      
    }
    
    else if(input$ScatterXaxis == "Marriage") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + MarriageSingle,family=binomial(link='logit'),data=DummyRegressData)
      
      
      predictData1 <- predictData
      predictData1[,"MarriageSingle"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"MarriageSingle"] <- 1
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      PredictResults$Marriage1 <- PredictResults11
      PredictResults$Marriage2 <- PredictResults12
      
    }
    
    else if(input$ScatterXaxis == "Country") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Country2 + Country3,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Country2"] <- 0
      predictData1[,"Country3"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Country2"] <- 1
      predictData2[,"Country3"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Country2"] <- 0
      predictData3[,"Country3"] <- 1
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      PredictResults$Country1 <- PredictResults11
      PredictResults$Country2 <- PredictResults12
      PredictResults$Country3 <- PredictResults13
      
    }
    
    else if(input$ScatterXaxis == "Gender") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + GenderMale,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"GenderMale"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"GenderMale"] <- 1
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      PredictResults$Gender1 <- PredictResults11
      PredictResults$Gender2 <- PredictResults12
      
    }
    
    
    
    
    #PredictResults <- as.data.frame(PredictResults)
    
    PredictResults$WeightedAIS <- seq(-1, 1, length.out = 101)
    
    PredictResults$Coeff <- sprintf("%.4f",model$coefficients["WeightedAIS"])
    
    PredictResults$Intercept <- sprintf("%.4f",model$coefficients[1])
    
    PredictResults
    
    #    aa <- select(filteredGTD,c("Education","NewVote","Count","Proportion"))
    
    #--------------------------------------End------------------------------------#
  })
  
  fitFullModel <- reactive({
    DummyRegressData <- prepareCurrentData()
    
    model <- glm(NewLeaveVote ~ ., data=DummyRegressData, family=binomial(link="logit"))
    
    PredictData <- expand.grid("Education"=c("Edu1", "Edu2", "Edu3"), 
                               "Gross"=c("Gross1", "Gross2", "Gross3", "Gross4", "Gross5"),
                               "Skill"=c("Skill1", "Skill2", "Skill3", "Skill4", "Skill5", "Skill6", "Skill7", "Skill8"),
                               "Age"=c("Age1", "Age2", "Age3", "Age4", "Age5"),
                               "MarriageStatus"=c("MarriageSingle", "MarriageMarried"),
                               "Country"=c("Country1", "Country2", "Country3"),
                               "Gender"=c("GenderMale", "GenderFemale")
    )
    
    PredictData <- dummy_cols(PredictData, remove_first_dummy=TRUE)
    
    PredictData <- PredictData[ -c(1:7)]
    
    ##Change the variable names for GLM
    colnames(PredictData)[1] <- "Edu2"
    colnames(PredictData)[2] <- "Edu3"
    
    colnames(PredictData)[3] <- "Gross2"
    colnames(PredictData)[4] <- "Gross3"
    colnames(PredictData)[5] <- "Gross4"
    colnames(PredictData)[6] <- "Gross5"
    
    colnames(PredictData)[7] <- "Skill2"
    colnames(PredictData)[8] <- "Skill3"
    colnames(PredictData)[9] <- "Skill4"
    colnames(PredictData)[10] <- "Skill5"
    colnames(PredictData)[11] <- "Skill6"
    colnames(PredictData)[12] <- "Skill7"
    colnames(PredictData)[13] <- "Skill8"
    
    colnames(PredictData)[14] <- "Age2"
    colnames(PredictData)[15] <- "Age3"
    colnames(PredictData)[16] <- "Age4"
    colnames(PredictData)[17] <- "Age5"
    
    colnames(PredictData)[18] <- "MarriageSingle"
    
    colnames(PredictData)[19] <- "Country2"
    colnames(PredictData)[20] <- "Country3"
    
    colnames(PredictData)[21] <- "GenderMale"
    
    PredictData <- PredictData[rep(seq_len(nrow(PredictData)), each=101), ]
    
    PredictData <- cbind("WeightedAIS"=rep(seq(-1, 1, length.out=101), nrow(PredictData)/101), PredictData)
    
    fitted <- predict(model, newdata=PredictData, type="response")
    
    # PredictData <- expand.grid("Education"=c("Edu1", "Edu2", "Edu3"), 
    #                            "Gross"=c("Gross1", "Gross2", "Gross3", "Gross4", "Gross5"),
    #                            "Skill"=c("Skill1", "Skill2", "Skill3", "Skill4", "Skill5", "Skill6", "Skill7", "Skill8"),
    #                            "Age"=c("Age1", "Age2", "Age3", "Age4", "Age5"),
    #                            "MarriageStatus"=c("MarriageSingle", "MarriageMarried"),
    #                            "Country"=c("Country1", "Country2", "Country3"),
    #                            "Gender"=c("GenderMale", "GenderFemale")
    # )
    PredictData$fitted <- fitted
    
    PredictData
  })
  
  output$ScatterTitle <- renderText({
    
    if(input$ScatterPlotType == "plotly"){
      PlotType <- "plotly"
    } else{
      PlotType <- "ggplot"
    }
    
    paste("<div style='color:#1369bf' align = 'center' >
          Brexit Survey Visualization -",
          PlotType,
          "</div>")
  })
  
  #############################################################################  
  #     FORMULA CODE
  #############################################################################
  #Create formula
  
  b <- reactive({
    currentData <- fitReducedModels()
    currentData$Coeff[1]
  })
  
  a <- reactive({
    currentData <- fitReducedModels()
    currentData$Intercept[1]
  })
  
  output$formula <- renderUI(
    withMathJax(helpText(paste0("$$\\hat{p}=\\frac{1}{1+e^{-(", a(), "+", b(), "*AIS+...)}}$$")))
  )
  
  #############################################################################  
  #     PLOTLY CODE
  #############################################################################
  #Function for showing country-year, x, and y information when hovering over 
  #data points
  
  
  
  #A reactive expression with the plotly plot
  output$graph1 <- renderPlotly({
    
    currentData <- fitReducedModels()
    
    if (input$ScatterXaxis == "None") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~PredictResults, data=currentData, type="scatter", mode="lines")
      
    }
    
    else if(input$ScatterXaxis == "Education") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~Edu1, data=currentData, name = 'Edu year 16 or under', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Edu2, data=currentData, name = 'Edu year 17-19', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Edu3, data=currentData, name = 'Edu year 20 or above', type="scatter", mode="lines")
      
    }
    
    else if(input$ScatterXaxis == "GrossIncome") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~Gross1, data=currentData, name = '0-19k', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Gross2, data=currentData, name = '20-39k', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Gross3, data=currentData, name = '40-59k', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Gross4, data=currentData, name = '60-99k', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Gross5, data=currentData, name = '100k or above', type="scatter", mode="lines")
      
    }
    
    else if(input$ScatterXaxis == "Skill") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~Skill1, data=currentData, name = 'Skill Level 1', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Skill2, data=currentData, name = 'Skill Level 2', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Skill3, data=currentData, name = 'Skill Level 3', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Skill4, data=currentData, name = 'Skill Level 4', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Skill5, data=currentData, name = 'Skill Level 5', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Skill6, data=currentData, name = 'Skill Level 6', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Skill7, data=currentData, name = 'Skill Level 7', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Skill8, data=currentData, name = 'Skill Level 8', type="scatter", mode="lines")
      
    }
    
    else if(input$ScatterXaxis == "AgeGroup") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~Age1, data=currentData, name = 'Age19-35', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Age2, data=currentData, name = 'Age36-45', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Age3, data=currentData, name = 'Age46-65', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Age4, data=currentData, name = 'Age66-85', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Age5, data=currentData, name = 'AgeOver85', type="scatter", mode="lines")
      
    }
    
    else if(input$ScatterXaxis == "Marriage") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~Marriage1, data=currentData, name = 'Married', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Marriage2, data=currentData, name = 'Single', type="scatter", mode="lines") 
      
      
    }
    
    else if(input$ScatterXaxis == "Country") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~Country1, data=currentData, name = 'England', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Country2, data=currentData, name = 'Scottland', type="scatter", mode="lines") 
      currentVis <- currentVis %>% add_trace(y=~Country3, data=currentData, name = 'Wales', type="scatter", mode="lines")
      
    }
    
    else if(input$ScatterXaxis == "Gender") {
      
      currentVis <- plot_ly(x=~WeightedAIS, y=~Gender1, data=currentData, name = 'Female', type="scatter", mode="lines")
      currentVis <- currentVis %>% add_trace(y=~Gender2, data=currentData, name = 'Male', type="scatter", mode="lines") 
      
    }
    
    currentVis <- currentVis %>% layout(xaxis = list(title = 'Anti Immigration Score (Normalized)'),
                                        yaxis = list (title = 'Probability of supporting Brexit'))
    
    currentVis <- currentVis %>% layout(xaxis = list(hoverformat = '.2f'))
    currentVis <- currentVis %>% layout(yaxis = list(hoverformat = '.2f'))
    currentVis
    
    #glm(NewLeaveVote ~ , family = "binomial", data=currentData)
    
  })
  
  
  #############################################################################  
  #     GGPLOT CODE
  #############################################################################
  
  #ggplot scatterplot
  output$ggplot1 <- renderPlot({   
    currentData <- fitReducedModels()  
    
    #Preparing titles for axes
    XaxisTitle = input$ScatterXaxis
    YaxisTitle = input$ScatterYaxis
    
    if(input$ScatterXaxis == "None") {
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictResults)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    if(input$ScatterXaxis == "Education") {
      currentData <- currentData %>%
        pivot_longer(starts_with("Edu"), names_to = "ControlVar", values_to = "PredictedByVar")
      currentData$ControlVar <- str_replace_all(currentData$ControlVar, 
                                                c("Edu1"="<17", 
                                                  "Edu2"="17-19",
                                                  "Edu3"=">19"))
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictedByVar, color=ControlVar)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    if(input$ScatterXaxis == "GrossIncome") {
      currentData <- currentData %>%
        pivot_longer(starts_with("Gross"), names_to = "ControlVar", values_to = "PredictedByVar")
      currentData$ControlVar <- str_replace_all(currentData$ControlVar, 
                                                c("Gross1"="0-19k", 
                                                  "Gross2"="20-39k",
                                                  "Gross3"="40-59k",
                                                  "Gross4"="60-99k",
                                                  "Gross5"="100k or above"))
      currentData$ControlVar <- factor(currentData$ControlVar, levels = c("0-19k", "20-39k", "40-59k", "60-70k", "100k or above"))
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictedByVar, color=ControlVar)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    if(input$ScatterXaxis == "Skill") {
      currentData <- currentData %>%
        pivot_longer(starts_with("Skill"), names_to = "ControlVar", values_to = "PredictedByVar")
      currentData$ControlVar <- str_replace_all(currentData$ControlVar, 
                                                c("Skill1"="Skill Level 1", 
                                                  "Skill2"="Skill Level 2",
                                                  "Skill3"="Skill Level 3",
                                                  "Skill4"="Skill Level 4",
                                                  "Skill5"="Skill Level 5",
                                                  "Skill6"="Skill Level 6",
                                                  "Skill7"="Skill Level 7",
                                                  "Skill8"="Skill Level 8"))
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictedByVar, color=ControlVar)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    if(input$ScatterXaxis == "AgeGroup") {
      currentData <- currentData %>%
        pivot_longer(starts_with("Age"), names_to = "ControlVar", values_to = "PredictedByVar")
      currentData$ControlVar[currentData$ControlVar=="Age1"] = "19-35"
      currentData$ControlVar[currentData$ControlVar=="Age2"] = "36-45"
      currentData$ControlVar[currentData$ControlVar=="Age3"] = "46-65"
      currentData$ControlVar[currentData$ControlVar=="Age4"] = "66-85"
      currentData$ControlVar[currentData$ControlVar=="Age5"] = "Over85"
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictedByVar, color=ControlVar)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    if(input$ScatterXaxis == "Marriage") {
      currentData <- currentData %>%
        pivot_longer(starts_with("Marriage"), names_to = "ControlVar", values_to = "PredictedByVar")
      currentData$ControlVar <- str_replace_all(currentData$ControlVar, 
                                                c("Marriage1"="Married", 
                                                  "Marriage2"="Single"))
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictedByVar, color=ControlVar)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    if(input$ScatterXaxis == "Country") {
      currentData <- currentData %>%
        pivot_longer(starts_with("Country"), names_to = "ControlVar", values_to = "PredictedByVar")
      currentData$ControlVar <- str_replace_all(currentData$ControlVar, 
                                                c("Country1"="England", 
                                                  "Country2"="Scottland",
                                                  "Country3"="Wales"))
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictedByVar, color=ControlVar)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    if(input$ScatterXaxis == "Gender") {
      currentData <- currentData %>%
        pivot_longer(starts_with("Gender"), names_to = "ControlVar", values_to = "PredictedByVar")
      currentData$ControlVar <- str_replace_all(currentData$ControlVar, 
                                                c("Gender1"="Female", 
                                                  "Gender2"="Male"))
      currentVis <- ggplot(currentData, aes(x=WeightedAIS, y=PredictedByVar, color=ControlVar)) +
        geom_line() +
        labs(x="Anti Immigration Score (Normalized)", y="Probability of supporting Brexit")
    }
    
    # currentVis <- ggplot(currentData, aes_string(x=input$ScatterXaxis, y="display", fill="NewVote")) +
    #   geom_bar(stat="identity", position=position_dodge()) + 
    #   theme_bw() + 
    #   xlab(XaxisTitle) + ylab(YaxisTitle) + 
    #   theme(axis.title=element_text(size=18)) +
    #   scale_fill_manual("NewVote", values=c("blue", "orange"))
    
    currentVis
  })
  
  # output$ScatterNotes <- renderText({
  #   
  #   if(input$ScatterNumPoints == 0){
  #     
  #   }
  #   
  #   Notes
  # })
  # 
  # output$ScatterNotes_left <- renderText({
  #   
  #   
  # })
  # 
  # output$ScatterNotes_right <- renderText({
  #   
  # })
  
  #############################################################################  
  #     Full model code
  #############################################################################
  output$full.graph <- renderPlotly({
    currentData <- fitFullModel()
    
    if (input$Edu == "<17") {
      currentData <- currentData[currentData$Edu2 == 0 & currentData$Edu3 == 0,]
    } else if (input$Edu == "17-19") {
      currentData <- currentData[currentData$Edu2 == 1,]
    } else {
      currentData <- currentData[currentData$Edu3 == 1,]
    }
    
    if (input$Gross == "0-19k") {
      currentData <- currentData[currentData$Gross2 == 0 & currentData$Gross3 == 0 & currentData$Gross4 == 0 & currentData$Gross5 == 0,]
    } else if (input$Gross == "20-39k") {
      currentData <- currentData[currentData$Gross2 == 1,]
    } else if (input$Gross == "40-59k") {
      currentData <- currentData[currentData$Gross3 == 1,]
    } else if (input$Gross == "60-99k") {
      currentData <- currentData[currentData$Gross4 == 1,]
    } else {
      currentData <- currentData[currentData$Gross5 == 1,]
    }
    
    if (input$Skill == "Skill Level 1") {
      currentData <- currentData[currentData$Skill2 == 0 & currentData$Skill3 == 0 & currentData$Skill4 == 0 & currentData$Skill5 == 0 & currentData$Skill6 == 0 & currentData$Skill7 == 0 & currentData$Skill8 == 0,]
    } else if (input$Skill == "Skill Level 2") {
      currentData <- currentData[currentData$Skill2 == 1,]
    } else if (input$Skill == "Skill Level 3") {
      currentData <- currentData[currentData$Skill3 == 1,]
    } else if (input$Skill == "Skill Level 4") {
      currentData <- currentData[currentData$Skill4 == 1,]
    } else if (input$Skill == "Skill Level 5") {
      currentData <- currentData[currentData$Skill5 == 1,]
    } else if (input$Skill == "Skill Level 6") {
      currentData <- currentData[currentData$Skill6 == 1,]
    } else if (input$Skill == "Skill Level 7") {
      currentData <- currentData[currentData$Skill7 == 1,]
    } else {
      currentData <- currentData[currentData$Skill8 == 1,]
    }
    
    if (input$Age == "19-35") {
      currentData <- currentData[currentData$Age2 == 0 & currentData$Age3 == 0 & currentData$Age4 == 0 & currentData$Age5 == 0,]
    } else if (input$Age == "36-45") {
      currentData <- currentData[currentData$Age2 == 1,]
    } else if (input$Age == "46-65") {
      currentData <- currentData[currentData$Age3 == 1,]
    } else if (input$Age == "66-85") {
      currentData <- currentData[currentData$Age4 == 1,]
    } else {
      currentData <- currentData[currentData$Age5 == 1,]
    }
    
    if (input$Marriage == "Married") {
      currentData <- currentData[currentData$MarriageSingle == 0,]
    } else {
      currentData <- currentData[currentData$MarriageSingle == 1,]
    }
    
    if (input$Country == "England") {
      currentData <- currentData[currentData$Country2 == 0 & currentData$Country3 == 0,]
    } else if (input$Country == "Scottland") {
      currentData <- currentData[currentData$Country2 == 1,]
    } else {
      currentData <- currentData[currentData$Country3 == 1,]
    }
    
    if (input$Gender == "Female") {
      currentData <- currentData[currentData$GenderMale == 0,]
    } else {
      currentData <- currentData[currentData$GenderMale == 1,]
    }
    fitted.AIS <- currentData$fitted[round(currentData$WeightedAIS, 2) == round(input$WeightedAIS, 2)]
    currentVis <- plot_ly(x=~WeightedAIS, y=~fitted, data=currentData, type="scatter", mode="lines", name="Model") %>%
      layout(xaxis = list(title = "Anti Immigration Score (Normalized)"),
             yaxis = list(title = "Predicted probability of wanting to leave the EU"),
             showlegend = FALSE) %>%
      #horizontal line
      add_lines(x=seq(-1, input$WeightedAIS, length.out=100), y=rep(fitted.AIS, 100), line=list(color="red"), text=paste0("Predicted Probability=",round(fitted.AIS, 3)), hoverinfo="text") %>%
      #vertical line
      add_lines(x=rep(input$WeightedAIS, 100), y=seq(0, fitted.AIS, length.out=100), line=list(color="red"), text="Anti Immigration Score", hoverinfo="text")
    
    currentVis
  })
  
}
# Run the app ----
shinyApp(ui = ui, server = server)