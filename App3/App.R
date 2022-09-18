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
                                         selectInput("Edu", "Education", choices = c("<17", "17-19", ">19")),
                                         selectInput("Gross", "Gross Income", choices = c("0-19k", "20-39k", "40-59k", "60-99k", "100k or above")),
                                         selectInput("Skill", "Skill Level", choices = c("Skill Level 1", "Skill Level 2", "Skill Level 3", "Skill Level 4", "Skill Level 5", "Skill Level 6", "Skill Level 7", "Skill Level 8")),
                                         selectInput("Age", "Age", choices = c("19-35", "36-45", "46-65", "66-85", "Over85")),
                                         selectInput("Marriage", "Marital Status", choices = c("Married", "Single")),
                                         selectInput("Country", "Country", choices = c("England", "Scottland", "Wales")),
                                         selectInput("Gender", "Gender", choices = c("Female", "Male")),
                                       )
                                ),
                                
                                column(8, plotlyOutput("full.graph", height = "500px"))
                       )
                       
                     )
                   ),
                   
                   #Stores information on how many data points are being displayed
                   conditionalPanel(condition="1==0", 
                                    numericInput("ScatterNumPoints", NULL,
                                                 min = 0, max = 10000, value=0)
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
  filterDataReactive <- reactive({
    
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
    colnames(DummyRegressData)[3] <- "Edu1"
    colnames(DummyRegressData)[4] <- "Edu2"
    
    colnames(DummyRegressData)[5] <- "Gross1"
    colnames(DummyRegressData)[6] <- "Gross2"
    colnames(DummyRegressData)[7] <- "Gross3"
    colnames(DummyRegressData)[8] <- "Gross4"
    
    colnames(DummyRegressData)[9] <- "Skill1"
    colnames(DummyRegressData)[10] <- "Skill2"
    colnames(DummyRegressData)[11] <- "Skill3"
    colnames(DummyRegressData)[12] <- "Skill4"
    colnames(DummyRegressData)[13] <- "Skill5"
    colnames(DummyRegressData)[14] <- "Skill6"
    colnames(DummyRegressData)[15] <- "Skill7"
    
    colnames(DummyRegressData)[16] <- "Age1"
    colnames(DummyRegressData)[17] <- "Age2"
    colnames(DummyRegressData)[18] <- "Age3"
    colnames(DummyRegressData)[19] <- "Age4"
    
    colnames(DummyRegressData)[20] <- "MarriageSingle"
    
    colnames(DummyRegressData)[21] <- "Country1"
    colnames(DummyRegressData)[22] <- "Country2"
    
    colnames(DummyRegressData)[23] <- "GenderMale"
    
    
    
    
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
    
    predictData <- do.call("rbind", replicate(100, predictData, simplify = FALSE))
    
    predictData$WeightedAIS <- seq(-1, 1, length.out = 100)
    
    PredictResults <- predict(model0, newdata = predictData, type = "response")
    
    PredictResults <- as.data.frame(PredictResults)
    
    if (input$ScatterXaxis == "None") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS,family=binomial(link='logit'),data=DummyRegressData)
      PredictResults <- predict(model, newdata = predictData, type = "response")
      PredictResults <- as.data.frame(PredictResults)
      
    }
    
    else if(input$ScatterXaxis == "Education") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Edu1 + Edu2,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Edu1"] <- 0
      predictData1[,"Edu2"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Edu1"] <- 1
      predictData2[,"Edu2"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Edu1"] <- 0
      predictData3[,"Edu2"] <- 1
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      PredictResults$Edu1 <- PredictResults11
      PredictResults$Edu2 <- PredictResults12
      PredictResults$Edu3 <- PredictResults13
    }
    
    
    else if(input$ScatterXaxis == "GrossIncome") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Gross1 + Gross2 + Gross3 + Gross4,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Gross1"] <- 0
      predictData1[,"Gross2"] <- 0
      predictData1[,"Gross3"] <- 0
      predictData1[,"Gross4"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Gross1"] <- 1
      predictData2[,"Gross2"] <- 0
      predictData2[,"Gross3"] <- 0
      predictData2[,"Gross4"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Gross1"] <- 0
      predictData3[,"Gross2"] <- 1
      predictData3[,"Gross3"] <- 0
      predictData3[,"Gross4"] <- 0
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      predictData4 <- predictData
      predictData4[,"Gross1"] <- 0
      predictData4[,"Gross2"] <- 0
      predictData4[,"Gross3"] <- 1
      predictData4[,"Gross4"] <- 0
      
      PredictResults14 <- predict(model, newdata = predictData4, type = "response")
      
      predictData5 <- predictData
      predictData5[,"Gross1"] <- 0
      predictData5[,"Gross2"] <- 0
      predictData5[,"Gross3"] <- 0
      predictData5[,"Gross4"] <- 1
      
      PredictResults15 <- predict(model, newdata = predictData5, type = "response")      
      
      PredictResults$Gross1 <- PredictResults11
      PredictResults$Gross2 <- PredictResults12
      PredictResults$Gross3 <- PredictResults13
      PredictResults$Gross4 <- PredictResults14
      PredictResults$Gross5 <- PredictResults15
      
    }
    
    else if(input$ScatterXaxis == "Skill") {
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Skill1 + Skill2 + Skill3 + Skill4 + Skill5 + Skill6 + Skill7,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Skill1"] <- 0
      predictData1[,"Skill2"] <- 0
      predictData1[,"Skill3"] <- 0
      predictData1[,"Skill4"] <- 0
      predictData1[,"Skill5"] <- 0
      predictData1[,"Skill6"] <- 0
      predictData1[,"Skill7"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Skill1"] <- 1
      predictData2[,"Skill2"] <- 0
      predictData2[,"Skill3"] <- 0
      predictData2[,"Skill4"] <- 0
      predictData2[,"Skill5"] <- 0
      predictData2[,"Skill6"] <- 0
      predictData2[,"Skill7"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Skill1"] <- 0
      predictData3[,"Skill2"] <- 1
      predictData3[,"Skill3"] <- 0
      predictData3[,"Skill4"] <- 0
      predictData3[,"Skill5"] <- 0
      predictData3[,"Skill6"] <- 0
      predictData3[,"Skill7"] <- 0
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      predictData4 <- predictData
      predictData4[,"Skill1"] <- 0
      predictData4[,"Skill2"] <- 0
      predictData4[,"Skill3"] <- 1
      predictData4[,"Skill4"] <- 0
      predictData4[,"Skill5"] <- 0
      predictData4[,"Skill6"] <- 0
      predictData4[,"Skill7"] <- 0
      
      PredictResults14 <- predict(model, newdata = predictData4, type = "response")
      
      predictData5 <- predictData
      predictData5[,"Skill1"] <- 0
      predictData5[,"Skill2"] <- 0
      predictData5[,"Skill3"] <- 0
      predictData5[,"Skill4"] <- 1
      predictData5[,"Skill5"] <- 0
      predictData5[,"Skill6"] <- 0
      predictData5[,"Skill7"] <- 0
      
      PredictResults15 <- predict(model, newdata = predictData5, type = "response")
      
      predictData6 <- predictData
      predictData6[,"Skill1"] <- 0
      predictData6[,"Skill2"] <- 0
      predictData6[,"Skill3"] <- 0
      predictData6[,"Skill4"] <- 0
      predictData6[,"Skill5"] <- 1
      predictData6[,"Skill6"] <- 0
      predictData6[,"Skill7"] <- 0
      
      PredictResults16 <- predict(model, newdata = predictData6, type = "response")
      
      predictData7 <- predictData
      predictData7[,"Skill1"] <- 0
      predictData7[,"Skill2"] <- 0
      predictData7[,"Skill3"] <- 0
      predictData7[,"Skill4"] <- 0
      predictData7[,"Skill5"] <- 0
      predictData7[,"Skill6"] <- 1
      predictData7[,"Skill7"] <- 0
      
      PredictResults17 <- predict(model, newdata = predictData7, type = "response")
      
      predictData8 <- predictData
      predictData8[,"Skill1"] <- 0
      predictData8[,"Skill2"] <- 0
      predictData8[,"Skill3"] <- 0
      predictData8[,"Skill4"] <- 0
      predictData8[,"Skill5"] <- 0
      predictData8[,"Skill6"] <- 0
      predictData8[,"Skill7"] <- 1
      
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
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Age1 + Age2 + Age3 + Age4,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Age1"] <- 0
      predictData1[,"Age2"] <- 0
      predictData1[,"Age3"] <- 0
      predictData1[,"Age4"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Age1"] <- 1
      predictData2[,"Age2"] <- 0
      predictData2[,"Age3"] <- 0
      predictData2[,"Age4"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Age1"] <- 0
      predictData3[,"Age2"] <- 1
      predictData3[,"Age3"] <- 0
      predictData3[,"Age4"] <- 0
      
      PredictResults13 <- predict(model, newdata = predictData3, type = "response")
      
      predictData4 <- predictData
      predictData4[,"Age1"] <- 0
      predictData4[,"Age2"] <- 0
      predictData4[,"Age3"] <- 1
      predictData4[,"Age4"] <- 0
      
      PredictResults14 <- predict(model, newdata = predictData4, type = "response")
      
      predictData5 <- predictData
      predictData5[,"Age1"] <- 0
      predictData5[,"Age2"] <- 0
      predictData5[,"Age3"] <- 0
      predictData5[,"Age4"] <- 1
      
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
      
      model <- glm(NewLeaveVote ~ WeightedAIS + Country1 + Country2,family=binomial(link='logit'),data=DummyRegressData)
      
      predictData1 <- predictData
      predictData1[,"Country1"] <- 0
      predictData1[,"Country2"] <- 0
      
      PredictResults11 <- predict(model, newdata = predictData1, type = "response")
      
      predictData2 <- predictData
      predictData2[,"Country1"] <- 1
      predictData2[,"Country2"] <- 0
      
      PredictResults12 <- predict(model, newdata = predictData2, type = "response")
      
      predictData3 <- predictData
      predictData3[,"Country1"] <- 0
      predictData3[,"Country2"] <- 1
      
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
    
    PredictResults$WeightedAIS <- seq(-1, 1, length.out = 100)
    
    PredictResults$Coeff <- sprintf("%.4f",model$coefficients["WeightedAIS"])
    
    PredictResults$Intercept <- sprintf("%.4f",model$coefficients[1])
    
    PredictResults
    
    #    aa <- select(filteredGTD,c("Education","NewVote","Count","Proportion"))
    
    #--------------------------------------End------------------------------------#
    
    
    
  })
  
  prepareCurrentData <- reactive({
    GTDandGM <- filterDataReactive()
    
    #Selects data relevant to user input and assigns uniform names to that
    # data, so the plotly functions can all use the same names
    currentData <- GTDandGM
    
    
    #if (input$ScatterXaxis != "Skill") {
    #  currentData <- currentData[currentData$NewVote==input$ScatterXaxis,]
    #}
    
    #Option: Colors
    #if(input$ScatterColorby != "none"){
    #  currentData$Color <- GTDandGM[[input$ScatterColorby]]
    #}
    #Option: Facets
    #if(input$ScatterFacetby != "none"){
    #  currentData$currentFacet <- GTDandGM[[input$ScatterFacetby]]
    #}
    
    
    #Ensures that there is a Y-variable if there is no data, so that plotly
    # doesn't throw an error
    #if(length(currentData$Wave) == 0) currentData$Yvar <- currentData$Yvar + 0
    
    currentData
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
    currentData <- prepareCurrentData()
    currentData$Coeff[1]
  })
  
  a <- reactive({
    currentData <- prepareCurrentData()
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
    
    currentData <- prepareCurrentData()
    
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
    currentData <- prepareCurrentData()  
    
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
  #     RIVERPLOT CODE
  #############################################################################
  
  output$RiverPlot <- renderPlot({
    
    # Variable to join GTDOverTime and VariablesByYear (3 dataframes with
    # total number of incidents/fatalities/wounded by year and facet, 
    # if applicable)
    if(input$RiverFacets != "none"){
      joinTotalBy <- c("Year", input$RiverFacets)
    } else {
      joinTotalBy <- "Year"   
    }
    
    #Variables is used in ddply to extract absolute number of incidents, etc.
    Variables = c(joinTotalBy, input$RiverColors)
    
    # GTDOverTime is a dataframe with rows as each possible combination of year, 
    # color variable, facet variable (if applicable), and the corresponding 
    # count of the appropriate variable (incidents, fatalities, wounded) 
    # "absolute" references the column which has the absolute counts of 
    # incidents, fatalities, or wounded
    GTDOverTime <- switch(input$RiverYaxis,
                          "Incidents" = ddply(GTDdata, Variables, 
                                              summarise, absolute=length(Success)),
                          "Fatalities" = ddply(GTDdata, Variables, 
                                               summarise, absolute=sum(Fatalities, na.rm = TRUE)),
                          "Wounded" = ddply(GTDdata, Variables, 
                                            summarise, absolute=sum(Wounded, na.rm = TRUE)))
    
    #Adding the total number of incidents by year (and facet, if applicable)
    # and calculating the relative values from the total and absolute
    VariablesByYear <- CalcTotalByFacet(input$RiverFacets)
    GTDOverTime <- left_join(GTDOverTime, VariablesByYear[[input$RiverYaxis]], 
                             by=joinTotalBy) 
    GTDOverTime$relative <- GTDOverTime$absolute / GTDOverTime$total
    
    #Renaming columns 
    names(GTDOverTime)[names(GTDOverTime) == input$RiverColors] <- "Color"  
    
    if(input$RiverFacets != "none" &&
       input$RiverColors != input$RiverFacets){
      names(GTDOverTime)[names(GTDOverTime)==input$RiverFacets]<-"currentFacet"
    }
    
    #Filters by years
    GTDOverTime <- GTDOverTime[GTDOverTime$Year >= input$RiverYears[1] &
                                 GTDOverTime$Year <= input$RiverYears[2],  ]
    
    #Option: Color By
    if(input$RiverCounts == "P"){
      RiverPlot <- ggplot(data=GTDOverTime, aes(x=Year, y=relative, fill=Color)) +
        geom_area(position = "stack") +
        scale_y_continuous(labels=percent)
      
      # fix the huge number of y-axis for log
    } else if (input$RiverCounts == "C"){
      #       if(input$Riverlogy){
      #         GTDOverTime$logabsolute <- log10(GTDOverTime$absolute + 1)
      #         GTDOverTime$logcumabsolute <- cumsum()
      #         RiverPlot <- ggplot(data=GTDOverTime, aes(x=Year, y=absolute, fill=Color)) +
      #           geom_area(position = "dodge") 
      #         
      # #         RiverPlot <- qplot(Year, absolute, data=GTDOverTime, 
      # #                            fill = Color, geom="area")
      #       } else {
      RiverPlot <- ggplot(data=GTDOverTime, aes(x=Year, y=absolute, fill=Color)) +
        geom_area(position = "stack") 
      #       }
    }
    
    
    #Option: Facet By
    if(input$RiverFacets != "none"){
      if(input$RiverFacets != input$RiverColors){
        RiverPlot <- RiverPlot + facet_wrap(~currentFacet, ncol=4) 
      } else {
        RiverPlot <- RiverPlot + facet_wrap(~Color, ncol=4)    
      }
    }
    
    RiverPlot <- RiverPlot + 
      theme(legend.position="right", strip.text=element_text(size=18),
            axis.title=element_text(size=18)) + 
      ylab(input$RiverYaxis)
    
    #   print(sum(is.na(GTDOverTime)))
    
    #    A problem: why there is some grey area? Color by Target Type, Facet by
    #         Weapon Type
    #    GTDOverTime <- arrange(GTDOverTime, currentFacet)
    #    GTDOverTime <- arrange(GTDOverTime, Year)
    #    
    #    View(GTDOverTime)
    
    RiverPlot
    
  })
  
  #############################################################################  
  #     BARPLOT CODE
  #############################################################################
  
  output$BarPlot <- renderPlot({ 
    
    #Removing year 1993 as an option
    validate(
      if(input$BarYears == 1993){
        "Year 1993 doesn't have data. Please choose another year."
      }
    )
    
    #Making title for X-axis based on user input
    if(input$BarRangeType == "Below/Above"){
      XaxisTitle <- "Below/Above n"
    } else {
      XaxisTitle <- "Number of"
    }
    XaxisTitle <- paste(XaxisTitle, input$BarYaxis)
    
    #Option: Color by
    if(input$BarColor != "none"){
      GTDbyCountryYear$Color <- GTDbyCountryYear[[input$BarColor]]
    }
    
    #Setting X variable
    BarYvar <- paste("Num", input$BarYaxis, sep="")
    
    if(input$BarRangeType == "Below/Above"){      
      GTDbyCountryYear$Xvar <- paste(input$BarNValue, "or Fewer", input$BarYaxis)
      AboveX <- GTDbyCountryYear[[BarYvar]] > input$BarNValue
      GTDbyCountryYear$Xvar[AboveX] <- paste(input$BarNValue + 1, "or More", input$BarYaxis)
      
      currentBarPlot <- ggplot(GTDbyCountryYear[GTDbyCountryYear$Year == input$BarYears, ], 
                               aes(as.numeric(factor(Xvar)), y = ..count.., fill="#a6cee3")) + 
        geom_bar()
      
    } else {
      GTDbyCountryYear$Xvar <- GTDbyCountryYear[[BarYvar]]
      
      #Option: Not Change Y-axis to percentage
      if(!input$BarCounts){
        currentBarPlot <- ggplot(GTDbyCountryYear[GTDbyCountryYear$Year == input$BarYears, ], aes(x=Xvar, fill="#a6cee3")) + 
          geom_histogram() + geom_rug() +
          coord_cartesian(ylim=c(0, input$BarYlim))
      }
      #Option: Change Y-axis to percentage
      else{
        currentBarPlot <- ggplot(GTDbyCountryYear[GTDbyCountryYear$Year == input$BarYears, ], aes(x=Xvar, fill="#a6cee3")) + 
          geom_histogram(aes(y = (..count..)/sum(..count..))) + geom_rug() +
          scale_y_continuous(labels=scales::percent)
      }
    }
    
    currentBarPlot <- currentBarPlot + xlab(input$BarRangeType) + ylab("Number of Countries") +
      scale_fill_manual(values=customColors) +
      theme(legend.position="none", axis.title=element_text(size=18)) + 
      xlab(XaxisTitle)
    
    #Option: Color by
    if(input$BarColor != "none"){
      currentBarPlot <- currentBarPlot + aes(fill=Color) +
        theme(legend.position="right")
    }
    
    #Option: Puts count and percentage at the top of bars for Below/Above
    # n incidents without color
    if(input$BarColor == "none" & input$BarRangeType == "Below/Above"){
      currentBarPlot <- currentBarPlot + 
        geom_text(aes(y=(..count..), 
                      label = ifelse((..count..)==0,
                                     "", 
                                     paste(..count.., " (",
                                           scales::percent((..count..)/sum(..count..)),
                                           ")", sep = ""))),
                  stat="bin",colour="#1f78b4")
    }
    
    currentBarPlot 
  })
  
  #Self-updating labels
  observe({
    BarYaxisVar <- input$BarYaxis
    
    # Radio group ==============================================
    BarRangeType_options <- list()
    BarRangeType_options[[paste("Below/Above n", BarYaxisVar)]] <- "Below/Above"
    BarRangeType_options[["Histogram"]] <- "Histogram"
    
    # Update text for range options to accomodate for Y-axis variable
    # ex. Change "Incidents" to "Fatalities" in range options label when
    #   Y-axis variable is changed from "Incidents" to "Fatalities"
    updateRadioButtons(session, "BarRangeType", choices = BarRangeType_options,
                       selected=input$BarRangeType)
  })
  
  
  #Reactivate Dataset
  downloadFile <- reactive({
    currentData <- filterDataReactive()
  })
  
  # Downloadable csv of selected dataset ----
  output$DownloadingData <- downloadHandler(
    
    filename = function() {
      paste("currentData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(downloadFile(), file, row.names = FALSE)
    })
  
  
}
# Run the app ----
shinyApp(ui = ui, server = server)