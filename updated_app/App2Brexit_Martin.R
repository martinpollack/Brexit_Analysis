library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(scales)
library(plotly)
library(DataCombine)

setwd("~/Documents/Brexit_Analysis/")
source('updated_app/initialize_Brexit_Martin.R')
print("Finished initializing")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#
ui <- navbarPage("Brexit Data Visualization",
                 
                 #==================
                 #Cross-Sectional Visualization
                 #==================
                 tabPanel(span("Cross Sectional Analysis" , style = "color:#1369bf"), fluidPage(
                   titlePanel(htmlOutput("ScatterTitle")),
                   #div("Brexit Data Time-series - ", style = "color:#1369bf", align = "center")),    
                   fluidRow(
                     
                     # Sidebar tabs
                     column(4, tabsetPanel(
                       tabPanel("Axes", wellPanel( #RENAME PANEL TAB
                         
                         #X-axis (Display variables, warnings, log option)
                         fluidRow(column(9, selectInput("ScatterXaxis", "Participants Group", choices = XAxisOptions))),
                         p(span(strong("note:"), style = "color:#1369bf"),
                           "Select the variable to display"),
                         
                         #Y-axis (GTD variables, warnings, log option)
                         fluidRow(column(9, selectInput("ScatterYaxis", "Display Option", choices = TemporalOptions))),
                         p(span(strong("note:"), style = "color:#1369bf"),
                           "Select the display by percentage or counts"),
                         
                         #Type of scatterplot and conditional faceting
                         radioButtons("ScatterPlotType", label="Type of Plot", inline=TRUE,
                                      choices=c("plotly" = "plotly", "ggplot" = "ggplot"),
                                      selected="plotly"),
                         
                         #Filter by variables
                         fluidRow(column(6, selectInput("ScatterFilterby", "Filter By", choices = FilterOptions))),
                         
                         
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
                         
                         HTML("Variable Descriptions for the app: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/Handouts/GlobalTerrorism/GTDVariableDescription.pdf'>Variable Descriptions</a>")
                         
                         # HTML("Additional resources for instructors: <a href='http://web.grinnell.edu/individuals/kuipers/stat2labs/'>Stats2Labs</a>")
                       )),
                       
                       # Four Filters
                       tabPanel("Filters", wellPanel(
                         selectInput("ScatterRegion", "Filter by Region", choices = XAxisOptions),
                         selectInput("ScatterAttack", "Filter by Attack Type", choices = XAxisOptions),
                         selectInput("ScatterTarget", "Filter by Target Type", choices = XAxisOptions),
                         selectInput("ScatterWeapon", "Filter by Weapon Type", choices = XAxisOptions),
                         sliderInput("ScatterNincidents", "Restrict data to Country-Years that have at
                                            least n incidents",
                                     0, 200, 0, step = 5)
                       ))
                     )),
                     
                     #Output
                     column(8, conditionalPanel(condition="input.ScatterPlotType == 'plotly'",
                                                plotlyOutput("graph1", height = "500px")),
                            conditionalPanel(condition="input.ScatterPlotType == 'ggplot'",
                                             plotOutput("ggplot1", height = "500px"))
                            
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
    
    
    #Data to select from GTD
    fixedVariables <- c("Year", "NumCode", "Region", "Religion")
    
    #Filtering the GTDdata
    BXTDates <- BXTDates
    
    #Change the colnames in GTDdata
    colnames(GTDdata)[which(names(GTDdata) == "gender")] <- "Gender"
    colnames(GTDdata)[which(names(GTDdata) == "country")] <- "Country"
    
    #Construct a function to compute changes
    compute_changes <- function(input) {
      
      Data <- data.frame(select(input, wave, TotalPar))
      
      Out <- PercChange(Data, Var = 'TotalPar',
                        type = 'proportion',
                        NewVar = 'PercentChange',
                        slideBy = -1)
      
      output <- merge(x = input, y = select(Out,wave,PercentChange), by = "wave", all.x = TRUE)
      
      output <- output %>% mutate(ChangeInPercent = Percentage - lag(Percentage,1))
      output <- output %>% mutate(TotalChange = TotalPar - lag(TotalPar,1))
      
      return(output)
    }
    
    #Contruct a function to compute percentages based on votes
    compute_percentage <- function(input) {
      
      filteredGTD <- input %>% 
        group_by(wave,NewVote) %>%
        summarise(TotalLeave=sum(WeightedNewLeaveVote,na.rm=T), 
                  TotalStay=sum(WeightedNewStayVote,na.rm=T),
                  AIS_mean=mean(AIS, na.rm=T),
                  AIS_sd=sd(AIS, na.rm=T)) %>%
        mutate(TotalPar = TotalLeave + TotalStay)%>%
        mutate(Percentage = prop.table(TotalPar/sum(TotalPar)))
      
      filteredGTD <- merge(x = filteredGTD, y = BXTDates, by = "wave", all.x = TRUE)
      
      #Compute change in percent/Percent Change/Total Change
      agg11 <- filteredGTD[filteredGTD$NewVote=="Leave",]
      agg11 <- dplyr::arrange(agg11,wave,NewVote)
      agg12 <- filteredGTD[filteredGTD$NewVote=="Stay",]
      agg12 <- dplyr::arrange(agg12,wave,NewVote)
      
      
      agg11 <- compute_changes(agg11)
      agg12 <- compute_changes(agg12)
      
      
      output <- rbind(agg11, agg12) 
      
      return(output)
    }
    
    
    #Option:Filter By
    if(input$ScatterFilterby != "all"){
      
      #If Filter by gender
      if(input$ScatterFilterby=="Gender"){
        
        filteredGTDMale <-compute_percentage(GTDdata[GTDdata$Gender=="Male",])
        filteredGTDFemale <-compute_percentage(GTDdata[GTDdata$Gender=="Female",])
        filteredGTDMale$Gender = "Male"
        filteredGTDFemale$Gender = "Female"
        filteredGTD<- rbind(filteredGTDMale, filteredGTDFemale)
        
      }
      
      
      #If Filter by marriage
      else if(input$ScatterFilterby=="Marriage"){
        
        filteredGTDMarried <-compute_percentage(GTDdata[GTDdata$Marriage=="Married",])
        filteredGTDSingle <-compute_percentage(GTDdata[GTDdata$Marriage=="Single",])
        filteredGTDMarried$Marriage = "Married"
        filteredGTDSingle$Marriage = "Single"
        filteredGTD<- rbind(filteredGTDMarried, filteredGTDSingle)
      }
      
      #If Filter by country
      else if(input$ScatterFilterby=="Country"){
        
        filteredGTDEngland <-compute_percentage(GTDdata[GTDdata$Country=="England",])
        filteredGTDScotland <-compute_percentage(GTDdata[GTDdata$Country=="Scotland",])
        filteredGTDWales <-compute_percentage(GTDdata[GTDdata$Country=="Wales",])
        
        filteredGTDEngland$Country = "England"
        filteredGTDScotland$Country = "Scotland"
        filteredGTDWales$Country = "Wales"
        
        filteredGTD<- rbind(filteredGTDEngland, filteredGTDScotland,filteredGTDWales)
      }      
      
      #If Filter by Age
      else if(input$ScatterFilterby=="AgeGroup"){
        
        filteredGTDAge1 <-compute_percentage(GTDdata[GTDdata$AgeGroup=="Age19-35",])
        filteredGTDAge2 <-compute_percentage(GTDdata[GTDdata$AgeGroup=="Age36-45",])
        filteredGTDAge3 <-compute_percentage(GTDdata[GTDdata$AgeGroup=="Age46-65",])
        filteredGTDAge4 <-compute_percentage(GTDdata[GTDdata$AgeGroup=="Age66-85",])
        filteredGTDAge5 <-compute_percentage(GTDdata[GTDdata$AgeGroup=="AgeOver85",])
        
        filteredGTDAge1$AgeGroup = "Age19-35"
        filteredGTDAge2$AgeGroup = "Age36-45"
        filteredGTDAge3$AgeGroup = "Age46-65"
        filteredGTDAge4$AgeGroup = "Age66-85"
        filteredGTDAge5$AgeGroup = "AgeOver85"
        
        filteredGTD<- rbind(filteredGTDAge1, filteredGTDAge2,filteredGTDAge3, filteredGTDAge4, filteredGTDAge5)
      } 
      
      
    }
    else{
      
      filteredGTD <-compute_percentage(GTDdata)
      
    }
    
    filteredGTD <- dplyr::arrange(filteredGTD,wave,NewVote)
    
    
    ##Rename TotalPar as Count
    colnames(filteredGTD)[which(names(filteredGTD) == "TotalPar")] <- "Count"
    
    ##Rename percentage as proportion
    colnames(filteredGTD)[which(names(filteredGTD) == "Percentage")] <- "Proportion"
    
    #Option: Colors
    #if(input$ScatterColorby != "none"){
    #  fixedVariables <- c(fixedVariables, input$ScatterColorby)    
    #}
    
    #Option: Facets
    #if(input$ScatterFacetby != "none"){
    #  fixedVariables <- c(fixedVariables, input$ScatterFacetby)
    #}
    
    #By years
    filteredGTD <- filteredGTD[filteredGTD$date >= input$ScatterYear[1] & 
                                 filteredGTD$date <= input$ScatterYear[2], ]
    
    #If there's a filter variable
    if(input$ScatterFilterby != "all"){
      select(filteredGTD,c("date","NewVote","wave","Count","Proportion", "PercentChange", "ChangeInPercent", "TotalChange",input$ScatterFilterby))
    }
    #If there's no filter variable selected
    else{
      select(filteredGTD,c("date","NewVote","wave","Count","Proportion", "PercentChange", "ChangeInPercent", "TotalChange"))
    }
    
  })
  
  prepareCurrentData <- reactive({
    GTDandGM <- filterDataReactive()
    
    #Selects data relevant to user input and assigns uniform names to that
    # data, so the plotly functions can all use the same names
    currentData <- GTDandGM
    
    
    
    if (input$ScatterXaxis != "all") {
      currentData <- currentData[currentData$NewVote==input$ScatterXaxis,]
    }
    
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
    
    currentData$display <- currentData[,input$ScatterYaxis]
    
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
  #     PLOTLY CODE
  #############################################################################
  #Function for showing country-year, x, and y information when hovering over 
  #data points
  
  
  
  #A reactive expression with the plotly plot
  output$graph1 <- renderPlotly({
    
    currentData <- prepareCurrentData()
    
    Visualization <- function(inputData, legend) {
      
      #Preparing titles for axes
      XaxisTitle = "Date"
      YaxisTitle = input$ScatterYaxis
      
      
      dat = inputData[, c("date", "NewVote", "display", "wave")] %>% pivot_wider(names_from=NewVote, values_from=display)
      
      
      if (input$ScatterXaxis != "all") {
        voter_type = case_when(input$ScatterXaxis=="Stay" ~ "Stay",
                               input$ScatterXaxis=="Leave" ~ "Leave")
        
        if (voter_type == "Stay"){
          Add_color = "Orange"
        }
        
        else{
          Add_color = "Blue"
        }
        
        
        currentVis <- plot_ly() %>% add_trace(data=dat, x=~date, y=~get(voter_type), text=~wave, name=voter_type,line=list(color=Add_color),
                                              type = 'scatter', mode = 'lines+markers', marker=list(color=Add_color,size=10),
                                              hovertemplate = ~paste("Wave",': %{text}<br>',
                                                                     XaxisTitle,'<br>= %{x}<br>',
                                                                     YaxisTitle,'<br>= %{y}<br>'),
                                              showlegend = legend)
        
        
        currentVis <- currentVis %>% layout(title=voter_type)
      }
      
      else {
        
        currentVis <- plot_ly() %>% add_trace(data=inputData, x=~date, y=~display, color=~NewVote, colors=c("Blue", "Orange"), text=~wave, 
                                              type = 'scatter', mode = 'lines+markers', marker=list(size=10),
                                              hovertemplate = ~paste("Wave",': %{text}<br>',
                                                                     XaxisTitle,'<br>= %{x}<br>',
                                                                     YaxisTitle,'<br>= %{y}<br>'),
                                              showlegend = legend)
        
        # currentVis <- currentVis %>% add_trace(data=dat, x=~date, y=~Stay, text=~wave, name="Stay",
        #                                        type="scatter", mode="lines+markers", marker=list(size=10),
        #                                        hovertemplate = ~paste("Wave",': %{text}<br>',
        #                                                               XaxisTitle,'<br>= %{x}<br>',
        #                                                               YaxisTitle,'<br>= %{y}<br>'),
        #                                        showlegend = TRUE)
      }
      
      
      #add horizontal line
      event1 <- "2016-06-23"
      event2 <- "2017-03-29"
      event3 <- "2020-02-01"
      
      #Define the Y-axis range of event according to user selection of Count/Proportion
      
      if(input$ScatterYaxis=="Proportion")
      {
        y_min = 0.2
        y_max = 0.8
      }
      else if(input$ScatterYaxis=="Count")
      {
        y_min = 0
        y_max = 12000
      }
      else if(input$ScatterYaxis=="PercentChange")
      {
        y_min = -0.6
        y_max = 0.6
      }
      else if(input$ScatterYaxis=="ChangeInPercent")
      {
        y_min = -0.1
        y_max = 0.1
      }
      else if(input$ScatterYaxis=="TotalChange")
      {
        y_min = -6000
        y_max = 6000
      }
      
      
      #If event1 is included in the user selection of dates
      if(event1 >= input$ScatterYear[1] & event1 <= input$ScatterYear[2])
      {    
        
        lines <- data.frame(date=rep(event1, 100), y=seq(y_min,y_max,length.out=100))
        currentVis <- currentVis %>% 
          add_trace(data=lines, x=~date, y=~y, 
                    hovertemplate= ~paste("Event1",': %{x}','<br>UK held a referendum<br>'),
                    type="scatter", mode="lines", name="Event1", showlegend=FALSE)
      }
      
      #If event2 is included in the user selection of dates
      if(event2 >= input$ScatterYear[1] & event2 <= input$ScatterYear[2])
      {    
        
        lines <- data.frame(date=rep(event2, 100), y=seq(y_min,y_max,length.out=100))
        currentVis <- currentVis %>% 
          add_trace(data=lines, x=~date, y=~y, 
                    hovertemplate= ~paste("Event2",': %{x}','<br>UK notified the EU of its decision to leave<br>'),
                    type="scatter", mode="lines", name="Event2", showlegend=FALSE)
      }
      
      #If event3 is included in the user selection of dates
      if(event3 >= input$ScatterYear[1] & event3 <= input$ScatterYear[2])
      {    
        
        lines <- data.frame(date=rep(event3, 100), y=seq(y_min,y_max,length.out=100))
        currentVis <- currentVis %>% 
          add_trace(data=lines, x=~date, y=~y, 
                    hovertemplate= ~paste("Event3",': %{x}','<br>UK withdrawals from the European Union<br>'),
                    type="scatter", mode="lines", name="Event3", showlegend=FALSE)
      }    
      
      
      #Sets titles for axes
      a <- list(title = XaxisTitle)
      b <- list(title = YaxisTitle)
      
      currentVis <- currentVis %>% layout(xaxis = a, yaxis = b)
      currentVis <- currentVis %>% layout(showlegend=TRUE)
      
      return(currentVis)
      
    }
    
    #Configure the subgraphs based on 
    
    if(input$ScatterFilterby == "Gender"){
      currentDataMale <- currentData[currentData$Gender=="Male",]
      currentDataFemale <- currentData[currentData$Gender=="Female",]
      currentVisMale <-Visualization(currentDataMale, TRUE)
      currentVisFemale <-Visualization(currentDataFemale, FALSE)
      
      #Configure the layout of subplots
      currentVis<-subplot(currentVisMale,currentVisFemale, nrows =2)
      currentVis <- currentVis %>% layout(annotations=list(list(text="Male", showarrow=F, x=-0.05, y=1, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)), 
                                                           list(text="Female", showarrow=F, x=-0.07, y=0.47, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18))))
      
    }
    
    else if(input$ScatterFilterby == "Marriage"){
      currentDataMarried <- currentData[currentData$Marriage=="Married",]
      currentDataSingle <- currentData[currentData$Marriage=="Single",]
      currentVisMarried <-Visualization(currentDataMarried, TRUE)
      currentVisSingle <-Visualization(currentDataSingle, FALSE)
      
      #Configure the layout of subplots
      currentVis<-subplot(currentVisMarried,currentVisSingle, nrows =2)
      currentVis <- currentVis %>% layout(annotations=list(list(text="Married", showarrow=F, x=-0.05, y=1, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)), 
                                                           list(text="Single", showarrow=F, x=-0.05, y=0.5, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18))))
      
    }
    
    else if(input$ScatterFilterby == "Country"){
      currentDataEngland <- currentData[currentData$Country=="England",]
      currentDataScotland <- currentData[currentData$Country=="Scotland",]
      currentDataWales <- currentData[currentData$Country=="Wales",]
      
      currentVisEngland <-Visualization(currentDataEngland, TRUE)
      currentVisScotland <-Visualization(currentDataScotland, FALSE)
      currentVisWales <-Visualization(currentDataWales, FALSE)
      
      #Configure the layout of subplots
      currentVis<-subplot(currentVisEngland,currentVisScotland, currentVisWales, nrows =3)
      currentVis <- currentVis %>% layout(annotations=list(list(text="England", showarrow=F, x=-0.05, y=1, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)), 
                                                           list(text="Scotland", showarrow=F, x=-0.05, y=0.6, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)), 
                                                           list(text="Wales", showarrow=F, x=-0.08, y=0.3, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18))))
      
    }
    
    else if(input$ScatterFilterby == "AgeGroup"){
      currentDataAge1 <- currentData[currentData$AgeGroup=="Age19-35",]
      currentDataAge2 <- currentData[currentData$AgeGroup=="Age36-45",]
      currentDataAge3 <- currentData[currentData$AgeGroup=="Age46-65",]
      currentDataAge4 <- currentData[currentData$AgeGroup=="Age66-85",]
      currentDataAge5 <- currentData[currentData$AgeGroup=="AgeOver85",]
      
      currentVisAge1 <-Visualization(currentDataAge1, TRUE)
      currentVisAge2 <-Visualization(currentDataAge2, FALSE)
      currentVisAge3 <-Visualization(currentDataAge3, FALSE)
      currentVisAge4 <-Visualization(currentDataAge4, FALSE)
      currentVisAge5 <-Visualization(currentDataAge5, FALSE)
      
      #Configure the layout of subplots
      currentVis<-subplot(currentVisAge1,currentVisAge2, currentVisAge3,
                          currentVisAge4,currentVisAge5, nrows =5)
      currentVis <- currentVis %>% layout(annotations=list(list(text="19-35", showarrow=F, x=-0.05, y=1, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)), 
                                                           list(text="36-45", showarrow=F, x=-0.05, y=0.75, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)),
                                                           list(text="46-65", showarrow=F, x=-0.05, y=0.59, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)),
                                                           list(text="66-85", showarrow=F, x=-0.06, y=0.37, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18)),
                                                           list(text="86+", showarrow=F, x=-0.05, y=0.17, xanchor="center", yanchor="bottom", xref='paper', yref='paper', font=list(size=18))))
      
    }
    
    else{
      
      currentVis <-Visualization(currentData, TRUE)
      
    }
    #Option: color by
    #if(input$ScatterColorby != "none"){
    #  currentVis <- currentVis %>% add_trace(color = ~Color, showlegend = TRUE)
    #}
    #else {currentVis <- currentVis %>% add_trace(marker = list(color = "black"), showlegend = FALSE)}
    
    currentVis
    
  })
  
  
  #############################################################################  
  #     GGPLOT CODE
  #############################################################################
  
  #ggplot scatterplot
  output$ggplot1 <- renderPlot({   
    currentData <- prepareCurrentData()  
    
    #Preparing titles for axes
    XaxisTitle = "Date"
    YaxisTitle = input$ScatterYaxis
    
    voter_type = case_when(input$ScatterXaxis=="Stay" ~ "Stay",
                           input$ScatterXaxis=="Leave" ~ "Leave")
    
    if (input$ScatterXaxis != "all") {
      currentVis <- ggplot(currentData, aes(x=date, y=display)) +
        geom_line(size=1) + geom_point(size=4)
      theme_bw() + 
        xlab(XaxisTitle) + ylab(YaxisTitle) + 
        theme(axis.title=element_text(size=18)) + 
        labs(title=voter_type)
    }
    else {
      dat = currentData
      levels(dat$NewVote) = c("Leave", "Stay")
      
      currentVis <- ggplot(dat, aes(x=date, y=display, color=NewVote)) +
        geom_line(size=1) + geom_point(size=4) + 
        theme_bw() + 
        xlab(XaxisTitle) + ylab(YaxisTitle) + 
        theme(axis.title=element_text(size=18)) + 
        scale_colour_manual(values=c("blue", "orange", "green", "red"))
    }
    
    #Option: Facets
    # if(input$ScatterFacetby != "none" && length(currentData$Year) != 0){
    #   currentPlot <- currentPlot + facet_wrap(~currentFacet, ncol=4) +
    #     theme(legend.position="right", strip.text=element_text(size=18))
    # }
    
    #Option: Add color by region
    # if(input$ScatterColorby != "none"){
    #   currentPlot <- currentPlot + aes(colour=Color) +
    #     scale_fill_manual(values=customColors)
    # }
    currentVis
  })
  
  output$ScatterNotes <- renderText({
    
    if(input$ScatterNumPoints == 0){
      
    }
    
    Notes
  })
  
  output$ScatterNotes_left <- renderText({
    
    
  })
  
  output$ScatterNotes_right <- renderText({
    
  })
  
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
  
}
# Run the app ----
shinyApp(ui = ui, server = server)