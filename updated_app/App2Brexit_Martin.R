library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(scales)
library(plotly)

#setwd("~/Brexit_Analysis/Brexit Shiny")
source('initialize_Brexit_Martin.R')
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
                                     value=as.Date(c("2016-12-01","2020-12-31")),
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
    
    print("Step 2")
    
    #Option:Filter By
    if(input$ScatterFilterby != "all"){
      #filteredGTD <- select(filteredGTD,c("date","NewVote","wave","Count",input$ScatterFilterby))   
    }
    else{
      filteredGTD <- GTDdata %>% 
        group_by(wave,NewVote) %>%
        summarise(TotalLeave=sum(WeightedNewLeaveVote,na.rm=T), 
                  TotalStay=sum(WeightedNewStayVote,na.rm=T),
                  AIS_mean=mean(AIS, na.rm=T),
                  AIS_sd=sd(AIS, na.rm=T)) %>%
        mutate(TotalPar = TotalLeave + TotalStay)%>%
        mutate(Percentage = prop.table(TotalPar/sum(TotalPar)))
      
      filteredGTD <- merge(x = filteredGTD, y = BXTDates, by = "wave", all.x = TRUE)
      
      filteredGTD <- dplyr::arrange(filteredGTD,wave,NewVote)
    }
    
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
    
    
    select(filteredGTD,c("date","NewVote","wave","Count","Proportion"))
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
    
    print("Step 1")
    print(currentData[1,])

    #Preparing titles for axes
    XaxisTitle = "Date"
    YaxisTitle = input$ScatterYaxis
    
    print(input$ScatterYaxis)
    print(input$ScatterXaxis)
    
    if (input$ScatterXaxis != "all") {
      voter_type = case_when(input$ScatterXaxis=="Stay" ~ "Stay",
                             input$ScatterXaxis=="Leave" ~ "Leave")
      
      currentVis <- plot_ly(currentData, x = ~date, y = ~display, text =~wave,  name=voter_type, type = 'scatter',mode = 'lines', line=list(color="black"),
                          hovertemplate = ~paste("Wave",': %{text}<br>',
                                                 XaxisTitle,'<br>= %{x}<br>',
                                                 YaxisTitle,'<br>= %{y}<br>'), marker = list(size = 10, color="black"),
                          showlegend = FALSE)
      currentVis <- currentVis %>% layout(title=voter_type)
    }
    
    else {
      dat = currentData[, c("date", "NewVote", "display", "wave")] %>% pivot_wider(names_from=NewVote, values_from=display)
      
      currentVis <- plot_ly(dat, x=~date, y=~Leave, text=~wave, name="Leave", type = 'scatter', mode = 'lines', 
                            hovertemplate = ~paste("Wave",': %{text}<br>',
                                                   XaxisTitle,'<br>= %{x}<br>',
                                                   YaxisTitle,'<br>= %{y}<br>'), marker = list(size = 10),
                            showlegend = TRUE)
      
      currentVis <- currentVis %>% add_trace(y=~Stay, text=~wave, name="Stay", mode="lines")
    }
    
    #Sets titles for axes
    a <- list(title = XaxisTitle)
    b <- list(title = YaxisTitle)

    currentVis <- currentVis %>% layout(xaxis = a, yaxis = b)  
    
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