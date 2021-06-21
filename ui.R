library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Brexit Referendum Survey"),
  
  fluidRow(
   
    
    column(4,
           wellPanel(
             selectInput("group_choice", "Group by", 
                         c("none", names(ulti_dat)[c(2:12,17:27)]), 
                         selected = "none", multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL),
             selectInput("y", "Immigration Sentiment Measure", 
                         c("all", names(ulti_dat)[c(13:14,28:33)]), 
                         selected = "AIS", multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL)
           )
    )
    ),
   
  plotlyOutput("plot")
  ))
    
