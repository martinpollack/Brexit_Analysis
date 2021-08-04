library(shiny)
library(ggplot2)

data <- read.csv("panel_data.csv")

ui <- fluidPage(
  
  titlePanel(title = "Brexit Referendum Panel Survey"),
  
  mainPanel(
    plotlyOutput(outputId = "raw_vote"), 
    plotlyOutput(outputId = "bi_voter_type")
  )
)
  
server <- function(input, output) {
  output$raw_vote <- renderPlotly(
    plot <- ggplot(data, aes(x = wave, fill = NewVote)) + 
      geom_bar(position = "fill") + 
      labs(y = "Proporation of participants")
  )
  output$bi_voter_type <- renderPlotly(
    plot <- ggplot(data, aes(x = wave, fill = NewStatus)) + 
      geom_bar(position = "fill") + 
      labs(y = "Proporation of participants")
  )
}
  
shinyApp(ui = ui, server = server)