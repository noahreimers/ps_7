#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggrepel)

ps7 <- readRDS("ps7.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Democratic Advantage"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "win_party",
                  label = "Victorious Party",
                  choices = c("R",
                              "D",
                              "UNDECIDED"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("DemAdvantagePlot"),
      HTML(
        paste(
          h3("Description"),
          p("This graph is showing the predicted Democratic advantage in each election versus the actual results of
            that election. The dropdown at the left allows you to filter by which party ended up winning the election.
            There are a couple races that stick out in the spread between these two: tx-23, mn-08, wv-03, ny-11, ca-25.")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$DemAdvantagePlot <- renderPlot({
    
    DemAdvantage <- ps7 %>% 
      filter(win_party == input$win_party)
    
    ggplot(data = DemAdvantage, aes(x = dem_advantage, y = dem_advantage_results, label = state_district)) + 
      geom_point(color = "blue", size = 3) + 
      geom_label_repel(aes(label = state_district),
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50') +
      theme_classic() + 
      labs(x = "Democratic Advantage", y = "Democratic Advantage Results")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
