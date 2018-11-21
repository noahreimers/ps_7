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
      plotOutput("DemAdvantagePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$DemAdvantagePlot <- renderPlot({
    
    DemAdvantage <- ps7 %>% 
      filter(win_party == input$win_party)
    
    ggplot(data = DemAdvantage, aes(x = dem_advantage, y = dem_advantage_results)) + geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)