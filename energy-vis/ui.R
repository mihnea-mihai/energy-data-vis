#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Energy sources per Country per Year"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Year",
                  min = 2000,
                  max = 2020,
                  value = 2010,
                  sep = ""),
      selectInput("country",
                  "Country",
                  choices = unique(data$Country),
                  selected = "Romania")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("energPlot")
    )
  )
)
