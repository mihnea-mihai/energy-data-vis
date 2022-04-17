library(shiny)
library(shinythemes)
source("init.R")

src_by_country_ui <-
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country",
                             choices = unique(data$Country),
                             selected = "EU27+1")
      ),
    mainPanel(
      plotOutput("src_by_country"),
      plotOutput("src_lines_by_country")
      )
  )

ui <- fixedPage(theme = shinytheme("darkly"), padding = c("1em"),
    tabsetPanel(
      tabPanel("Plots by country",
               src_by_country_ui)
    )
)

server <- function(input, output) {
    output$src_by_country <- renderPlot({
        src_by_country(input$country)
    })
    output$src_lines_by_country <- renderPlot({
      src_lines_by_country(input$country)
    })
}

shinyApp(ui = ui, server = server)
