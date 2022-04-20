library(shiny)
library(shinythemes)
source("init.R")

ui <- fluidPage(
  theme = shinytheme("darkly"),
  padding = c("1em"),
  tabsetPanel(
    selected = "Plots by country",
    tabPanel("Raw data", raw_data_ui),
    tabPanel("Plots by country", src_by_country_ui)
  )
)
 
server <- function(input, output, session) {
  output$src_by_country <-
    renderPlot({
      src_by_country(input$country, input$year)
    }) %>%
    bindCache(input$country, input$year)
  
  output$src_lines_by_country <-
    renderPlot({
      src_lines_by_country(input$country)
    })
  
  output$src_by_country_by_year <-
    renderPlot({
      src_by_country_by_year(input$country, input$year)
    }) %>%
    bindCache(input$country, input$year)
  
  output$raw_data <-
    renderDataTable(
      filter_data(
        input$filter_country,
        input$year_interval,
        input$source,
        input$production_interval
      ),
      options = list(pageLength = 15)
    )
  
  observe({
    val <- input$click_year$x
    updateSliderInput(session, "year", value = val)
  })
}

shinyApp(ui = ui, server = server)
