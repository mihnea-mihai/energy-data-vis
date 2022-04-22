library(shiny)
library(shinythemes)
source("init.R")

ui <- fluidPage(theme = shinytheme("darkly"),
                sidebarLayout(
                  sidebarPanel(input_ui, width = 2),
                  mainPanel(
                    width = 10,
                    tabsetPanel(
                      selected = "Plots by country",
                      tabPanel("Raw data", raw_data_ui),
                      tabPanel("Plots by country", src_by_country_ui)
                    )
                  )
                ))

server <- function(input, output, session) {
  output$src_by_country <-
    renderPlot({
      src_by_country(input$country, input$year)
    }) %>%
    bindCache(input$country, input$year)
  
  output$src_lines_by_country <-
    renderPlot({
      src_lines_by_country(input$country, input$year)
    }) %>%
    bindCache(input$country, input$year)
  
  output$src_by_country_by_year <-
    renderPlot({
      src_by_country_by_year(input$country, input$year)
    }) %>%
    bindCache(input$country, input$year)
  
  output$src_by_country_by_year_lollipop <-
    renderPlot({
      src_by_country_by_year_lollipop(input$country, input$year)
    }) %>%
    bindCache(input$country, input$year)
  
  output$raw_data <-
    renderDataTable(filter_data(input$country, input$year, input$source),
                    options = list(pageLength = 15))
  
  session$allowReconnect("force")
  
  observe({
    val <- input$click_year$x
    updateSliderInput(session, "year", value = val)
  })
}

shinyApp(ui = ui, server = server)
