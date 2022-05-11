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
                      tabPanel("Plots by country", src_by_country_ui),
                      tabPanel("Plots by year", by_year_ui)
                    )
                  )
                ))

server <- function(input, output, session) {
  output$src_by_country <-
    renderPlot({
      src_by_country(input$country, input$year, input$absolute)
    }) %>%
    bindCache(input$country, input$year, input$absolute)
  
  output$src_lines_by_country <-
    renderPlot({
      src_lines_by_country(input$country, input$year, input$absolute)
    }) %>%
    bindCache(input$country, input$year, input$absolute)
  
  output$src_by_country_by_year <-
    renderPlot({
      src_by_country_by_year(input$country, input$year, input$absolute)
    }) %>%
    bindCache(input$country, input$year, input$absolute)
  
  output$src_by_country_by_year_lollipop <-
    renderPlot({
      src_by_country_by_year_lollipop(input$country, input$year, input$absolute)
    }) %>%
    bindCache(input$country, input$year, input$absolute)
  
  output$country_by_year_by_source_lollipop <- 
    renderPlot({
      country_by_year_by_source_lollipop(input$year, input$source, input$absolute)
    }) %>%
    bindCache(input$year, input$source, input$absolute)
  
  output$country_by_year_stacked  <- 
    renderPlot({
      country_by_year_stacked (input$year, input$absolute)
    }) %>%
    bindCache(input$year, input$absolute)
  
  output$raw_data <-
    renderDT(mutate(data, across(c(
      Production, Percent
    ), round, digits = 2)),
    options = list(pageLength = 15),
    style = "bootstrap") # to apply shiny theme
  
  output$map_by_year_by_source <- 
    renderPlot({
        map_by_year_by_source(input$year, input$source, input$absolute)
    }) %>%
    bindCache(input$year, input$source, input$absolute)
  
  session$allowReconnect("force")
  
  observe({
    val <- input$click_year$x
    updateSliderInput(session, "year", value = val)
  })
}

shinyApp(ui = ui, server = server)
