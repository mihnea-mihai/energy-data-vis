library(shiny)
library(shinythemes)
library(DT)
source("init.R")

ui <- fluidPage(title = "EU energy visualisation",
  theme = shinytheme("darkly"),
  sidebarLayout(
    sidebarPanel(width = 2,
      column(width = 12, textOutput("drill_down"), uiOutput("input_ui"))
      ),
    mainPanel(width = 10,
      tabsetPanel(
        tabPanel("Raw data", sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("countries", "Country",
                               choices = all_countries,
                               selected = all_countries, inline = TRUE),
            checkboxGroupInput("sources", "Source",
                               choices = all_sources, 
                               selected = all_sources, inline = TRUE),
            sliderInput("years", "Year",
                        min = 2000, max = 2020,
                        sep = "", value = c(2000, 2020))
          ),
          mainPanel(DTOutput("raw_data"))
        )
      ),
        tabPanel("Country overview", src_by_country_ui),
        tabPanel("Yearly energy mix",
                 plotOutput("country_by_year_stacked", height = "85rem")),
        tabPanel("Yearly source overview",
          fluidRow(
            column(width = 6,
                   plotOutput("map_by_year_by_source", height = "85rem")),
            column(width = 6,
                   plotOutput("country_by_year_by_source_lollipop",
                              height = "85rem"))
            )
          ),
        tabPanel("Yearly majority source",
          fluidRow(
            column(6, plotOutput("map_maj_source_by_year", height = "85rem")),
            column(6)
          )
        )
      )
      )
    )
  )

server <- function(input, output, session) {
  
  fdata <- reactive({
    filter_data(input$countries, input$years, input$sources)
  })
  
  output$raw_data <-
    renderDT(
      prettify_data(fdata()),
      options = list(pageLength = 15),
      style = "bootstrap" # to make it not ignore shiny theme
    )
  
  output$input_ui <- renderUI({
    tagList(
      selectInput("country", "Country",
                  choices = unique(fdata()$Country), selected = "EU27+1"),
      selectInput("source", "Source",
                  choices = unique(fdata()$Source), selected = "Nuclear"),
      sliderInput("year", "Year",
                  min = min(fdata()$Year), max = max(fdata()$Year), sep = "",
                  value = mean(fdata()$Year), round = TRUE, step = 1,
                  animate = animationOptions(interval = 3000)
      ),
      radioButtons("absolute", "Value type",
                   choiceNames = c("Absolute", "Percentages"),
                   choiceValues = c(TRUE, FALSE)) 
    )
  })
  
  output$src_by_country <-
    renderPlot({
      src_by_country(fdata(), input$country, input$year, input$absolute)
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$country, input$year,
              input$absolute)

  output$src_lines_by_country <-
    renderPlot({
      src_lines_by_country(fdata(), input$country, input$year, input$absolute)
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$country, input$year,
              input$absolute)

  output$src_by_country_by_year <-
    renderPlot({
      src_by_country_by_year(fdata(), input$country, input$year, input$absolute)
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$country, input$year,
              input$absolute)

  output$src_by_country_by_year_lollipop <-
    renderPlot({
      src_by_country_by_year_lollipop(
        fdata(), input$country, input$year, input$absolute
        )
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$country, input$year,
              input$absolute)

  output$country_by_year_by_source_lollipop <-
    renderPlot({
      country_by_year_by_source_lollipop(
        fdata(), input$year, input$source, input$absolute
        )
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$year, input$source,
              input$absolute)
  
  output$country_by_year_stacked  <-
    renderPlot({
      country_by_year_stacked(fdata(), input$year, input$absolute)
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$year,
              input$absolute)
  
  output$map_by_year_by_source <-
    renderPlot({
        map_by_year_by_source(
          fdata(), input$year, input$source, input$absolute
          )
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$year, input$source,
              input$absolute)
  
  output$map_maj_source_by_year <- 
    renderPlot({
      map_maj_source_by_year(fdata(), input$year, input$absolute)
    }) %>%
    bindCache(input$countries, input$years, input$sources, 
              input$year,
              input$absolute)
  
  session$allowReconnect("force")
  
  observe({
    val <- input$click_year$x
    updateSliderInput(session, "year", value =val)
  })
  
  output$drill_down <- renderText({
    "Use these controls to control the plots you see."
  })
}

shinyApp(ui = ui, server = server)

