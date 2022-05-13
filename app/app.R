library(shiny)
library(shinythemes)
library(DT)
source("init.R")

ui <- fluidPage(title = "EU energy visualisation",
  theme = shinytheme("darkly"),
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("country", "Country",
                  choices = unique(data$Country), selected = "EU27+1"),
      selectInput("source", "Source",
                  choices = unique(data$Source), selected = "Nuclear"),
      sliderInput("year", "Year",
                  min = min(data$Year), max = max(data$Year), sep = "",
                  value = mean(data$Year), round = TRUE, step = 1,
                  animate = animationOptions(interval = 3000)),
      radioButtons("absolute", "Value type", inline = TRUE,
                   choiceNames = c("Absolute", "Percentages"),
                   choiceValues = c(TRUE, FALSE)),
      radioButtons("broad", "Source type", inline = TRUE,
                   choiceNames = c("Basic", "Broad"),
                   choiceValues = c(FALSE, TRUE))        
    ),
    mainPanel(width = 10,
      tabsetPanel(
        tabPanel("Raw data",
          sidebarLayout(
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
        tabPanel("Europe energy mix",
          fluidRow(
            column(width = 6,
                   plotOutput("map_maj_source_by_year", height = "85rem")),
            column(width = 6,
                   plotOutput("country_by_year_stacked", height = "85rem"))
          )
        ),
        tabPanel("Source overview",
          fluidRow(
            column(width = 6,
                   plotOutput("map_by_year_by_source", height = "85rem")),
            column(width = 6,
                   plotOutput("country_by_year_by_source_lollipop",
                              height = "85rem"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  fdata <- reactive(
    filter_data(input$countries, input$years, input$sources)
  )
  
  observeEvent(input$years,
    updateSliderInput(session, "year", value = input$year,
                      min = min(fdata()$Year), max = max(fdata()$Year))
  )
  
  observeEvent(input$countries,
    updateSelectInput(session, "country", selected = input$country,
                      choices = unique(fdata()$Country))
  )
  
  observeEvent(input$sources,
    updateSelectInput(session, "source", selected = input$source,
                      choices = unique(fdata()$Source))
  )
  
  observeEvent(input$click_year,
    updateSliderInput(session, "year", value = input$click_year$x)
  )
  
  output$raw_data <-
    renderDT(
      prettify_data(fdata()),
      options = list(pageLength = 15),
      style = "bootstrap" # to make it not ignore shiny theme
    )
  
  output$src_by_country <-
    renderPlot(
      src_by_country(
        fdata(), input$country, input$year, input$absolute, input$broad
        )
    ) %>%
    bindCache(fdata(), input$country, input$year, input$absolute, input$broad)
  
  output$src_lines_by_country <-
    renderPlot({
      src_lines_by_country(fdata(), input$country, input$year, input$absolute)
    }) %>%
    bindCache(fdata(), input$country, input$year, input$absolute)
  
  output$src_by_country_by_year <-
    renderPlot({
      src_by_country_by_year(fdata(), input$country, input$year, input$absolute)
    }) %>%
    bindCache(fdata(), input$country, input$year, input$absolute)

  output$src_by_country_by_year_lollipop <-
    renderPlot({
      src_by_country_by_year_lollipop(
        fdata(), input$country, input$year, input$absolute
        )
    }) %>%
    bindCache(fdata(), input$country, input$year, input$absolute)

  output$country_by_year_by_source_lollipop <-
    renderPlot({
      country_by_year_by_source_lollipop(
        fdata(), input$year, input$source, input$absolute
        )
    }) %>%
    bindCache(fdata(), input$year, input$source, input$absolute)
  
  output$country_by_year_stacked  <-
    renderPlot({
      country_by_year_stacked(fdata(), input$year, input$absolute, input$broad)
    }) %>%
    bindCache(fdata(), input$year, input$absolute, input$broad)
  
  output$map_by_year_by_source <-
    renderPlot({
        map_by_year_by_source(
          fdata(), input$year, input$source, input$absolute
          )
    }) %>%
    bindCache(fdata(), input$year, input$source, input$absolute)
  
  output$map_maj_source_by_year <- 
    renderPlot({
      map_maj_source_by_year(fdata(), input$year, input$absolute, input$broad)
    }) %>%
    bindCache(fdata(), input$year, input$absolute, input$broad)
  
  session$allowReconnect("force")
}

shinyApp(ui = ui, server = server)

