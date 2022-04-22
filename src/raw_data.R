filter_data <-
  function(country,
           year_interval,
           source,
           production_interval) {
    data %>%
      filter(
        Country %in% country &
          Year >= year_interval[1] & Year <= year_interval[2] &
          Source %in% source &
          Production >= production_interval[1] &
          Production <= production_interval[2]
      )
  }

raw_data_ui <-
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year_interval",
        "Year interval",
        min = min(data$Year),
        max = max(data$Year),
        value = c(min(data$Year), max(data$Year)),
        sep = ""
      ),
      selectInput(
        "filter_country",
        "Country",
        choices = unique(data$Country),
        multiple = TRUE,
        selected = c("EU27+1")
      ),
      selectInput(
        "source",
        "Source",
        choices = unique(data$Source),
        multiple = TRUE,
        selected = c("Demand")
      ),
      sliderInput(
        "production_interval",
        "Production interval",
        min = as.integer(min(data$Production)),
        max = as.integer(max(data$Production)),
        value = c(as.integer(min(data$Production)),
                  as.integer(max(data$Production))),
        step = 100, round = 3
      )
    ),
    mainPanel(dataTableOutput("raw_data"))
  )
