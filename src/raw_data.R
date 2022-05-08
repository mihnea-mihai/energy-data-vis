library(DT)

filter_data <- function(country, year, source) {
  data %>%
    filter(Country == country, Year == year, Source == source)
}

raw_data_ui <-
    mainPanel(DTOutput("raw_data"))
