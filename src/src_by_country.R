src_by_country <- function(country, year) {
  filtered <- data %>%
    filter(Country == country)
  
  broad_data <- filtered %>%
    filter(Broad.Source %in% c("Renewables", "Fossil", "Nuclear")) %>%
    group_by(Year, Broad.Source) %>%
    summarise(Total.Production = sum(Production),
              .groups = "keep")
  
  demand_data <- filtered %>%
    filter(Broad.Source == "Demand")
  
  filtered %>%
    filter(Source %in% sources$base) %>%
    ggplot() +
    geom_area(aes(x = Year, y = Production, fill = Source)) +
    scale_fill_manual(values = colormap$basic.sources, limits = force) +
    labs(title = "Detailed energy generation sources evolution",
         subtitle = country) +
    geom_line(
      data = broad_data,
      aes(x = Year, y = Total.Production, group = Broad.Source),
      position = "stack",
      size = 1
    ) +
    geom_line(
      data = demand_data,
      aes(x = Year, y = Production, group = Broad.Source),
      color = "white",
      size = 1.5
    ) +
    geom_vline(
      xintercept = year,
      color = "white",
      size = 15,
      alpha = 0.15
    )
}

src_by_country_by_year <- function(country, year) {
  data %>%
    filter(
      Country == country & Year == year &
        Source %in% c(sources$base, "Net imports") & Production != 0
    ) %>%
    ggplot() +
    geom_col(aes(x = Broad.Source, y = Production,
                 fill = Source), width = 0.7) +
    scale_fill_manual(values = colormap$basic.sources, limits = force) +
    labs(title = "Energy generation sources",
         subtitle = paste(country, " (", year, ")", sep = ""))
}

src_by_country_ui <-
  fluidRow(
    column(
      width = 6,
      selectInput(
        "country",
        "Country",
        choices = unique(data$Country),
        selected = "EU27+1"
      ),
      plotOutput("src_by_country", click = "click_year"),
      plotOutput("src_lines_by_country")
    ),
    column(
      width = 6,
      sliderInput(
        "year",
        "Year",
        width = "100%",
        min = min(data$Year),
        max = max(data$Year),
        sep = "",
        value = mean(data$Year),
        animate = animationOptions(interval = 3000)
      ),
      plotOutput("src_by_country_by_year")
    )
  )
