country_by_year_by_source_lollipop <- function(year, source) {
  data %>%
    filter(Year == year, Source == source, Country != "EU27+1", Production > 0) %>%
    arrange(Production) %>%
    mutate(Country = factor(Country, levels = Country)) %>%
    ggplot(aes(Country, Production, color = Source)) +
    geom_segment(
      aes(
        x = Country,
        xend = Country,
        y = 0,
        yend = Production
      ),
      size = 1
    ) +
    scale_color_manual(values = colormap$basic.sources) +
    geom_point(size = 4) +
    coord_flip() +
    scale_y_continuous(labels = add.TWh) +
    labs(title = "Generated energy by source",
         subtitle = paste(source, " (", year, ")", sep = "")) +
    theme(legend.position = "none")
}

country_by_year_stacked <- function(year) {
  
  ordered_countries <- data %>%
    filter(Source %in% c(sources$base, "Net imports"),
           Year == year,
           Production > 0) %>%
    group_by(Country) %>%
    summarise(Total.Production = sum(Production)) %>%
    arrange(Total.Production)
  
  data %>%
    filter(Year == year, Country != "EU27+1", Production > 0, Source != "Demand") %>%
    mutate(Country = factor(Country, levels = ordered_countries$Country)) %>%
    ggplot(aes(Production, Country, fill = Source)) +
    geom_col() +
    scale_fill_manual(values = colormap$basic.sources, limits = force) +
    labs(title = "Energy mix", subtitle = year) +
    theme(legend.position = "bottom") +
    scale_x_continuous(labels = add.TWh)
}

by_year_ui <-
  fluidPage(
    column(
      width = 8,
      plotOutput("country_by_year_stacked", height = "800px")
    ),
    column(
      width = 4,
      plotOutput("country_by_year_by_source_lollipop", height = "800px")
    )
  )