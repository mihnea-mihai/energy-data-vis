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
    scale_y_continuous(labels = add.TWh) +
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

src_lines_by_country <- function(country, year) {
  data %>%
    filter(
      Country == country &
        Broad.Source %in% c("Renewables", "Fossil",
                            "Nuclear", "Net imports")
    ) %>%
    mutate(Production = if_else(Production < 0, 0, Production)) %>%
    group_by(Year, Broad.Source) %>%
    summarise(Total.Production = sum(Production),
              .groups = "keep") %>%
    ggplot(aes(x = Year, y = Total.Production, color = Broad.Source)) +
    geom_line(size = 2) +
    scale_color_manual(values = colormap$broad.sources, limits = force) +
    labs(title = "High-level energy generation sources evolution",
         subtitle = country) +
    scale_y_continuous(labels = add.TWh) +
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
        Source %in% c(sources$base, "Net imports") & Production > 0
    ) %>%
    ggplot() +
    geom_col(aes(x = Broad.Source, y = Production,
                 fill = Source), width = 0.7) +
    scale_fill_manual(values = colormap$basic.sources, limits = force) +
    labs(title = "Energy generation sources",
         subtitle = paste(country, " (", year, ")", sep = "")) +
    scale_y_continuous(labels = add.TWh) +
    coord_flip()
}

src_by_country_by_year_lollipop <- function(country, year) {
  data %>%
    filter(Country == country,
           Production > 0,
           Year == year,
           Source != "Demand") %>%
    arrange(Production) %>%
    mutate(Source = factor(Source, levels = Source)) %>%
    ggplot(aes(Source, Production, color = Broad.Source)) +
    geom_segment(aes(
      x = Source,
      y = 0,
      xend = Source,
      yend = Production
    ), size = 1) +
    geom_point(size = 4) +
    scale_y_continuous(labels = add.TWh) +
    coord_flip() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = colormap$broad.sources, limits = force) +
    labs(title = "Energy generation sources",
         subtitle = paste(country, " (", year, ")", sep = ""))
}

src_by_country_ui <-
  fluidPage(
    column(
      width = 6,
      plotOutput("src_by_country", click = "click_year"),
      plotOutput("src_lines_by_country", click = "click_year")
    ),
    column(
      width = 6,
      plotOutput("src_by_country_by_year"),
      plotOutput("src_by_country_by_year_lollipop")
    )
  )