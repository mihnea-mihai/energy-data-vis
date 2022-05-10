country_by_year_by_source_lollipop <- function(year, source, absolute = TRUE) {
  data %>%
    filter(Year == year, Source == source, Country != "EU27+1", Production > 0) %>%
    arrange(if(absolute) Production else Percent) %>%
    mutate(Country = factor(Country, levels = Country)) %>%
    ggplot(aes(Country, if(absolute) Production else Percent, color = Source)) +
    geom_segment(aes(x = Country, xend = Country, y = 0,
                     yend = if(absolute) Production else Percent),
                 size = 1) +
    scale_color_manual(values = colormap$basic.sources) +
    geom_point(size = 4) +
    coord_flip() +
    labs(title = "Generated energy by source",
         subtitle = paste(source, " (", year, ")", sep = "")) +
    ylab(if(absolute) "Production (TWh)" else "Percent (%)") +
    theme(legend.position = "none")
}

country_by_year_stacked <- function(year, absolute = TRUE) {
  ordered_countries <- data %>%
    filter(Source %in% c(sources$base, "Net imports"),
           Year == year, Production > 0) %>%
    group_by(Country) %>%
    summarise(Total = sum(Production)) %>%
    arrange(Total)
  
  broad_data <- data %>%
    filter(Year == 2010, Production > 0, Broad.Source != "Demand", 
           Country != "EU27+1") %>%
    mutate(across(Broad.Source, factor, levels = rev(sources$broad))) %>%
    group_by(Country, Broad.Source) %>%
    arrange(Broad.Source) %>%
    summarise(Subtotal = sum(Production)) %>%
    mutate(Country = factor(Country, levels = ordered_countries$Country)) %>%
    group_by(Country) %>%
    summarise(Cumul = cumsum(Subtotal))
  
  data %>%
    filter(Year == year, Country != "EU27+1", Production > 0, Source != "Demand") %>%
    mutate(Country = factor(Country, levels = ordered_countries$Country)) %>%
    ggplot(aes(if(absolute) Production else Percent, Country, fill = Source)) +
    geom_col() +
    scale_fill_manual(values = colormap$basic.sources, limits = force) +
    labs(title = "Energy mix", subtitle = year) +
    theme(legend.position = "bottom") +
    xlab(if(absolute) "Production (TWh)" else "Percent (%)") +
    geom_segment(data = broad_data,
                 mapping = aes(x = Cumul, xend = Cumul,
                               y = as.numeric(Country) - 0.5,
                               yend = as.numeric(Country) + 0.5
                 ),
                 inherit.aes = FALSE, color = "black", size = 1)
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