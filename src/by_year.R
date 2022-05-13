country_by_year_by_source_lollipop <- 
  function(dta, year, source, absolute = TRUE) {
  dta %>%
    filter(Year == year, Source == source,
           Country != "EU27+1", Production > 0) %>%
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

country_by_year_stacked <- function(dta, year, absolute = TRUE, broad = FALSE) {
  ordered_countries <- dta %>%
    filter(Source != "Demand", Production > 0, Year == year) %>%
    group_by(Country) %>%
    summarise(Total = sum(Production)) %>%
    arrange(Total)
  
  broad_data <- dta %>%
    filter(Production > 0, Broad.Source != "Demand", Country != "EU27+1",
           Year == year) %>%
    mutate(across(Broad.Source, factor, levels = rev(sources$broad))) %>%
    group_by(Country, Broad.Source) %>%
    arrange(Broad.Source) %>%
    summarise(Subtotal = sum(if(absolute) Production else Percent)) %>%
    mutate(Country = factor(Country, levels = ordered_countries$Country)) %>%
    group_by(Country) %>%
    summarise(Cumul = cumsum(Subtotal))
  
  dta %>%
    filter(Country != "EU27+1", Production > 0, Source != "Demand",
           Year == year) %>%
    mutate(Country = factor(Country, levels = ordered_countries$Country)) %>%
    ggplot(aes(if(absolute) Production else Percent, Country,
               fill = if(broad) Broad.Source else Source, group = Source)) +
    geom_col() +
    scale_fill_manual(
      values = if(broad) colormap$broad.sources else colormap$basic.sources,
      limits = force
    ) +    
    guides(fill = guide_legend(
      title = if(broad) "Broad source" else "Source")
    ) +
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
