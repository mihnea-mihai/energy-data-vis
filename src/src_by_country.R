src_by_country <- function(dta, country, year, absolute = TRUE, broad = FALSE) {
  fdata <- dta %>%
    filter(Country == country, Source %in% sources$base)
  
  broad_data <- fdata %>%
    group_by(Year, Broad.Source, Country) %>%
    summarise(Total = sum(if(absolute) Production else Percent))
  
  demand_data <- dta %>%
    filter(Country == country, Broad.Source == "Demand")

  fdata %>%
    ggplot() +
    geom_area(aes(x = Year, y = if(absolute) Production else Percent,
                  fill = if(broad) Broad.Source else Source, group = Source)) +
    scale_fill_manual(
      values = if(broad) colormap$broad.sources else colormap$basic.sources,
      limits = force
      ) +
    guides(fill = guide_legend(
        title = if(broad) "Broad source" else "Source")
        ) +
    labs(title = "Yearly energy production evolution",
         subtitle = paste(year, country)) +
    ylab(if(absolute) "Production (TWh)" else "Percent (%)") +
    geom_line(data = broad_data,
              aes(x = Year, y = Total, group = Broad.Source),
              position = "stack", size = 1) +
    geom_line(data = demand_data,
              aes(x = Year, y = if(absolute) Production else Percent,
                  group = Broad.Source, color = Broad.Source),
              size = 1.5) +
    scale_color_manual(values = colormap$basic.sources, limits = force) +
    geom_vline(xintercept = year, color = "white", size = 15, alpha = 0.1)
}

src_lines_by_country <- function(dta, country, year, absolute = TRUE) {
  dta %>%
    filter(Country == country, Source != "Demand") %>%
    mutate(Production = if_else(Production < 0, 0, Production)) %>%
    mutate(Percent = if_else(Percent < 0, 0, Percent)) %>% 
    group_by(Year, Broad.Source) %>%
    summarise(Total = sum(if(absolute) Production else Percent)) %>%
    ggplot(aes(x = Year, y = Total, color = Broad.Source)) +
    geom_line(size = 2) +
    scale_color_manual(values = colormap$broad.sources, limits = force) +
    labs(title = "Yearly energy production evolution",
         subtitle = paste(year, country)) +
    ylab(if(absolute) "Total Production (TWh)" else "Total Percent (%)")+
    geom_vline(xintercept = year, color = "white", size = 15, alpha = 0.05)
}

src_by_country_by_year <- function(dta, country, year, absolute = TRUE) {
  dta %>%
    filter(Country == country, Year == year,
           Source %in% c(sources$base, "Net imports"), Production > 0) %>%
    ggplot() +
    geom_col(aes(x = Broad.Source, y = if(absolute) Production else Percent,
                 fill = Source)) +
    scale_fill_manual(values = colormap$basic.sources, limits = force) +
    labs(title = "Energy generation sources", 
         subtitle = paste(country, " (", year, ")", sep = "")) +
    ylab(if(absolute) "Production (TWh)" else "Percent (%)") +
    xlab("Broad source") +
    theme(legend.position = "bottom") +
    coord_flip()
}

src_by_country_by_year_lollipop <- function(dta, country, year, absolute = TRUE) {
  dta %>%
    filter(Country == country, Production > 0, Year == year,
           Source != "Demand") %>%
    arrange(Production) %>%
    mutate(Source = factor(Source, levels = Source)) %>%
    ggplot(aes(x = Source, y = if(absolute) Production else Percent,
               color = Broad.Source)) +
    geom_segment(aes(x = Source, y = 0, xend = Source,
                     yend = if(absolute) Production else Percent),
                 size = 1) +
    geom_point(size = 4) +
    coord_flip() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = colormap$broad.sources, limits = force) +
    labs(title = "Energy generation sources",
         subtitle = paste(country, " (", year, ")", sep = "")) +
    ylab(if(absolute) "Production (TWh)" else "Percent (%)")
}

src_by_country_ui <-
  fluidPage(
    column(
      width = 6,
      plotOutput("src_by_country", click = "click_year", height = "40rem"),
      plotOutput("src_lines_by_country", click = "click_year", height = "40rem")
    ),
    column(
      width = 6,
      plotOutput("src_by_country_by_year", height = "40rem"),
      plotOutput("src_by_country_by_year_lollipop", height = "40rem")
    )
  )