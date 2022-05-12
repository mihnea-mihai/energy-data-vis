library(maps)
library(mapproj)
library(cowplot)

eu_map <- map_data("world") %>%
  mutate(Country = if_else(region == "UK", "United Kingdom", region))

map_by_year_by_source <- function(dta, year, source, absolute = TRUE) {
  g <- dta %>%
    filter(Source == source, Year == year, Production > 0,
           Country != "EU27+1") %>%
    merge(eu_map, all = F) %>%
    arrange(order) %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(color = "gray15",
                 aes(fill = if(absolute) Production else Percent)) +
    scale_fill_gradient(low = "gray14", high = colormap$basic.sources[source],
                        trans = "sqrt") +
    labs(title = "Energy generation by source in Europe",
         subtitle = paste(source, year)) +
    guides(fill = guide_legend(
      title = if(absolute) "Production (TWh)" else "Percent (%)")) +
    coord_map()
  ggdraw(g) + theme(panel.background = element_rect(fill = "gray14"))
}

map_maj_source_by_year <- function(dta, year, absolute = TRUE) {
  g <- dta %>%
    filter(Year == year, Production > 0,
           Country != "EU27+1", Source != "Demand") %>%
    group_by(Country) %>%
    select(-Broad.Source, -Year) %>%
    mutate(Rank = rank(desc(if(absolute) Production else Percent))) %>%
    filter(Rank == 1) %>%
    merge(eu_map, all = F) %>%
    arrange(order) %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(color = "gray15",
                 aes(alpha = if(absolute) Production else Percent, 
                     fill = Source)) +
    scale_fill_manual(values = colormap$basic.sources, limits = force) +
    scale_alpha_continuous(trans = "sqrt") +
    guides(alpha = guide_legend(
      title = if (absolute) "Production (TWh)" else "Percent (%)")
    ) +
    coord_map()
  ggdraw(g) + theme(panel.background = element_rect(fill = "gray14"))
}
