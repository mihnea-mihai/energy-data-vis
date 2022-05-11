library(maps)
library(mapproj)
library(cowplot)

eu_map <- map_data("world") %>%
  mutate(Country = if_else(region == "UK", "United Kingdom", region))

map_by_year_by_source <- function(year, source, absolute = TRUE) {
  g <- data %>%
    filter(Source == source, Year == year, Production > 0,
           Country != "EU27+1") %>%
    merge(eu_map, all = F) %>%
    arrange(order) %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(color = "gray15",
                 aes(fill = if(absolute) Production else Percent)) +
    scale_fill_gradient(low = "gray14", high = colormap$basic.sources[source],
                        trans = "sqrt") +
    guides(fill = guide_legend(
      title = if(absolute) "Production (TWh)" else "Percent (%)")) +
    coord_map()
  ggdraw(g) + theme(panel.background = element_rect(fill = "gray14"))
}

maps_ui <- fluidPage(
  column(
    width = 6,
    plotOutput("map_by_year_by_source", height = "85rem")
    ),
  column(
    width = 6,
    plotOutput("country_by_year_by_source_lollipop", height = "85rem")
  )
)