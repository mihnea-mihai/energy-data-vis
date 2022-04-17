library(tidyverse)

data <- readRDS("../docs/data.rds")
sources <- readRDS("../docs/sources.rds")

theme_update(text = element_text(size = 20, color = "gray81"),
             rect = element_rect(fill = "gray14"),
             axis.text = element_text(color = "gray48"),
             panel.background = element_rect(fill = "gray10"),
             panel.grid = element_line(color = "gray15"),
             legend.key = element_rect(fill = "gray14")
)

src_by_country_by_year <- function(country, year) {
  filtered_data <- data %>%
    filter(Country == country & Year == year & Source %in% sources$base)
  filtered_data %>%
    ggplot() +
    geom_col(aes(x = Broad.Source, y = Production,
                 fill = Source), width = 0.7) +
    scale_fill_manual(values = sources$palette) +
    labs(title = "Energy generation sources",
         subtitle = paste(country, " (", year, ")", sep = ""))
}

src_by_country <- function(country) {
  broad_data <- data %>%
    filter(Country == country & Source %in% sources$base) %>%
    group_by(Year, Broad.Source) %>%
    summarise(Total.Production = sum(Production), .groups = "keep")
  
  demand_data <- data %>%
    filter(Country == country & Source == "Demand")
  
  data %>%
    filter(Country == country & Source %in% sources$base) %>%
    ggplot() +
    geom_area(aes(x = Year, y = Production, fill = Source)) +
    scale_fill_manual(values = sources$palette) +
    geom_line(data = broad_data,
              mapping = aes(x = Year, y = Total.Production,
                            group = Broad.Source),
              position = "stack", size = 1) +
    geom_line(data = demand_data,
              mapping = aes(x = Year, y = Production),
              color = "white", size = 1.5) +
    labs(title = "Detailed energy generation sources evolution",
         subtitle = country)
}

src_lines_by_country <- function(country) {
  demand_data <- data %>%
    filter(Country == country & Source == "Demand")
  
  production_data <- data %>%
    filter(Country == country & !is.na(Broad.Source)) %>%
    group_by(Year) %>%
    summarise(Total.Production = sum(Production), .groups = "keep")
  
  import_data <- cbind(demand_data, production_data)
  
  import_data <- import_data %>%
    select(1, Total.Production, Production) %>%
    mutate(Imports = Production - Total.Production)
  
  data %>%
    filter(Country == country & !is.na(Broad.Source)) %>%
    group_by(Year, Broad.Source) %>%
    summarise(Total.Production = sum(Production), .groups = "keep") %>%
    ggplot(aes(x = Year, y = Total.Production, color = Broad.Source)) +
    geom_line(size = 2) +
    scale_color_manual(values = sources$broad.palette) +
    geom_line(data = import_data, aes(x = Year, y = Imports),
              color = "white", size = 2) +
    labs(title = "High-level energy generation sources evolution",
         subtitle = country)
}
