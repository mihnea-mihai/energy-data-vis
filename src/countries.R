data <- data %>%
  filter(Country != "EU-27") %>%
  mutate(across(Country, factor))

all_countries <- unique(data$Country)
