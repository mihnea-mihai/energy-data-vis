sources <- list(
  fossil = c("Hard Coal", "Lignite", "Gas", "Other fossil"),
  renewable = c("Hydro", "Wind", "Solar", "Bioenergy", "Other renewables")
)
sources$base <- c(sources$fossil, sources$renewable, "Nuclear")

data <- data %>%
  mutate(across(Source, factor,
                levels = c(sources$base, "Demand", "Net imports"))) %>%
  filter(!is.na(Source))

sources$broad <- c("Net imports", "Fossil", "Renewables", "Nuclear", "Demand")

data <- data %>%
  mutate(Broad.Source = factor(case_when(
    Source %in% sources$fossil ~ "Fossil",
    Source %in% sources$renewable ~ "Renewables",
    Source == "Nuclear" ~ "Nuclear",
    Source == "Demand" ~ "Demand",
    Source == "Net imports" ~ "Net imports"
  ))) %>%
  mutate(across(Broad.Source, factor, levels = sources$broad))
