filter_data <- function(countries, years, sources) {
  data %>%
    filter(
      Country %in% countries,
      Year >= years[1], Year <= years[2],
      Source %in% sources
      )
}

prettify_data <- function(dta)
    dta %>%
      mutate(across(c(Production, Percent), round, digits = 2))

