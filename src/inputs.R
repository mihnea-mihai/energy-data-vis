input_ui <- fluidPage(
  selectInput(
    "country",
    "Country",
    choices = unique(data$Country),
    selected = "EU27+1"
  ),
  selectInput(
    "source",
    "Source",
    choices = unique(data$Source),
    selected = "Nuclear"
  ),
  sliderInput(
    "year",
    "Year",
    min = min(data$Year),
    max = max(data$Year),
    sep = "",
    value = mean(data$Year),
    animate = animationOptions(interval = 500)
  ),
  radioButtons(
    "absolute", 
    "Value type",
    choiceNames = c("Absolute", "Percentages"),
    choiceValues = c(TRUE, FALSE)
  )
)
