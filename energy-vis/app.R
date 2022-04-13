#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("httr")
library("readxl")
library("dplyr")
library("ggplot2")
library("scales")
GET("https://query.data.world/s/dpqucaqcnzwoj45c4ft6mxihxzmmpe",
    write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)
data <- df %>%
  select(Year = Year, Country = Area, Source = Variable,
         Generation = `Generation (TWh)`, Percent = `Share of production (%)`)
sources <- list(
  fossil = c("Hard Coal", "Lignite", "Gas", "Other fossil"),
  renewable = c("Hydro", "Wind", "Solar", "Bioenergy", "Other renewables"),
  nuclear = c("Nuclear")
)
sources$base <- c(sources$renewable, sources$fossil, sources$nuclear)
sources$palette <- list("Hard Coal" = "gray10",
                        "Lignite" = "saddlebrown",
                        "Gas" = "darkgoldenrod",
                        "Other fossil" = "darkgray",
                        "Hydro" = "royalblue",
                        "Wind" = "deepskyblue",
                        "Solar" = "gold",
                        "Bioenergy" = "forestgreen",
                        "Other renewables" = "springgreen3",
                        "Nuclear" = "orchid4")

assign_broad_source <- function(src.vec) {
  res = rep_len(NA, length(src.vec))
  res[src.vec %in% sources$fossil] = "Fossil"
  res[src.vec %in% sources$renewable] = "Renewables"
  res[src.vec %in% sources$nuclear] = "Nuclear"
  return(res)
}

data <- data %>%
  mutate(Broad.Source = assign_broad_source(Source)) %>%
  filter(!is.na(Broad.Source))

data$Source <- factor(data$Source, levels = sources$base)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Energy sources per Country per Year"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year",
                        min = 2000,
                        max = 2020,
                        value = 2010,
                        sep = ""),
            selectInput("country",
                        "Country",
                        choices = unique(data$Country),
                        selected = "Romania")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("energPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$energPlot <- renderPlot({
        filtered_data <- data %>%
          filter(Country == input$country & Year == input$year
                 & Source %in% sources$base)
        filtered_data %>%
          ggplot() +
          geom_col(mapping = aes(x = Broad.Source, y = Percent, fill = Source)) +
          ggtitle(paste("Energy sources in ", input$country,
                        " (", input$year, ")", sep = "")) +
          scale_fill_manual(values = sources$palette)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
