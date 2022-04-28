

colormap = list(
  basic.sources = c(
    "Hard Coal" = "orangered4",
    "Lignite" = "saddlebrown",
    "Gas" = "darkgoldenrod",
    "Other fossil" = "darkgray",
    "Hydro" = "royalblue",
    "Wind" = "skyblue",
    "Solar" = "gold",
    "Bioenergy" = "forestgreen",
    "Other renewables" = "springgreen3",
    "Nuclear" = "orchid4",
    "Net imports" = "grey50",
    "Demand" = "white"
  ),
  
  broad.sources = c(
    "Renewables" = "forestgreen",
    "Fossil" = "brown",
    "Nuclear" = "orchid4",
    "Demand" = "white",
    "Net imports" = "grey50"
  )
)

theme_update(
  text = element_text(size = 20, color = "gray81"),
  rect = element_rect(fill = "gray14"),
  axis.text = element_text(color = "gray48"),
  panel.background = element_rect(fill = "gray10"),
  panel.grid = element_line(color = "gray15"),
  legend.key = element_rect(fill = "gray14")
)
