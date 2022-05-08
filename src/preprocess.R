library(httr)
library(readxl)

if (file.exists("../data/raw.rds")) {
  raw <- readRDS("../data/raw.rds")
} else {
  GET(
    "https://query.data.world/s/dpqucaqcnzwoj45c4ft6mxihxzmmpe",
    write_disk(tf <- tempfile(fileext = ".xlsx"))
  )
  raw <- read_excel(tf)
  dir.create("../data")
  saveRDS(raw, file = "../data/raw.rds")
}

data <- raw %>%
  select(
    Year = Year,
    Country = Area,
    Source = Variable,
    Production = `Generation (TWh)`
  )

data <- data %>%
  filter(Source == "Demand") %>%
  select(-Source) %>%
  rename(Total.Production = Production) %>%
  merge(data) %>%
  mutate(Percent = Production / Total.Production * 100) %>%
  select(-Total.Production)
