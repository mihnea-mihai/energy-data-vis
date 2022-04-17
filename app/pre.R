library(tidyverse)
library(httr)
library(readxl)

# embed code from https://data.world/makeovermonday/2021w5
GET("https://query.data.world/s/rnsywm4k4ae3vz2iitu6cg4sww6xbc", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
raw <- read_excel(tf)