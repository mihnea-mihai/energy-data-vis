library(tidyverse)

source("../src/colormap.R")

source("../src/preprocess.R")
source("../src/sources.R")
source("../src/countries.R")
add.TWh <- function(x) {
  paste(x, "TWh")
}
source("../src/raw_data.R")
source("../src/src_by_country.R")
