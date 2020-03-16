bom_data <- read_csv("data/BOM_data.csv")
library(tidyverse)
bom_stations <- read_csv("data/BOM_stations.csv")
view(bom_stations)
view(bom_data)
#For each station, how many days have a minimum
#temperature, a maximum temperature and rainfall
#measurement recorded?
