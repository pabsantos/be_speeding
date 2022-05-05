library(ndsbr)
library(tidyverse)
library(sf)

source("R/nds.R")

drivers <- load_sample()

full_time <- nds_calc_time(drivers, by = DRIVER, units = "hours") %>% 
  pull(TIME) %>% 
  sum()

drivers_lines <- drivers %>% nds_create_lines(x = LONG, y = LAT)

full_distance <- drivers_lines %>% nds_calc_dist(
  geom = wkt_lines,
  by = DRIVER,
  units = "kilometers"
  ) %>% 
  pull(DIST) %>% 
  sum()

drivers_points <- drivers %>% 
  nds_create_points(x = LONG, y = LAT) 
