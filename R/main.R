library(ndsbr)
library(tidyverse)
library(sf)
library(tmap)

# NDS sample --------------------------------------------------------------

source("R/nds.R")

drivers <- load_sample()

full_time <- drivers %>% calc_time()

drivers_lines <- drivers %>% nds_create_lines(x = LONG, y = LAT)

full_distance <- drivers_lines %>% calc_dist()

drivers_valid <- drivers %>% 
  filter(CIDADE == "Curitiba", LIMITE_VEL != "NPI", VALID_TIME == "Yes")

valid_time <- drivers_valid %>% calc_time()

drivers_valid_lines <- drivers_valid %>% nds_create_lines(x = LONG, y = LAT)

valid_distance <- drivers_valid_lines %>% calc_dist()

exp_distance <- drivers_valid_lines %>% calc_exp_dist(exp = 10)

spd_distance <- drivers_valid_lines %>% calc_spd_dist(spd = 5)

valid_trips <- unique(drivers_valid_lines$ID) %>% length()

valid_distance_summary <- drivers_valid_lines %>% extract_dist_summary()

valid_trips_summary <- drivers_valid_lines %>% extract_trips_summary()

save_nds_results()

rm(drivers_lines)

# TAZ construction --------------------------------------------------------

source("R/taz.R")

if (file.exists("data/taz_complete.gpkg")) {
  taz <- st_read("data/taz_complete.gpkg")
} else {
  taz <- load_taz()
  
  census_data <- load_census_data()
  
  taz <- add_census_data(taz, census_data) %>% 
    add_snd() %>% 
    add_par() %>% 
    add_dis() %>% 
    add_tsd() %>% 
    add_dcsu() %>% 
    add_ldi() %>% 
    add_bsd() %>% 
    add_dsc() %>% 
    add_spd_exp_dist() %>% 
    remove_na_unit()
  
  st_write(taz, "data/taz_complete.gpkg")
  
  rm(census_data)
}

dist_map <- taz %>% plot_dist_map()

ggsave(
  filename = "plot/taz_dist.png", 
  dist_map,
  device = "png",
  height = 3.5,
  width = 3.5,
  dpi = 300
)

var_maps <- taz %>% plot_var_maps()

var_maps %>% save_var_maps()

# EDA ---------------------------------------------------------------------

source("R/eda.R")

drivers_time_date <- drivers_valid_lines %>% 
  transform_km() %>%
  extract_time_date()

dotw_dist <- drivers_time_date %>% plot_dotw_dist()

dotw_trips <- drivers_time_date %>% plot_dotw_trips()

hotd_dist <- drivers_time_date %>% plot_hotd_dist()

hotd_trips <- drivers_time_date %>% plot_hotd_trips()

hotd_sp <- drivers_time_date %>% plot_hotd_sp()

