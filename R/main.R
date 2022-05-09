library(ndsbr)
library(tidyverse)
library(sf)
library(tmap)
library(GGally)
library(GWmodel)
library(spdep)

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

dotw_sp <- drivers_time_date %>% plot_dotw_sp()
mm
richards_sp <- drivers_time_date %>% plot_richards_sp()

sp_trips_summary <- drivers_time_date %>% extract_sp_summary(ID)

sp_driver_summary <- drivers_time_date %>% extract_sp_summary(DRIVER)

save_eda_plots()

# GWR ---------------------------------------------------------------------

source("R/gwr.R")

taz_gwr <- taz %>% arrange_taz()

map_removal <- taz_gwr %>% plot_taz_removal()

ggsave(
  "plot/map_removal.png", 
  plot = map_removal, 
  width = 3,
  height = 3.5,
  device = "png",
  dpi = 300
)

spear <- taz_gwr %>% plot_spear()

ggsave(
  filename = "plot/spear.png",
  plot = spear,
  device = "pdf",
  width = 4.5,
  height = 3.5,
  dpi = 300
)

sample <- taz_gwr %>% calc_sample_size()
write_csv(sample, "data/final_sample_size.csv")

global_summary <- taz_gwr %>% 
  calc_global_summary() %>% 
  xtable::xtable()

gwr_ind_var <- c(
  "AVI", "PD", "SND", "PAR", "DIS", "TSD", "DCSU", "LDI", "BSD", "DSC"
  )

taz_gwr <- taz_gwr %>% 
  select(
    -neigh, -id_taz, -area, -road_length, -DIST_TOTAL, -DIST_EXP, -DIST_SPD
  )

gwr_model_results <- calc_gwr(taz_gwr, gwr_ind_var)

gwr_diag_table <- extract_gwr_diag(gwr_model_results)

mmc_results <- gwr_model_results %>% calc_moran()

gwr_diag_table %>% 
  left_join(mmc_results, by = c("kernel" = "model"))

# Selecting best model -- manual process, check diagnostic (WIP)
gwr_chosen_model <- gwr_model_results[[2]]

taz_gwr_sp <- as(taz_gwr, "Spatial")

gwr_summary <- extract_gwr_summary(taz_gwr_sp, gwr_chosen_model)
