library(ndsbr)
library(tidyverse)
library(sf)
library(tmap)
library(GGally)
library(GWmodel)
library(spdep)
library(rgeoda)

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
rm(drivers)

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
  
  st_write(taz, "data/taz_complete.gpkg", append = FALSE)
  
  rm(census_data)
}

dist_map <- taz %>% plot_dist_map()

ggsave(
  filename = "plot/taz_dist.png", 
  dist_map,
  device = "png",
  height = 4.5,
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
  device = "png",
  width = 4.5,
  height = 3.5,
  dpi = 300
)

sample <- taz_gwr %>% calc_sample_size()
write_csv(sample, "table/final_sample_size.csv")

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

# Selecting best model -- manual process, check diagnostic (WIP)
gwr_chosen_model <- gwr_model_results[[2]]

taz_gwr_sp <- as(taz_gwr, "Spatial")

gwr_summary <- extract_gwr_summary(taz_gwr_sp, gwr_chosen_model)

gwr_summary_plots <- plot_gwr_summary()

map2(
  gwr_summary_plots, 
  c("plot/summary_mean_sp.png", "plot/summary_sd_sp.png"),
  ~tmap_save(.x, .y, height = 3.5, width = 3, units = "in", dpi = 300)
)

gwr_results_plots <- plot_gwr_results(gwr_ind_var)

map2(
  gwr_results_plots,
  paste0("plot/gwr_", gwr_ind_var, ".png"),
  ~tmap_save(.x, .y, height = 3.5, width = 3, units = "in", dpi = 300)
)

local_r2_plot <- plot_local_r2(gwr_chosen_model)

tmap_save(
  local_r2_plot, 
  "plot/gwr_local_r2.png",
  height = 3.5,
  width = 3,
  units = "in", 
  dpi = 300
)

taz_coef <- count_taz_coef(gwr_chosen_model[["SDF"]]@data)

write_csv(taz_coef, "table/gwr_taz_coef.csv")

sp_plot <- plot_sp(taz_gwr)

ggsave(
  "plot/taz_SP.png",
  sp_plot,
  width = 3.5,
  height = 3.5,
  dpi = 300,
  device = "png"
)

moran_sp_results <- calc_moran_sp()

# LISA --------------------------------------------------------------------

source("R/lisa.R")

lisa_gwr_coefs <- map(gwr_ind_var, calc_lisa, gwr_chosen_model[["SDF"]])
lisa_sp <- calc_lisa("SP", taz_gwr)
lisa_r2 <- calc_lisa("Local_R2", gwr_chosen_model[["SDF"]])

lisa_sp_plot <- plot_lisa(lisa_sp, "SP")
lisa_gwr_coefs_plot <- map2(lisa_gwr_coefs, gwr_ind_var, plot_lisa)
lisa_r2_plot <- plot_lisa(lisa_r2, "Local R²")

lisa_maps <- list(lisa_gwr_coefs_plot, lisa_sp_plot, lisa_r2_plot)
lisa_names <- c(gwr_ind_var, "SP", "Local_R2")

map2(lisa_gwr_coefs_plot, gwr_ind_var, save_lisa_plots)
save_lisa_plots(lisa_sp_plot, "SP")
save_lisa_plots(lisa_r2_plot, "local_r2")

# misc --------------------------------------------------------------------

source("R/misc.R")

tmap_options(check.and.fix = TRUE)

cwb <- geobr::read_municipality(code_muni = 4106902, year = 2010)

road_ctb <- fix_road_ctb()
road_ctb_plot <- plot_road_ctb(road_ctb)

road_cwb <- fix_road_cwb()
road_cwb_plot <- plot_road_cwb(road_cwb)

map2(
  c("plot/road_ctb.png", "plot/road_cwb.png"),
  list(road_ctb_plot, road_cwb_plot),
  ~ggsave(.x, .y, width = 6, height = 4.5, device = "png")
)

sp_cluster <- create_sp_cluster(lisa_sp)
area_calma_plot <- plot_area_calma(sp_cluster)

tmap_save(
  area_calma_plot,
  "plot/area_calma_plot.png",
  units = "in",
  height = 4.5,
  width = 4,
  dpi = 300
)

zoning_plot <- plot_land_zoning()

tmap_save(
  tm = zoning_plot, filename = "plot/zoning_map.png",
  units = "in", height = 4.5, width = 6, dpi = 300
)

# address_plot <- plot_address()
# 
# tmap_save(
#   address_plot, "plot/address_map.png", units = "in", height = 4.5, width = 6,
#   dpi = 300
# )

road_sp_cluster_plot <- plot_roads_cluster(road_ctb, sp_cluster)

ggsave(
  "plot/road_sp_cluster_map.png", road_sp_cluster_plot, width = 6, 
  height = 4.5, device = "png"
)

od_plot <- plot_travel_demand(sp_cluster)

ggsave(
  "plot/od_plot.png",
  plot = od_plot,
  device = "png",
  height = 3.5,
  width = 5,
  dpi = 300
)

camera_buffer_plot <- plot_spd_cameras()

ggsave(
  "plot/cam_buffer_plot.png",
  plot = camera_buffer_plot,
  device = "png",
  height = 4.5,
  width = 6,
  dpi = 300
)

taz_sp <- extract_sp_groups(lisa_sp)
wilcox_table <- calc_wilcox(taz_sp, gwr_ind_var, "cluster")
write_csv(wilcox_table, "table/wilcox_table.csv")

wilcox_hist <- plot_wilcox_hist()

ggsave(
  "plot/wilcox_hist.png", wilcox_hist, width = 6, height = 3.5, device = "png"
)

taz_par <- extract_par_groups(taz_gwr)
wilcox_table_par <- calc_wilcox(
  taz_par,
  gwr_ind_var[gwr_ind_var != "PAR"],
  "group"
)
write_csv(wilcox_table_par, "table/wilcox_table_par.csv")

par_hist <- plot_par_hist()
ggsave(
  "plot/par_hist.png", par_hist, width = 6, height = 3.5, device = "png"
)
