output05 <- glue("{output}/05/")

# Calculating LISA ----------------------------------------------------

calc_lisa <- function(var, taz) {
  # Convert taz to sf, if necessary
  class <- class(taz)
  if ("sf" %in% class) {
    print("taz in 'sf' format!")
  } else {
    taz <- st_as_sf(taz)
  }
  
  # Removing NA
  taz <- taz %>% 
    drop_na(var)
  
  # Spatial weighting
  queen_w <- queen_weights(taz)
  
  # LISA calc
  var <- as.character(var)
  lisa <- local_moran(queen_w, taz[var])
  
  # Results
  taz$cluster <- as.factor(lisa$GetClusterIndicators())
  levels(taz$cluster) <- lisa$GetLabels()
  
  # p-values
  taz$lisa_pvalues <- lisa$GetLocalSignificanceValues()
  
  return(taz)
}

lisa_coefs <- map(gwr_ind_var, calc_lisa, gwr_chosen_model[["SDF"]])
lisa_sp <- calc_lisa("SP", taz_gwr)
lisa_lm_sp <- calc_lisa("SP_LM", summary_gwr[["SDF"]])
lisa_r2 <- calc_lisa("Local_R2", gwr_chosen_model[["SDF"]])
lisa_sd <- calc_lisa("SP_LSD", summary_gwr[["SDF"]])

# Plotting LISA maps ------------------------------------------------------

make_lisa_maps <- function(lisa_taz, var) {
  
  # Legend and colors
  label <- paste("Local Moran\nClusters: ", var, sep = "")
  cluster_colors <- c(
    "#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8", "#464646", "#999999"
  )
  
  # Plot
  p1 <- ggplot() +
    geom_sf(data = taz, fill = "Grey70", color = "Grey50", lwd = 0.1) +
    geom_sf(data = lisa_taz, aes(fill = cluster), color = "Grey20", lwd = 0.1) +
    theme_void() +
    scale_fill_manual(values = cluster_colors) +
    labs(fill = label) +
    theme(
      legend.position = c(0.99, 0.21),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7),
      legend.key.size = unit(0.5, "cm")
    )
  
  return(p1)
}

lisa_coefs_maps <- map2(lisa_coefs, gwr_ind_var, make_lisa_maps)
lisa_sp_maps <- make_lisa_maps(lisa_sp, "SP")
lisa_lm_sp_maps <- make_lisa_maps(lisa_lm_sp, "\nLocal mean SP")
lisa_r2_maps <- make_lisa_maps(lisa_r2, "Local R²")
lisa_lsd_sp_maps <- make_lisa_maps(lisa_sd, "Local SD")

# Saving LISA maps --------------------------------------------------------

all_lisa_maps <- list(
  lisa_coefs_maps, lisa_sp_maps, lisa_lm_sp_maps, lisa_r2_maps
)

all_lisa_names <- c(gwr_ind_var, "SP", "LM_SP", "local_R2")

save_lisa_maps <- function(maps, names) {
  names <- glue("{output05}lisa_{names}.png")
  ggsave(
    filename = names, 
    plot = maps, 
    width = 3, 
    height = 3.5, 
    device = "png",
    dpi = 300
  )
}

map2(lisa_coefs_maps, gwr_ind_var, save_lisa_maps)
save_lisa_maps(lisa_sp_maps, "SP")
save_lisa_maps(lisa_lm_sp_maps, "LM_SP")
save_lisa_maps(lisa_r2_maps, "local_R2")
save_lisa_maps(lisa_lsd_sp_maps, "LSD_SP")

# Creating clusters data --------------------------------------------------

# make_cluster_data <- function(lisa_taz) {
#   lisa_taz %>% 
#     st_make_valid() %>% 
#     group_by(cluster) %>% 
#     summarise() %>% 
#     filter(cluster != "Not significant")
# }
# 
# lisa_coefs_cluster <- map(lisa_coefs, make_cluster_data)
# names(lisa_coefs_cluster) <- gwr_ind_var
# 
# lisa_misc_cluster <- map(list(lisa_sp, lisa_lm_sp, lisa_r2), make_cluster_data)
# 
# lisa_sp_cluster <- lisa_sp %>% 
#   st_make_valid() %>% 
#   group_by(cluster) %>% 
#   summarise(geom = st_union(geom)) %>% 
#   filter(cluster != "Not significant")
# 
# lisa_lm_r2_cluster <- map(list(lisa_lm_sp, lisa_r2), make_cluster_data)
# names(lisa_lm_r2_cluster) <- c("LM_SP", "local_R2")
