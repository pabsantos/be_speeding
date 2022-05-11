calc_lisa <- function(var, taz) {
  if (!"sf" %in% class(taz)) {
    taz <- st_as_sf(taz)
  }
  taz <- taz %>% drop_na(var)
  queen_w <- queen_weights(taz)
  var <- as.character(var)
  lisa <- local_moran(queen_w, taz %>% select(all_of(var)))
  taz$cluster <- as.factor(lisa$GetClusterIndicators())
  levels(taz$cluster) <- lisa$GetLabels()
  taz$lisa_pvalues <- lisa$GetLocalSignificanceValues()
  return(taz)
}

plot_lisa <- function(lisa_taz, var) {
  label <- paste("Local Moran\nClusters: ", var, sep = "")
  cluster_colors <- c(
    "#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8", "#464646", "#999999"
  )
  ggplot() +
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
}

save_lisa_plots <- function(plot, name) {
  name <- paste0("plot/lisa_", name, ".png")
  ggsave(
    filename = name, 
    plot = plot, 
    width = 3, 
    height = 3.5, 
    device = "png",
    dpi = 300
  )
}
