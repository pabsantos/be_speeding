arrange_taz <- function(taz_data) {
  taz_data %>% 
    filter(DIST_EXP > 0, (DIST_TOTAL / road_length) > 0.1) %>% 
    rename(SP = SPEEDING)
}

plot_taz_removal <- function(taz_gwr_data) {
  ggplot() +
    geom_sf(data = taz, lwd = 0.1, color = "grey70") +
    geom_sf(data = taz_gwr_data, lwd = 0.1, fill = "orange") +
    theme_void()
}

plot_spear <- function(taz_gwr_data) {
  taz_gwr_data %>% 
    select(
    -neigh, -id_taz, -area, -road_length, 
    -DIST_TOTAL, -DIST_EXP, -DIST_SPD, -SP
    ) %>%
    st_drop_geometry() %>%
    ggcorr(
      method = c("everything", "spearman"), 
      label = TRUE,
      label_size = 2.5, 
      label_round = 2, 
      name = "Spearman Coefficient:",
      size = 2.5, 
      legend.size = 7
    )
}

calc_sample_size <- function(taz_gwr_data) {
  taz_gwr_data %>% 
    select(DIST_TOTAL, DIST_EXP, DIST_SPD) %>% 
    st_drop_geometry() %>% 
    summarise_all(sum) %>% 
    as_tibble()
}

calc_global_summary <- function(taz_gwr_data) {
  taz_gwr_data %>% 
    select(
      -neigh, -id_taz, -area, -road_length, -DIST_TOTAL, -DIST_EXP, -DIST_SPD
    ) %>% 
    st_drop_geometry() %>% 
    pivot_longer(AVI:SP, names_to = "VAR", values_to = "VALUE") %>% 
    group_by(VAR) %>% 
    summarise(
      mean = mean(VALUE),
      sd = sd(VALUE),
      min = min(VALUE),
      `1q` = quantile(VALUE, 0.25),
      median = median(VALUE),
      `3q` = quantile(VALUE, 0.75),
      max = max(VALUE)
    )
}

calc_gwr <- function(taz, var) {
  
  taz <- as(taz, "Spatial")
  
  # Sort independent variables for best fit
  ind_variables <- var
  sort_var <- gwr.model.selection(
    DeVar = "SP",
    InDeVars = ind_variables,
    data = taz,
    bw = 30, # Generic bandwidth
    approach = "AIC",
    kernel = "gaussian",
    adaptive = TRUE # Number of neighbors
  )
  formula <- as.formula(sort_var[[1]][[length(sort_var[[1]])]][[1]])
  
  # Extracting bw size for each kernel type 
  kernel_type <- c("gaussian", "bisquare", "tricube", "boxcar", "exponential")
  bw_sizes <- vector(mode = "integer", length = 5)
  calc_bw_sizes <- function(kernel) {
    bw.gwr(
      formula = formula, 
      data = taz, 
      approach = "AIC", 
      kernel = kernel,
      adaptive = TRUE
    )
  }
  bw_sizes <- map_dbl(kernel_type, calc_bw_sizes)
  names(bw_sizes) <- kernel_type
  
  # Running a GWR for each kernel type
  calc <- function(bw, kernel) {
    gwr.basic(
      formula = formula, 
      data = taz, 
      bw = bw, 
      kernel = kernel, 
      adaptive = TRUE
    )
  }
  gwr_results <- vector(mode = "list", length = 5)
  gwr_results <- map2(bw_sizes, kernel_type, calc)
  return(gwr_results)
}

extract_gwr_diag <- function(gwr_model) {
  df <- tibble(
    test = c("RSS.gw", "AIC", "AICc", "enp", "edf", "gw.R2", 
             "gwR2.adj", "BIC", "bandwidth"),
    gaussian = c(unlist(gwr_model[[1]][["GW.diagnostic"]]), 
                 gwr_model[["gaussian"]][["GW.arguments"]][["bw"]]), 
    bisquare = c(unlist(gwr_model[[2]][["GW.diagnostic"]]),
                 gwr_model[["bisquare"]][["GW.arguments"]][["bw"]]), 
    tricube = c(unlist(gwr_model[[3]][["GW.diagnostic"]]),
                gwr_model[["tricube"]][["GW.arguments"]][["bw"]]), 
    boxcar = c(unlist(gwr_model[[4]][["GW.diagnostic"]]),
               gwr_model[["boxcar"]][["GW.arguments"]][["bw"]]),
    exponential = c(unlist(gwr_model[[5]][["GW.diagnostic"]]),
                    gwr_model[["exponential"]][["GW.arguments"]][["bw"]])
  )
  
  df %>% 
    pivot_longer(-test, names_to = "kernel", values_to = "value") %>% 
    pivot_wider(names_from = test, values_from = value)
}

calc_moran <- function(gwr_results) {
  # Extracting neighbors
  nb <- poly2nb(gwr_results[[1]][["SDF"]], queen = TRUE)
  
  # Setting weights for each neighbor
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Moran's I with Monte Carlo Simulation for gwr
  gwr_mmc <- function(gwr_model_data){
    moran.mc(
      gwr_model_data[["SDF"]]$residual, lw, nsim = 999, alternative = "greater"
    )
  }
  
  gwr_mmc_results <- map(gwr_results, gwr_mmc)
  
  global_mmc <- moran.mc(
    gwr_results[[1]][["lm"]]$residuals, lw, nsim = 999, alternative = "greater"
  )
  
  # Results table
  mmc_results <- tibble(
    model = c(
      "gaussian", "bisquare", "tricube", "boxcar", "exponential", "global"
    ),
    i = c(
      gwr_mmc_results[[1]][["statistic"]], 
      gwr_mmc_results[[2]][["statistic"]],
      gwr_mmc_results[[3]][["statistic"]],
      gwr_mmc_results[[4]][["statistic"]],
      gwr_mmc_results[[5]][["statistic"]], 
      global_mmc[["statistic"]]
    ),
    p_value = c(
      gwr_mmc_results[[1]][["p.value"]], 
      gwr_mmc_results[[2]][["p.value"]],
      gwr_mmc_results[[3]][["p.value"]], 
      gwr_mmc_results[[4]][["p.value"]],
      gwr_mmc_results[[5]][["p.value"]], 
      global_mmc[["p.value"]]
    )
  )
  
  return(mmc_results)
}

extract_gwr_summary <- function(taz, results) {
  gwss(
    taz,
    vars = "SP",
    bw = results[["GW.arguments"]][["bw"]],
    kernel = results[["GW.arguments"]][["kernel"]],
    adaptive = TRUE,
    quantile = TRUE
  )
}

plot_gwr_summary <- function() {
  plot_summary <- function(vars, legend) {
    tm_shape(taz) + 
      tm_fill(col = "grey") + 
      tm_borders(col = "black", lwd = 0.1) +
      tm_shape(gwr_summary[["SDF"]]) + 
      tm_fill(col = vars, n = 6, style = "quantile", title = legend) + 
      tm_borders(col = "black", lwd = 0.2) + 
      tm_layout(frame = FALSE) + 
      tm_legend(
        legend.position = c(0.82,0.00),
        legend.title.size = 0.8, 
        legend.text.size = 0.6
      )
  }
  summary_vars <- c("SP_LM", "SP_LSD")
  summary_legend <- c("Local mean", "Local Std. deviation")
  
  gwss_maps <- map2(summary_vars, summary_legend, plot_summary)
  return(gwss_maps)
}

plot_gwr_results <- function(var) {
  plot <- function(var) {
    tm_shape(taz) + 
      tm_fill(col = "grey") + 
      tm_borders(col = "black", lwd = 0.1) +
      tm_shape(gwr_chosen_model[["SDF"]]) +
      tm_fill(
        col = var, n = 6, style = "quantile", palette = "-BrBG", midpoint = 0
      ) +
      tm_borders(col = "black", lwd = 0.2) +
      tm_layout(frame = FALSE, legend.width = 0.5) + 
      tm_legend(
        legend.position = c(0.75,0.00),
        legend.title.size = 0.8, 
        legend.text.size = 0.6
      )
  }
  
  gwr_maps <- map(var, plot)
  return(gwr_maps)
}

plot_local_r2 <- function(gwr_chosen_model) {
  tm_shape(taz) + 
    tm_fill(col = "grey") + 
    tm_borders(col = "black", lwd = 0.1) +
    tm_shape(gwr_chosen_model[["SDF"]]) +
    tm_fill(
      col = "Local_R2", 
      n = 8, 
      style = "quantile",
      palette = "RdBu", 
      midpoint = 0,
      title = "Local R²",
      colorNA = "#b3b3b3"
    ) +
    tm_borders(col = "black", lwd = 0.2) +
    tm_layout(frame = FALSE, legend.width = 0.6) + 
    tm_legend(
      legend.position = c(0.82,0.00),
      legend.title.size = 0.8, 
      legend.text.size = 0.6
    )
}

count_taz_coef <- function(results) {
  results %>% 
    mutate(across(everything(), ~ case_when(
      . >= 0 ~ "pos",
      . < 0 ~ "neg",
      TRUE ~ NA_character_))) %>% 
    pivot_longer(
      cols = Intercept:DIS, names_to = "variables", values_to = "count"
    ) %>% 
    mutate(n = 1) %>% 
    group_by(variables, count) %>% 
    summarise(n = sum(n)) %>% 
    pivot_wider(names_from = count, values_from = n) %>% 
    replace(is.na(.), 0) %>% 
    mutate(
      prop_neg = neg / (neg + pos),
      prop_pos = pos / (neg + pos)
    ) %>% 
    arrange(-prop_neg) %>% 
    mutate(
      prop_pos = scales::percent(prop_pos),
      prop_neg = scales::percent(prop_neg)
    )
}

plot_sp <- function(taz_gwr_data) {
  ggplot() + 
    geom_sf(data = taz, fill = "grey70", color = "grey50", lwd = 0.1) +
    geom_sf(data = taz_gwr_data, aes(fill = SP), color = NA) +
    theme_void() +
    labs(fill = "SP") +
    scale_fill_distiller(palette = "Blues", direction = -1) +
    theme(
      legend.position = c(0.93, 0.21),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.5, "cm")
    )
}

calc_moran_sp <- function() {
  nb <- poly2nb(gwr_chosen_model[["SDF"]], queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  moran.mc(gwr_chosen_model[["SDF"]]$y, lw, nsim = 999, alternative = "greater")
}
