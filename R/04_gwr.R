output04 <- glue("{output}/04/")

# Applying data arrangement criteria --------------------------------------

taz_gwr <- taz %>% 
  filter(DIST_EXP > 0, (DIST_TOTAL / ROAD_LENGTH) > 0.1) %>% 
  rename(SP = SPEEDING)
  
# Plotting the removal ---------------------------------------------------

map_filter <- ggplot() +
  geom_sf(data = taz, lwd = 0.1, color = "grey70") +
  geom_sf(data = taz_gwr, lwd = 0.1, fill = "orange") +
  theme_void()

ggsave(filename = glue("{output04}map_filter.png"), plot = map_filter,
       width = 3, height = 3.5, device = "png", dpi = 300)


# Checking co-linearity (Spearman) --------------------------------------

plot_spear <- taz_gwr %>% 
  select(-NEIGH, -ID_TAZ, -AREA, -ROAD_LENGTH, -DIST_TOTAL, -DIST_EXP, 
         -DIST_SPD, -SP) %>% 
  st_drop_geometry() %>% 
  ggcorr(method = c("everything", "spearman"), label = TRUE, 
         label_size = 2.5, label_round = 2, name = "Spearman Coefficient:",
         size = 2.5, legend.size = 7)

ggsave(filename = glue("{output04}plot_spear.pdf"),
       plot = plot_spear,
       device = "pdf",
       width = 4.5,
       height = 3.5,
       dpi = 300)

# Remaining sample size ---------------------------------------------------

taz_sample <- taz_gwr %>% 
  select(DIST_TOTAL, DIST_EXP, DIST_SPD) %>% 
  st_drop_geometry() %>% 
  summarise_all(sum) %>% 
  as_tibble()

write_csv(taz_sample, glue("{output04}final_sample_size.csv"))

# Variable global summary -------------------------------------------------

taz_gs <- taz_gwr %>% 
  select(-NEIGH, -ID_TAZ, -AREA, -ROAD_LENGTH, -DIST_TOTAL, -DIST_EXP, 
         -DIST_SPD) %>% 
  st_drop_geometry() %>% 
  pivot_longer(AVI:SP, names_to = "VAR", values_to = "VALUE") %>% 
  group_by(VAR) %>% 
  summarise(mean = mean(VALUE),
            sd = sd(VALUE),
            min = min(VALUE),
            `1q` = quantile(VALUE, 0.25),
            median = median(VALUE),
            `3q` = quantile(VALUE, 0.75),
            max = max(VALUE))

write_csv(taz_gs, glue("{output04}taz_global_summary.csv"))

# GWR function ------------------------------------------------------------

# Select variables
taz_gwr <- taz_gwr %>% 
  select(-NEIGH, -ID_TAZ, -AREA, -ROAD_LENGTH, -DIST_TOTAL, -DIST_EXP, 
         -DIST_SPD)

# Transform in SpatialDataFrame
taz_gwr <- as(taz_gwr, "Spatial")

# Running GWR
calc_gwr <- function(taz, var) {
  
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
    bw.gwr(formula = formula, 
           data = taz, 
           approach = "AIC", 
           kernel = kernel,
           adaptive = TRUE)
  }
  bw_sizes <- map_dbl(kernel_type, calc_bw_sizes)
  names(bw_sizes) <- kernel_type
  
  # Running a GWR for each kernel type
  calc <- function(bw, kernel) {
    gwr.basic(formula = formula, 
              data = taz, 
              bw = bw, 
              kernel = kernel, 
              adaptive = TRUE)
  }
  gwr_results <- vector(mode = "list", length = 5)
  gwr_results <- map2(bw_sizes, kernel_type, calc)
  return(gwr_results)
}

gwr_ind_var <- names(taz_gwr)[names(taz_gwr)!="SP"]

gwr_model_results <- calc_gwr(taz_gwr, gwr_ind_var)

# Extracting diagnostic for each GWR model --------------------------------

extract_diag <- function(gwr_model) {
  df <- tibble(test = c("RSS.gw", "AIC", "AICc", "enp", "edf", "gw.R2", 
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
                              gwr_model[["exponential"]][["GW.arguments"]][["bw"]]))
  
  df %>% 
    pivot_longer(-test, names_to = "kernel", values_to = "value") %>% 
    pivot_wider(names_from = test, values_from = value)
}

diag_table <- extract_diag(gwr_model_results) %>% as_tibble()

# Moran's I on residuals --------------------------------------------------

calc_moran <- function(gwr_results) {
  # Extracting neighbors
  nb <- poly2nb(gwr_results[[1]][["SDF"]], queen = TRUE)
  
  # Setting weights for each neighbor
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Moran's I with Monte Carlo Simulation for gwr
  gwr_mmc <- function(gwr_model_data){
    moran.mc(gwr_model_data[["SDF"]]$residual, lw, nsim = 999, 
             alternative = "greater")
  }
  
  gwr_mmc_results <- map(gwr_results, gwr_mmc)
  global_mmc <- moran.mc(gwr_results[[1]][["lm"]]$residuals, lw, nsim = 999,
                         alternative = "greater")
  
  # Results table
  mmc_results <- tibble(
    model = c("gaussian", "bisquare", "tricube", 
              "boxcar", "exponential", "global"),
    i = c(gwr_mmc_results[[1]][["statistic"]], 
          gwr_mmc_results[[2]][["statistic"]],
          gwr_mmc_results[[3]][["statistic"]],
          gwr_mmc_results[[4]][["statistic"]],
          gwr_mmc_results[[5]][["statistic"]], 
          global_mmc[["statistic"]]),
    p_value = c(gwr_mmc_results[[1]][["p.value"]], 
                gwr_mmc_results[[2]][["p.value"]],
                gwr_mmc_results[[3]][["p.value"]], 
                gwr_mmc_results[[4]][["p.value"]],
                gwr_mmc_results[[5]][["p.value"]], 
                global_mmc[["p.value"]])
  )
  
  return(mmc_results)
}

mmc_results <- calc_moran(gwr_model_results)

write_csv(mmc_results, glue("{output04}mmc_results.csv"))

# Selecting best model ----------------------------------------------------

# manual process, check diagnostic (code to be implemented ...)
gwr_chosen_model <- gwr_model_results[[4]]

# Plot of SP GW summary ---------------------------------------------------

# Summary
summary_gwr <- gwss(
  taz_gwr,
  vars = "SP",
  bw = gwr_chosen_model[["GW.arguments"]][["bw"]],
  kernel = gwr_chosen_model[["GW.arguments"]][["kernel"]],
  adaptive = TRUE,
  quantile = TRUE
)

# Plot
make_gwss_maps <- function(vars, legend) {
  tm_shape(taz) + 
    tm_fill(col="grey") + 
    tm_borders(col="black", lwd=0.1) +
    tm_shape(summary_gwr[["SDF"]]) + 
    tm_fill(col=vars, n = 6, style="quantile", title = legend) + 
    tm_borders(col="black", lwd=0.2) + 
    tm_layout(frame=FALSE) + 
    tm_legend(legend.position = c(0.82,0.00),
              legend.title.size = 0.8, 
              legend.text.size = 0.6)
}

summary_vars <- c("SP_LM", "SP_LSD")
summary_legend <- c("Local mean", "Local Std. deviation")

gwss_maps <- map2(summary_vars, summary_legend, make_gwss_maps)

# Saving maps

save_gwss_maps <- function(plot, names) {
  tmap_save(tm = plot,
            filename = names,
            height = 3.5,
            width = 3,
            units = "in",
            dpi = 300)
}

gwss_names <- c(glue("{output04}mean_map.png"), glue("{output04}sd_map.png"))
map2(gwss_maps, gwss_names, save_gwss_maps)

# Plotting GWR results ----------------------------------------------------

make_gwr_maps <- function(var) {
  tm_shape(taz) + 
    tm_fill(col="grey") + 
    tm_borders(col="black", lwd=0.1) +
    tm_shape(gwr_chosen_model[["SDF"]]) +
    tm_fill(col = var, n = 6, style = "quantile", palette = "-RdYlGn",
            midpoint = 0) +
    tm_borders(col="black", lwd = 0.2) +
    tm_layout(frame=FALSE, legend.width = 0.5) + 
    tm_legend(legend.position = c(0.75,0.00),
              legend.title.size = 0.8, 
              legend.text.size = 0.6)
}

gwr_maps <- map(gwr_ind_var, make_gwr_maps)

gwr_names <- paste(output04, gwr_ind_var, ".png", sep = "")

map2(gwr_maps, gwr_names, save_gwss_maps)

# Plotting local R2 -------------------------------------------------------

r2_map <- tm_shape(taz) + 
  tm_fill(col="grey") + 
  tm_borders(col="black", lwd=0.1) +
  tm_shape(gwr_chosen_model[["SDF"]]) +
  tm_fill(col = "Local_R2", n = 8, style = "quantile", palette = "RdBu",
          midpoint = 0) +
  tm_borders(col="black", lwd = 0.2) +
  tm_layout(frame=FALSE, legend.width = 0.6) + 
  tm_legend(legend.position = c(0.82,0.00),
            legend.title.size = 0.8, 
            legend.text.size = 0.6)

save_gwss_maps(r2_map, glue("{output04}r2_map.png"))

# Count positive and negative coefficients per TAZ and variable -----------

taz_results_table <- gwr_chosen_model[["SDF"]]@data %>% 
  mutate(across(everything(), ~ case_when(
    . >= 0 ~ "pos",
    . < 0 ~ "neg",
    TRUE ~ NA_character_))) %>% 
  pivot_longer(cols = all_of(gwr_ind_var), names_to = "variables", 
               values_to = "count") %>% 
  mutate(n = 1) %>% 
  group_by(variables, count) %>% 
  summarise(n = sum(n)) %>% 
  pivot_wider(names_from = count, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(prop_neg = neg / (neg+pos),
         prop_pos = pos / (neg+pos)) %>% 
  arrange(-prop_neg) %>% 
  mutate(prop_pos = scales::percent(prop_pos),
         prop_neg = scales::percent(prop_neg))

write_csv(taz_results_table, glue("{output04}taz_results_table.csv"))
