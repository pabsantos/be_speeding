load_taz <- function() {
  taz <- st_read("data/taz.gpkg") %>% 
    rename(id_taz = cod_taz, neigh = BAIRRO) %>% 
    mutate(id_taz = as.character(id_taz))
}

load_census_data <- function() {
  census_tract <- geobr::read_census_tract(code_tract = 4106902, year = 2010) %>% 
    mutate(code_tract = as.character(code_tract)) %>% 
    select(code_tract)
  
  census_data <- read_csv("data/Basico_PR.csv") %>% 
    filter(`Nome_do_municipio` == "CURITIBA") %>% 
    rename(
      code_tract = Cod_setor,
      population = V002,
      income = V005
    ) %>% 
    mutate(code_tract = as.character(code_tract)) %>% 
    select(code_tract, population, income)
  
  census_tract <- census_tract %>% 
    left_join(census_data, by = "code_tract")
}

add_census_data <- function(taz_data, census_data) {
  taz_pop_income <- census_data %>% 
    st_transform(crs = 31982) %>% 
    mutate(
      area = st_area(geom),
      income_area = income * area
    ) %>% 
    st_centroid() %>% 
    st_join(taz_data["id_taz"]) %>% 
    group_by(id_taz) %>% 
    summarise(
      population = sum(population, na.rm = T),
      income_area = sum(income_area, na.rm = T),
      area = sum(area)
    ) %>% 
    mutate(income = income_area / area) %>% 
    st_drop_geometry()
  
  taz_data %>% 
    left_join(taz_pop_income, by = "id_taz") %>% 
    mutate(
      area = units::set_units(st_area(geom), km^2),
      PD = population / area
    ) %>% 
    rename(AVI = income) %>% 
    select(-income_area, -population)
}

add_snd <- function(taz_data) {
  url <- "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/EIXO_RUA_SIRGAS.zip"
  ippuc_axis <- nds_download_sf(url)
  
  taz_road_length <- ippuc_axis %>% 
    st_join(taz_data["id_taz"]) %>% 
    mutate(length = units::set_units(st_length(geometry), km)) %>% 
    group_by(id_taz) %>% 
    summarise(road_length = sum(length)) %>% 
    st_drop_geometry()
  
  taz_data %>% 
    left_join(taz_road_length, by = "id_taz") %>% 
    mutate(SND = road_length / area)
}

add_par <- function(taz_data) {
  url <- "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/EIXO_RUA_SIRGAS.zip"
  ippuc_axis <- nds_download_sf(url)
  
  taz_par <- ippuc_axis %>% 
    st_join(taz_data["id_taz"]) %>% 
    filter(HIERARQUIA != "0") %>% 
    mutate(length = units::set_units(st_length(geometry), km)) %>% 
    group_by(id_taz, HIERARQUIA) %>% 
    summarise(road_length = sum(length)) %>%
    drop_na() %>% 
    select(-geometry) %>% 
    pivot_wider(names_from = "HIERARQUIA", values_from = "road_length") %>% 
    mutate(
      MAJOR_LENGTH = sum(`1`, `2`, na.rm = TRUE),
      TOTAL_LENGTH = sum(`1`, `2`, `3`, `4`, na.rm = TRUE),
      PAR = MAJOR_LENGTH / TOTAL_LENGTH
    ) %>% 
    select(id_taz, PAR)
  
  taz_data %>% 
    left_join(taz_par, by = "id_taz")
}

add_dis <- function(taz_data) {
  intersection <- st_read("data/intersection.shp")
  
  taz_di <- intersection %>% 
    st_transform(crs = 31982) %>% 
    st_join(taz_data["id_taz"]) %>% 
    group_by(id_taz) %>% 
    summarise(INTERSECTIONS = n()) %>% 
    st_drop_geometry()
  
  taz_data %>% 
    left_join(taz_di, by = "id_taz") %>% 
    mutate(DIS = INTERSECTIONS / road_length) %>% 
    select(-INTERSECTIONS)
}

add_tsd <- function(taz_data) {
  traffic_signal <- st_read("data/traffic_lights.shp")
  
  taz_ts <- traffic_signal %>% 
    st_transform(crs = 31982) %>% 
    st_join(taz_data["id_taz"]) %>% 
    group_by(id_taz) %>% 
    summarise(SIGNALS = n()) %>% 
    st_drop_geometry()
  
  taz_data %>% 
    left_join(taz_ts, by = "id_taz") %>% 
    mutate(TSD = SIGNALS / road_length) %>% 
    select(-SIGNALS)
}

add_dcsu <- function(taz_data) {
  alvaras <- st_read("data/alvara_comercial_ATIVO.shp")
  
  taz_units <- alvaras %>% 
    mutate(setor = str_sub(grupoativi,1,1)) %>% 
    filter(
      !is.na(cnpj),
      situacao == "ATIVO",
      setor %in% c(
        "G", "H", "I", "J", "K", "L", "M", "O", "P", "Q", "R", "S", "T", "U"
      )
    ) %>% 
    st_transform(crs = 31982) %>% 
    st_join(taz_data["id_taz"]) %>% 
    group_by(id_taz) %>% 
    summarise(UNITS = n()) %>% 
    st_drop_geometry()
  
  taz_data %>% 
    left_join(taz_units, by = "id_taz") %>% 
    mutate(DCSU = UNITS / area) %>% 
    select(-UNITS)
}

add_ldi <- function(taz_data) {
  url <- "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/ZONEAMENTO_15511_2019_SIRGAS.zip"
  
  land_zoning <- nds_download_sf(url)
  
  land_zoning_taz <- land_zoning %>% 
    st_intersection(taz_data["id_taz"])
  
  zoning_i <- land_zoning_taz %>% 
    group_by(id_taz) %>% 
    summarise(i = n_distinct(SG_ZONA)) %>% 
    st_drop_geometry()
  
  zoning_area <- land_zoning_taz %>% 
    mutate(area = st_area(geometry)) %>% 
    group_by(id_taz, SG_ZONA) %>% 
    summarise(area = sum(area)) %>% 
    st_drop_geometry()
  
  zoning_total <- land_zoning_taz %>% 
    mutate(area = st_area(geometry)) %>% 
    group_by(id_taz) %>% 
    summarise(area_total = sum(area)) %>% 
    st_drop_geometry()
  
  taz_ldi <- zoning_area %>% 
    left_join(zoning_i, by = "id_taz") %>% 
    left_join(zoning_total, by = "id_taz") %>% 
    mutate(
      P = area / area_total, 
      log_P = log(P), 
      log_i = log(i), 
      mul_P = P * log_P
    ) %>% 
    group_by(id_taz) %>% 
    summarise(sum_P = -1 * sum(mul_P), log_i = log_i) %>% 
    group_by(id_taz) %>% 
    summarise(LDI = sum_P / log_i) %>% 
    distinct() %>% 
    mutate_all(~replace(., is.nan(.), 0))
  
  taz_data %>% 
    left_join(taz_ldi, by = "id_taz")
}

add_bsd <- function(taz_data) {
  bus_stops <- st_read("data/PONTO_DE_ONIBUS.shp")
  
  taz_bsd <- bus_stops %>% 
    filter(CATEGORIA != "EXPRESSO" & CATEGORIA != "EXPRESSO LIGEIRÃO") %>% 
    st_join(taz_data["id_taz"]) %>% 
    group_by(id_taz) %>% 
    summarise(STOPS = n()) %>% 
    st_drop_geometry()
  
  taz_data %>% 
    left_join(taz_bsd, by = "id_taz") %>% 
    mutate(BSD = STOPS / road_length) %>% 
    select(-STOPS)
}

add_dsc <- function(taz_data) {
  spd_cameras <- st_read("data/speed_traps.shp")
  
  taz_dsc <- spd_cameras %>% 
    st_transform(crs = 31982) %>% 
    st_join(taz_data["id_taz"]) %>% 
    group_by(id_taz) %>% 
    summarise(CAMERAS = n()) %>% 
    st_drop_geometry()
  
  taz_data %>% 
    left_join(taz_dsc, by = "id_taz") %>% 
    mutate(DSC = CAMERAS / road_length) %>% 
    select(-CAMERAS)
}

add_spd_exp_dist <- function(taz_data) {
  taz_distances <- drivers_valid_lines %>% 
    st_transform(crs = 31982) %>%
    mutate(
      DISTANCE_TYPE = case_when(
        as.numeric(LIMITE_VEL) - SPD_KMH < 10 & 
          as.numeric(LIMITE_VEL) - SPD_KMH >= -5 ~ 
          "EXPOSURE",
        SPD_KMH - as.numeric(LIMITE_VEL) > 5 ~ "SPEEDING",
        TRUE ~ "NORMAL"
      ),
      LENGTH = st_length(wkt_lines)
    ) %>% 
    st_join(taz_data["id_taz"]) %>%
    distinct(ID, TIME_ACUM, .keep_all = TRUE) %>% 
    group_by(id_taz, DISTANCE_TYPE) %>% 
    summarise(LENGTH = sum(LENGTH)) %>% 
    st_drop_geometry() %>% 
    pivot_wider(
      names_from = "DISTANCE_TYPE", values_from = "LENGTH",
      values_fill = units::set_units(0, m)
    ) %>% 
    mutate(
      DIST_TOTAL = units::set_units(EXPOSURE + NORMAL + SPEEDING, km),
      DIST_EXP = units::set_units(EXPOSURE + SPEEDING, km),
      DIST_SPD = units::set_units(SPEEDING, km)
    ) %>% 
    select(-EXPOSURE, -NORMAL, -SPEEDING) %>% 
    mutate(SPEEDING = DIST_SPD / DIST_EXP)
  
  taz_data %>% 
    left_join(taz_distances, by = "id_taz")
}

remove_na_unit <- function(taz_data) {
  taz_data %>% 
    mutate_if(is.numeric, units::drop_units) %>% 
    replace(is.na(.), 0)
}

plot_dist_map <- function(taz_data) {
  taz_data %>% 
    ggplot() + 
    geom_sf(aes(fill = DIST_TOTAL), color = NA) +
    theme_void() +
    labs(fill = "Traveled\ndistances [km]:\n(Valid time)") +
    theme(
      legend.position = c(0.97, 0.24),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.5, "cm")
    )
}

plot_var_maps <- function(taz_data) {
  taz_bbox <- st_bbox(taz_data)
  taz_bbox[[3]] <- taz_bbox[[3]] + 4000
  
  plot_var <- function(var, title, palette) {
    tm_shape(taz, bbox = taz_bbox) + 
      tm_fill(
        col = var, n = 6, 
        style = if_else(var == "DSC", "jenks", "quantile"), 
        palette = palette,
        title = title
      ) +
      tm_borders(col = "black", lwd = 0.2) +
      tm_layout(frame = FALSE, legend.width = 0.5) + 
      tm_legend(
        legend.position = c(0.69,0.00),
        legend.title.size = 0.7, 
        legend.text.size = 0.6
      )
  }
  
  var <- c(
    "PD", "LDI", "DIS", "DSC", "TSD", "PAR", "SND", "DCSU", "BSD", "AVI"
  )
  
  title <- c(
    "Pop. density\n[inhab./km²]:", "Land use\ndiversity index:",
    "Density of intersections\n[no./km]:", 
    "Density of speed\ncameras [no./km]:",
    "Traffic signal density\n[no./km]:",
    "Proportion of\narterial roads:",
    "Street network density\n[km/km²]:",
    "Density of commercial\nand services units\n[no./km²]:",
    "Bus stop density\n[no./km]:",
    "Average income\n[BRL]:"
  )
  
  palette <- c(
    "Blues", "Greens", "Reds", "Greys", "Oranges", "Purples",
    "YlGn", "BuGn", "YlGnBu", "PuBu"
  )
  
  var_maps <- pmap(list(var, title, palette), plot_var)
}

save_var_maps <- function(var_maps) {
  var <- c(
    "PD", "LDI", "DIS", "DSC", "TSD", "PAR", "SND", "DCSU", "BSD", "AVI"
  )
  
  save_maps <- function(var_maps, names) {
    file_name <- paste0("plot/taz_", names, ".png")
    
    tmap_save(
      tm = var_maps,
      filename = file_name,
      height = 3.5,
      width = 3,
      units = "in",
      dpi = 300
    )
  }
  
  map2(var_maps, var, save_maps)
}

calc_shapiro <- function(taz) {
  taz_shapiro <- taz %>% 
    st_drop_geometry()
  
  shapiro_results <- map(taz_shapiro, shapiro.test)
  
  extract_shap_results <- function(shap_results, var) {
    p_value <- shap_results[[var]][["p.value"]]
    statistic <- shap_results[[var]][["statistic"]]
    tibble(
      var = var,
      statistic = statistic, 
      pvalue = format(p_value, scientific = FALSE, digits = 1))
  }
  
  map(names(taz_shapiro), ~extract_shap_results(shapiro_results, .x)) %>% 
    reduce(bind_rows)
}
