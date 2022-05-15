fix_road_ctb <- function() {
  road_ctb <- nds_download_sf(
    "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/EIXO_RUA_SIRGAS.zip"
  )
  
  road_ctb %>% 
    filter(HIERARQUIA %in% c("1", "2", "3", "4")) %>% 
    mutate(
      HIERARQUIA = case_when(
        HIERARQUIA == "1" ~ "Rapid transit",
        HIERARQUIA == "2" ~ "Arterial", 
        HIERARQUIA == "3" ~ "Collector",
        HIERARQUIA == "4" ~ "Local",
        TRUE ~ NA_character_),
      HIERARQUIA = factor(
        HIERARQUIA, levels = c(
          "Rapid transit", "Arterial", "Collector", "Local"
        )
      )
    )
}

plot_road_ctb <- function(road_axis) {
  ggplot(road_axis) +
    geom_sf(data = cwb, fill = "white") +
    geom_sf(
      data = road_axis, aes(
        color = HIERARQUIA, size = HIERARQUIA, alpha = HIERARQUIA
      )
    ) +
    theme_void() +
    scale_color_manual(values = c("red", "darkgreen", "orange", "grey")) +
    scale_alpha_manual(values = c(1, 0.9, 0.9, 0.6)) +
    scale_size_manual(values = c(0.4, 0.3, 0.2, 0.1)) +
    labs(
      color = "Road category:",
      alpha = "Road category:",
      size = "Road category:"
    ) +
    theme(
      legend.position = c(0.90, 0.13), 
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm")
    )
}

fix_road_cwb <- function() {
  road <- st_read("data/EIXO_RUA_UTF.shp")
  road %>% 
    mutate(
      SVIARIO = case_when(
        str_sub(SVIARIO, 1, 7) == "RODOVIA" ~ "Highway",
        str_sub(SVIARIO, 1, 8) == "COLETORA" ~ "Collector",
        SVIARIO == "EXTERNA" ~ "Structural",
        str_sub(SVIARIO, 1, 8) == "SETORIAL" ~ "Sectorial",
        SVIARIO == "OUTRAS VIAS" ~ "Other",
        SVIARIO == "CENTRAL" ~ "Structural", 
        SVIARIO == "NORMAL" ~ "Other",
        SVIARIO == "PRIORIT�RIA" ~ "Prioritized",
        TRUE ~ NA_character_
      ),
      SVIARIO = factor(
        SVIARIO, levels = c(
          "Highway", "Structural", "Sectorial", 
          "Collector", "Prioritized", "Other"
        )
      )
    )
}

plot_road_cwb <- function(road_axis) {
  ggplot() +
    geom_sf(data = cwb, color = "grey40", fill = "white", lwd = 0.4) +
    geom_sf(data = road_axis, aes(
      color = SVIARIO, alpha = SVIARIO, size = SVIARIO
    )) +
    theme_void() +
    scale_color_manual(values = c(
      "#E41A1E", "#FF7F1B", "#4DB152", "#387CB6", "#9849A0", "grey"
    )) +
    scale_alpha_manual(values = c(1, 0.9, 0.8, 0.8, 0.8, 0.6)) +
    scale_size_manual(values = c(0.4, 0.3, 0.2, 0.2, 0.2, 0.1)) +
    labs(
      color = "Road category:",
      alpha = "Road category:",
      size = "Road category:"
    ) +
    theme(
      legend.position = c(0.95, 0.15), 
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm")
    )
}

create_sp_cluster <- function(lisa_shape) {
  lisa_shape %>% 
    group_by(cluster) %>% 
    summarise() %>% 
    filter(cluster %in% c("High-High", "Low-Low")) %>% 
    mutate(cluster = as.character(cluster))
}

plot_area_calma <- function(cluster) {
  area_calma <- st_read("data/areacalma.shp") %>% 
    st_transform(crs = 4674)
  
  bbox_ac <- st_bbox(cluster)
  #bbox_ac["ymin"] <- bbox_ac["ymin"] - 3000
  bbox_ac["xmin"] <- bbox_ac["xmin"] - 2000
  
  tm_shape(cwb, bbox = bbox_ac) +
    tm_fill(col = "grey80") +
    tm_shape(taz_gwr) +
    tm_polygons(col = "white", border.col = "grey70") +
    tm_shape(cluster) +
    tm_fill(
      col = "cluster", 
      alpha = 0.3,
      palette = c("#ff0004", "#0700fa"),
      title = "SP clusters:"
    ) +
    tm_borders(col = "grey60") +
    tm_shape(area_calma) +
    tm_borders(col = "#1b9e77", lwd = 1.5) +
    tm_fill(
      col = "id", 
      palette = "#1b9e77", 
      alpha = 0.6, 
      title = "---",
      labels = "Área Calma"
    ) + 
    tm_scale_bar(position = c("left", "top"), width = 0.14) +
    tm_layout(legend.position = c(0.03, 0.12))
  
}

plot_land_zoning <- function() {
  land_zoning <- nds_download_sf(
    "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/ZONEAMENTO_15511_2019_SIRGAS.zip"
  )
  
  ambiental <- "Environmental Protection Area"
  
  land_zoning <- land_zoning %>% 
    mutate(
      NM_GRUPO = case_when(
        NM_GRUPO == "ÁREA DE PROTEÇÃO AMBIENTAL DO IGUAÇU" ~ ambiental,
        NM_GRUPO == "ÁREA DE PROTEÇÃO AMBIENTAL DO PASSAÚNA" ~ ambiental,
        NM_GRUPO == "EIXO ESTRUTURANTES" ~ "Structural Axes",
        NM_GRUPO == "EIXO METROPOLITANO - LINHA VERDE" ~ "Metropolitan Axis",
        NM_GRUPO == "EIXOS CONECTORES" ~ "Connector Axes",
        NM_GRUPO == "EIXOS DE ADENSAMENTO" ~ "Densification Axes",
        NM_GRUPO == "SETORES ESPECIAIS" ~ "Special Sectors",
        NM_GRUPO == "UNIDADE DE CONSERVAÇÃO" ~ "Preservation Areas",
        NM_GRUPO == "ZONA CENTRAL" ~ "Central Zoning",
        NM_GRUPO == "ZONAS COM DESTINAÇÃO ESPECÍFICA" ~ "Special Purpose Zoning",
        NM_GRUPO == "ZONAS DE USO MISTO" ~ "Mixed Use Zoning",
        TRUE ~ "Residential Zoning"
      )
    ) %>% 
    group_by(NM_GRUPO) %>% 
    summarise()
  
  zoning_axes <- land_zoning %>% 
    filter(NM_GRUPO %in% c(
      "Structural Axes", "Connector Axes", "Densification Axes", 
      "Metropolitan Axis"
    )) %>% 
    mutate(NM_GRUPO = factor(NM_GRUPO, levels = c(
      "Metropolitan Axis", "Structural Axes", "Densification Axes", 
      "Connector Axes"
    )))
  
  zoning_zones <- land_zoning %>% 
    filter(NM_GRUPO %in% c(
      "Residential Zoning", "Central Zoning", "Special Purpose Zoning", 
      "Mixed Use Zoning"
    ))
  
  zoning_other <- land_zoning %>% 
    filter(NM_GRUPO %in% c(
      "Environmental Protection Area", "Special Sectors", "Preservation Areas"
    ))
  
  map_axes <- tm_shape(zoning_axes, bbox = st_as_sfc(st_bbox(zoning_other))) +
    tm_fill(
      col = "NM_GRUPO",
      title = "Axes:",
      palette = c("#E35B70", "#FB8072", "#FDA065", "#E37C5B")
    ) +
    tm_borders(col = "grey30", lwd = 0.5)
  
  map_zones <- tm_shape(zoning_zones) +
    tm_fill(
      col = "NM_GRUPO",
      title = "Zones:",
      palette = c("#7E83E0", "#83A4EB", "#80B1D3", "#83DBEB")
    ) +
    tm_borders(col = "grey30", lwd = 0.5)
  
  
  map_other <- tm_shape(zoning_other) +
    tm_fill(
      col = "NM_GRUPO",
      title = "Other Areas:",
      palette = c("#B3DE69", "#EBDE63", "#F3F567")
    ) +
    tm_borders(col = "grey30", lwd = 0.5)
  
  zoning_map <- map_axes +
    map_zones +
    map_other +
    tm_layout(frame = FALSE, legend.width = 1) + 
    tm_legend(
      legend.position = c(0.97, 0.02),
      legend.title.size = 0.8,
      legend.text.size = 0.6
    )
  
  return(zoning_map)
}

plot_address <- function() {
  address <- readxl::read_excel("data/drivers_address.xlsx") %>% 
    st_as_sf(coords = c("LONG", "LAT")) %>% 
    st_set_crs(4674)
  
  bb <- st_bbox(address)
  bb["ymax"] <- -25.34929
  
  pr_mun <- geobr::read_municipality(code_muni = "PR")
  
  rmc_roads <- osmdata::opq(bbox = bb) %>% 
    osmdata::add_osm_feature(key = "highway") %>% 
    osmdata::osmdata_sf()
  
  address_map <- tm_shape(rmc_roads$osm_lines, bbox = bb) +
    tm_lines(col = "grey70", lwd = 0.2, alpha = 0.8) +
    tm_shape(address) +
    tm_dots(col = "red", size = 0.07, shape = 1) +
    tm_shape(pr_mun) +
    tm_borders(col = "grey30", lty = "dotted", lwd = 0.7)
  
  return(address_map)
}

plot_roads_cluster <- function(road_axis, cluster) {
  ggplot() +
    geom_sf(data = cwb, fill = "white") +
    geom_sf(
      data = road_axis, aes(
        color = HIERARQUIA, size = HIERARQUIA, alpha = HIERARQUIA
      )
    ) +
    theme_void() +
    scale_color_manual(values = c("red", "darkgreen", "orange", "grey")) +
    scale_alpha_manual(values = c(1, 0.9, 0.9, 0.6)) +
    scale_size_manual(values = c(0.4, 0.3, 0.2, 0.1)) +
    labs(
      color = "Road category:",
      alpha = "Road category:",
      size = "Road category:",
      fill = "SP clusters:"
    ) +
    theme(
      legend.position = c(1.00, 0.2), 
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm")
    ) +
    geom_sf(
      data = cluster,
      lwd = 0.1,
      alpha = 0.3,
      color = NA,
      aes(fill = cluster)
    ) +
    scale_fill_manual(values = c("#ff0004", "#0700fa"))
}

plot_travel_demand <- function(cluster) {
  travel_demand <- read_csv2("data/deslocamento.csv") %>% 
    janitor::clean_names() %>% 
    mutate(
      zona_destino = as.character(zona_destino),
      id_taz = str_sub(zona_destino, 1, -3)
    ) %>%
    group_by(id_taz) %>%
    summarise(trips = n())
  
  taz_trips <- taz %>%
    left_join(travel_demand, by = "id_taz") %>% 
    select(id_taz, trips)
  
  od_plot <- ggplot() +
    geom_sf(data = taz_trips, aes(fill = trips), color = NA) +
    geom_sf(
      data = cluster, 
      aes(color = cluster), 
      fill = NA,
      lty = "solid",
      lwd = 0.5
    ) +
    theme_void() +
    labs(fill = "Trips:", color = "SP clusters:") +
    theme(
      legend.position = c(1.20, 0.32),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.5, "cm"),
    ) +
    #scale_color_manual(values = c("#ff0004", "#0700fa")) +
    scale_fill_distiller(palette = "Greys", direction = -1)
  
  return(od_plot)
}

plot_spd_cameras <- function() {
  spd_cameras <- st_read("data/speed_traps.shp")
  
  cameras_buffer <- spd_cameras %>%
    st_transform(crs = 31982) %>% 
    st_buffer(dist = 200) %>% 
    st_union()
  
  cam_buffer_plot <- ggplot() +
    geom_sf(data = cwb, fill = "white") +
    geom_sf(
      data = road_ctb, aes(
        color = HIERARQUIA, size = HIERARQUIA, alpha = HIERARQUIA
      )
    ) +
    geom_sf(
      data = cameras_buffer, 
      aes(fill = "Speed cameras\nbuffer (200m)"),
      color = NA,
      alpha = 0.5
    ) +
    theme_void() +
    scale_color_manual(values = c("red", "darkgreen", "orange", "grey")) +
    scale_alpha_manual(values = c(1, 0.9, 0.9, 0.6)) +
    scale_size_manual(values = c(0.4, 0.3, 0.2, 0.1)) +
    scale_fill_manual(values = "midnightblue") +
    labs(
      color = "Road category:",
      alpha = "Road category:",
      size = "Road category:",
      fill = ""
    ) +
    theme(
      legend.position = c(1.00, 0.2), 
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm")
    )
  
  return(cam_buffer_plot)
}

extract_w <- function(index, results) {
  w_value <- results[[index]][["statistic"]][["W"]]
}

extract_pvalue <- function(index, results) {
  p_value <- results[[index]][["p.value"]]
}

extract_sp_groups <- function(data) {
  data %>% 
    filter(cluster %in% c("Low-Low", "High-High"))
}

calc_wilcox <- function(group, vars, name) {
  
  wilcox_results <- map(
    vars, 
    ~wilcox.test(formula(paste0(.x, "~", name)), data = group)
  )
  
  w_values <- map(
    seq(1,length(vars),1),
    ~extract_w(.x, wilcox_results)
  ) %>% unlist()
  p_values <- map(
    seq(1,length(vars),1),
    ~extract_pvalue(.x, wilcox_results)
  ) %>% unlist()
  
  wilcox_table <- tibble(
    variables = vars,
    W = w_values,
    p_value = p_values
  )
  
  return(wilcox_table)
}

plot_wilcox_hist <- function() {
  
  selected_vars <- wilcox_table %>% 
    filter(p_value < 0.05) %>% 
    pull(variables)
  
  lisa_sp %>%
    st_drop_geometry() %>% 
    filter(cluster %in% c("Low-Low", "High-High")) %>% 
    select(all_of(selected_vars), cluster) %>% 
    pivot_longer(-cluster, names_to = "var", values_to = "values") %>%
    mutate(
      var = case_when(
        var == "AVI" ~ "AVI [BRL]",
        var == "DCSU" ~ "DCSU [no./km²]",
        var == "DIS" ~ "DIS [no./km]",
        var == "DSC" ~ "DSC [no./km]",
        TRUE ~ "TSD [no./km]"
      ),
      cluster = if_else(
        cluster == "Low-Low", 
        paste0(
          "Low-Low (n=",
          lisa_sp$cluster[lisa_sp$cluster == "Low-Low"] %>% length(),
          ")"
        ),
        paste0(
          "High-High (n=",
          lisa_sp$cluster[lisa_sp$cluster == "High-High"] %>% length(),
          ")"
        )
      )
    ) %>% 
    ggplot(aes(x = cluster, y = values, fill = cluster)) +
    geom_boxplot(lwd = 0.2, outlier.size = 0.1) +
    facet_wrap(~var, scales = "free_y") +
    theme_bw(base_size = 8) +
    theme(legend.position = "none") +
    labs(y = "Values", x = "Local Moran Cluster: SP")
}

extract_par_groups <- function(data) {
  data %>% 
    st_drop_geometry() %>%
    select(AVI:DSC) %>% 
    mutate(group = case_when(
      PAR >= quantile(PAR, 0.75) ~ "high",
      PAR <= quantile(PAR, 0.25) ~ "low",
      TRUE ~ "no_group"
    )) %>% 
    select(-PAR) %>% 
    filter(group != "no_group")
}

plot_par_hist <- function() {
  selected_vars <- wilcox_table_par %>% 
    filter(p_value < 0.05) %>% 
    pull(variables)
  
  taz_par %>% 
    select(all_of(selected_vars), group) %>% 
    pivot_longer(-group, names_to = "var", values_to = "values") %>%
    mutate(
      var = case_when(
        var == "PD" ~ "PD [inhab/km²]",
        var == "SND" ~ "SND [km/km²]",
        var == "BSD" ~ "BSD [no./km]",
        var == "DIS" ~ "DIS [no./km]",
        var == "TSD" ~ "TSD [no./km]",
        TRUE ~ "LDI"
      ),
      group = if_else(
        group == "low", 
        paste0(
          "low (n=",
          taz_par$group[taz_par$group == "low"] %>% length(),
          ")"
        ),
        paste0(
          "high (n=",
          taz_par$group[taz_par$group == "high"] %>% length(),
          ")"
        )
      )
    ) %>% 
    ggplot(aes(x = group, y = values, fill = group)) +
    geom_boxplot(lwd = 0.2, outlier.size = 0.1) +
    facet_wrap(~var, scales = "free_y") +
    theme_bw(base_size = 8) +
    theme(legend.position = "none") +
    labs(y = "Values", x = "PAR group")
}
  