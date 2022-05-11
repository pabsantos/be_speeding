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
