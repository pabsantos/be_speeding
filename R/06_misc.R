output06 <- glue("{output}/06/")

# Curitiba's statistical grid (pop.) --------------------------------------

grid <- read_statistical_grid(code_grid = 25, year = 2010)

cwb_grid <- grid %>% 
  st_join(cwb["name_muni"]) %>% 
  filter(name_muni == "Curitiba")

rm(grid)

grid_map <- tm_shape(cwb_grid) +
  tm_fill(
    col = "POP", alpha = 1, style = "cont", n = 8, 
    title = "Inhabitants per 500 m²:", palette = "viridis"
  ) +
  tm_layout(frame = FALSE, legend.width = 1) + 
  tm_legend(
    legend.position = c(0.88, 0.02),
    legend.title.size = 0.8, 
    legend.text.size = 0.6
  )

tmap_save(
  tm = grid_map, filename = glue("{output06}grid_map.png"),
  units = "in", height = 3.5, width = 5
)

# Road system (CTB) --------------------------------------------------------

road_cwb <- download_sf(
  "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/EIXO_RUA_SIRGAS.zip"
)

road_cwb <- road_cwb %>% 
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

road_cwb_plot <- ggplot() +
  geom_sf(data = cwb, fill = "white") +
  geom_sf(
    data = road_cwb, aes(
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


ggsave(
  filename = glue("{output06}road_cwb_map.png"), plot = road_cwb_plot,
  width = 6, height = 4.5, dpi = 300, device = "png"
)

# Road system (zoning) ----------------------------------------------------

road_zoning_cwb <- st_read("data/input/EIXO_RUA_UTF.shp")

road_zoning_cwb <- road_zoning_cwb %>% 
  select(SVIARIO) %>% 
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

road_zoning_plot <- road_zoning_cwb %>% 
  ggplot() +
  geom_sf(data = cwb, color = "grey40", fill = "white", lwd = 0.4) +
  geom_sf(aes(color = SVIARIO, alpha = SVIARIO, size = SVIARIO)) +
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

ggsave(
  filename = glue("{output06}road_zoning_map.png"), 
  plot = road_zoning_plot,
  width = 6, 
  height = 4.5, 
  dpi = 300, 
  device = "png"
)

# Zona calma + clusters ------------------------------------------------------

area_calma <- st_read(glue("{input}/areacalma.shp"))
cwb_blocks <- download_sf(
  "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/ARRUAMENTO_QUADRAS_SIRGAS.zip"
)

## Transforming to sirgas
area_calma <- area_calma %>% 
  st_transform(crs = 4674)

lisa_sp_shape <- lisa_sp %>% 
  group_by(cluster) %>% 
  summarise() %>% 
  filter(cluster %in% c("High-High", "Low-Low")) %>% 
  mutate(cluster = as.character(cluster))

bbox_ac <- st_bbox(lisa_sp_shape)
bbox_ac["ymin"] <- bbox_ac["ymin"] - 3000
bbox_ac["xmin"] <- bbox_ac["xmin"] - 1000

sp_area_calma <- tm_shape(cwb, bbox = bbox_ac) +
  tm_fill(col = "grey80") +
  tm_shape(st_as_sf(taz_gwr)) +
  tm_polygons(col = "white", border.col = "grey70") +
  tm_shape(lisa_sp_shape) +
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
  tm_layout(legend.position = c("left", "bottom"))

tmap_save(
  tm = sp_area_calma, filename = glue("{output06}sp_area_calma.png"),
  units = "in", height = 4.5, width = 4, dpi = 300
)

# ## bbox area calma
# bb_area <- area_calma %>% 
#   st_bbox() %>% 
#   st_as_sfc()
# 
# area_plot <- tm_shape(cwb, bbox = bb_area) +
#   tm_borders(col = "grey20") +
#   tm_fill(col = "white") +
#   tm_shape(area_calma) +
#   tm_borders(col = "red", lty = "dashed", lwd = 2) +
#   tm_shape(cwb_blocks) +
#   tm_borders(col = "grey60", lwd = 0.5) +
#   tm_fill(col = "grey90")
# 
# city_area_plot <- tm_shape(cwb_blocks) +
#   tm_borders(col = "grey60", lwd = 0.2) +
#   tm_shape(cwb) +
#   tm_borders(col = "grey30") +
#   tm_shape(bb_area) +
#   tm_borders(col = "red", lwd = 2)
#   
# ## creating viewport
# vp <- grid::viewport(0.11, 0.29, width = 0.6, height = 0.6)
# 
# ## saving plot
# tmap_save(
#   tm = area_plot, 
#   filename = glue("{output06}area_calma.pdf"), 
#   units = "in", 
#   width = 6, 
#   height = 3.5, 
#   insets_tm = city_area_plot,
#   insets_vp = vp
# )
  
# Land zoning -------------------------------------------------------------

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

# tm_shape(land_zoning) +
#   tm_fill(
#     col = "NM_GRUPO", title = "Land Zoning Group:",
#     palette = c(
#       "#8dd3C7", "#fdb462", "#bebada", "#b3de69", "#80b1d3",
#       "#bc80bd", "#ccebc5", "#ffffb3", "#fccde5", "#d9d9d9", "#fb8072"
#     )
#   ) +
#   tm_borders(col = "grey30", lwd = 0.5) +
#   tm_layout(frame = FALSE, legend.width = 1) + 
#   tm_legend(
#     legend.position = c(0.86, 0.02),
#     legend.title.size = 0.8,
#     legend.text.size = 0.6
#   )
#   tm_add_legend("fill", title = "Axes", col = "NM_GRUPO")

tmap_save(
  tm = zoning_map, filename = glue("{output06}zoning_map.png"),
  units = "in", height = 4.5, width = 6, dpi = 300
)

# Participants address ----------------------------------------------------

address <- read_excel("data/input/drivers_address.xlsx") %>% 
  st_as_sf(coords = c("LONG", "LAT")) %>% 
  st_set_crs(4674)

pr_mun <- read_municipality(code_muni = "PR")

bb <- st_bbox(address)
bb["ymax"] <- -25.34929 

rmc_roads <- opq(bbox = bb) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

address_map <- tm_shape(rmc_roads$osm_lines, bbox = bb) +
  tm_lines(col = "grey70", lwd = 0.2, alpha = 0.8) +
  tm_shape(address) +
  tm_dots(col = "red", size = 0.07, shape = 1) +
  tm_shape(pr_mun) +
  tm_borders(col = "grey30", lty = "dotted", lwd = 0.7)

tmap_save(
  address_map, glue("{output06}address_map.png"), height = 4,
  width = 4.5, dpi = 300
)

# Local mean cluster + roads ---------------------------------------------

lisa_lm_sp_shape <- lisa_lm_sp %>% 
  select(cluster) %>% 
  group_by(cluster) %>% 
  summarise() %>% 
  filter(cluster %in% c("High-High", "Low-Low"))


lisa_lmsp_roadctb <- ggplot() +
  geom_sf(data = cwb, fill = "white") +
  geom_sf(
    data = road_cwb, aes(
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
    legend.position = c(1.05, 0.2), 
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm")
  ) +
  geom_sf(
    data = lisa_lm_sp_shape, 
    color = NA,
    lwd = 0.1,
    alpha = 0.3,
    aes(fill = cluster)
  ) +
  labs(fill = "SP local mean cluster:") +
  scale_fill_manual(values = c("#ff0004", "#0700fa"))

ggsave(
  filename = glue("{output06}lisa_lmsp_roadctb.png"), 
  plot = lisa_lmsp_roadctb,
  width = 6, 
  height = 4.5, 
  dpi = 300, 
  device = "png"
)

# SP cluster + roads ------------------------------------------------------

lisa_sp_roadctb <- ggplot() +
  geom_sf(data = cwb, fill = "white") +
  geom_sf(
    data = road_cwb, aes(
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
    data = lisa_sp_shape,
    lwd = 0.1,
    alpha = 0.3,
    color = NA,
    aes(fill = cluster)
  ) +
  scale_fill_manual(values = c("#ff0004", "#0700fa"))

ggsave(
  filename = glue("{output06}lisa_sp_roadctb.png"), 
  plot = lisa_sp_roadctb,
  width = 6, 
  height = 4.5, 
  dpi = 300, 
  device = "png"
)

# # Combining and saving spatial data ---------------------------------------
# 
# gwr_coefs_selection <- function(gwr_data) {
#   gwr_data %>% 
#     st_as_sf() %>% 
#     select(Intercept:DIS, Local_R2) %>% 
#     rename_with(~str_c("coef_", .), PAR:DIS)
# }
# 
# extract_lisa_clusters <- function(lisa_data, names) {
#   lisa_data %>% 
#     select(cluster) %>% 
#     rename_with(~paste0("cluster_", names), cluster)
# }
# 
# coef_data <- gwr_chosen_model$SDF %>% gwr_coefs_selection()
# 
# lisa_data <- append(lisa_coefs, list(lisa_sp, lisa_lm_sp, lisa_r2))
# 
# lisa_cluster_names <- c(gwr_ind_var, "SP", "LM_SP", "Local_R2")
# 
# clusters_data <- map2(lisa_data, lisa_cluster_names, extract_lisa_clusters)
# 
# taz %>% 
#   st_join(st_centroid(clusters_data[[1]])) %>% 
#   qtm(fill = "cluster_AVI")
# 
# join_coef_data <- function(taz, data) {
#   taz %>% 
#     st_join(st_centroid(data))
# }
# 
# taz <- join_coef_data(taz, coef_data)
# 
# 
# join_cluster_data <- function(taz, data) {
#   taz %>% 
#     st_join(st_centroid(data)) %>% 
#     select(starts_with("cluster")) %>% 
#     st_drop_geometry()
# }
# 
# taz_cluster_data <- map(clusters_data, ~join_cluster_data(taz, data = .x))
# 
# taz_cluster_data %>% reduce(bind_cols)
# 
# taz <- bind_cols(taz, taz_cluster_data)
# 
# st_write(taz, "data/output/06/taz_data.gpkg")
# 
# st_write()

# IPPUC travel demand -----------------------------------------------------

travel_demand <- read_csv2("data/input/deslocamento.csv") %>% 
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
    data = lisa_sp_shape, 
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
  scale_fill_distiller(palette = "Greys")

ggsave(
  "data/output/06/od_plot.png",
  plot = od_plot,
  device = "png",
  height = 3.5,
  width = 5,
  dpi = 300
)

# Speed cameras and arterial ---------------------------------------------

qtm(spd_cameras, dots.col = "Tipo")

new_spd_cameras <- read_csv2("data/input/new_spd_cameras.csv")

new_spd_cameras <- new_spd_cameras %>% 
  st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(4674)

tm_shape(spd_cameras) +
  tm_dots(col = "green") +
  tm_shape(new_spd_cameras) +
  tm_dots(col = "red")
