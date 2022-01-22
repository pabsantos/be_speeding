output06 <- glue("{output}/06/")

# Curitiba's statistical grid (pop.) --------------------------------------

grid <- read_statistical_grid(code_grid = 25, year = 2010)

cwb_grid <- grid %>% 
  st_join(cwb["name_muni"]) %>% 
  filter(name_muni == "Curitiba")

rm(grid)

grid_map <- tm_shape(cwb_grid) +
  tm_fill(col = "POP", alpha = 1, style = "cont", n = 8, 
          title = "Inhabitants per 500 m²:", palette = "viridis") +
  tm_layout(frame=FALSE, legend.width = 1) + 
  tm_legend(legend.position = c(0.88, 0.02),
            legend.title.size = 0.8, 
            legend.text.size = 0.6)

tmap_save(tm = grid_map, filename = glue("{output06}grid_map.png"),
          units = "in", height = 3.5, width = 5)

# Road system (CTB) --------------------------------------------------------

road_cwb <- download_sf("https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/EIXO_RUA_SIRGAS.zip")

road_cwb <- road_cwb %>% 
  filter(HIERARQUIA %in% c("1", "2", "3", "4")) %>% 
  mutate(HIERARQUIA = case_when(
    HIERARQUIA == "1" ~ "Rapid transit",
    HIERARQUIA == "2" ~ "Arterial", 
    HIERARQUIA == "3" ~ "Collector",
    HIERARQUIA == "4" ~ "Local",
    TRUE ~ NA_character_),
         HIERARQUIA = factor(HIERARQUIA, 
                             levels = c("Rapid transit", "Arterial",
                                        "Collector", "Local")))

road_cwb_plot <- ggplot() +
  geom_sf(data = cwb, fill = NA) +
  geom_sf(data = road_cwb, aes(color = HIERARQUIA, size = HIERARQUIA,
                               alpha = HIERARQUIA)) +
  theme_void() +
  scale_color_manual(values = c("red", "darkgreen", "orange", "grey")) +
  scale_alpha_manual(values = c(1, 0.9, 0.9, 0.6)) +
  scale_size_manual(values = c(0.4, 0.3, 0.2, 0.1)) +
  labs(color = "Road category:",
       alpha = "Road category:",
       size = "Road category:") +
  theme(legend.position = c(0.90, 0.15), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.4, "cm"))

ggsave(filename = glue("{output06}road_cwb_map.pdf"), plot = road_cwb_plot,
       width = 3, height = 3.5, dpi = 300, device = "pdf")

# Road system (zoning) ----------------------------------------------------

road_zoning_cwb <- st_read("input/EIXO_RUA_UTF.shp")

road_zoning_plot <- road_zoning_cwb %>% 
  select(SVIARIO) %>% 
  mutate(SVIARIO = case_when(
    str_sub(SVIARIO, 1, 7) == "RODOVIA" ~ "Highway",
    str_sub(SVIARIO, 1, 8) == "COLETORA" ~ "Collector",
    SVIARIO == "EXTERNA" ~ "Structural",
    str_sub(SVIARIO, 1, 8) == "SETORIAL" ~ "Sectorial",
    SVIARIO == "OUTRAS VIAS" ~ "Other",
    SVIARIO == "CENTRAL" ~ "Structural", 
    SVIARIO == "NORMAL" ~ "Other",
    SVIARIO == "PRIORIT�RIA" ~ "Priorized",
    TRUE ~ NA_character_),
         SVIARIO = factor(
           SVIARIO, levels = c("Highway", "Structural", "Sectorial", 
                               "Collector", "Priorized", "Other"))) %>% 
  ggplot() +
  geom_sf(data = cwb, color = "grey40", fill = NA, lwd = 0.4) +
  geom_sf(aes(color = SVIARIO, alpha = SVIARIO, size = SVIARIO)) +
  theme_void() +
  scale_color_manual(values = c("#E41A1E", "#FF7F1B", "#4DB152",
                                "#387CB6", "#9849A0", "grey")) +
  scale_alpha_manual(values = c(1, 0.9, 0.8, 0.8, 0.8, 0.6)) +
  scale_size_manual(values = c(0.4, 0.3, 0.2, 0.2, 0.2, 0.1)) +
  labs(color = "Road category:",
       alpha = "Road category:",
       size = "Road category:") +
  theme(legend.position = c(0.95, 0.15), 
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.4, "cm"))

ggsave(filename = glue("{output06}road_zoning_map.pdf"), 
       plot = road_zoning_plot,
       width = 4, height = 4.5, dpi = 300, device = "pdf")

# Zona calma --------------------------------------------------------------

area_calma <- st_read(glue("{input}/areacalma.shp"))
cwb_blocks <- download_sf("https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/ARRUAMENTO_QUADRAS_SIRGAS.zip")

## Transforming to sirgas
area_calma <- area_calma %>% 
  st_transform(crs = 4674)

## bbox area calma
bb_area <- area_calma %>% 
  st_bbox() %>% 
  st_as_sfc()

area_plot <- tm_shape(cwb, bbox = bb_area) +
  tm_borders(col = "grey20") +
  tm_fill(col = "white") +
  tm_shape(area_calma) +
  tm_borders(col = "red", lty = "dashed", lwd = 2) +
  tm_shape(cwb_blocks) +
  tm_borders(col = "grey60", lwd = 0.5) +
  tm_fill(col = "grey90")

city_area_plot <- tm_shape(cwb_blocks) +
  tm_borders(col = "grey60", lwd = 0.2) +
  tm_shape(cwb) +
  tm_borders(col = "grey30") +
  tm_shape(bb_area) +
  tm_borders(col = "red", lwd = 2)
  
## creating viewport
vp <- grid::viewport(0.11, 0.29, width = 0.6, height = 0.6)

## saving plot
tmap_save(tm = area_plot, filename = glue("{output06}area_calma.pdf"), 
          units = "in", width = 6, height = 3.5, insets_tm = city_area_plot,
          insets_vp = vp)
  
# Land zoning -------------------------------------------------------------

ambiental <- "Environmental Protection Area"

land_zoning <- land_zoning %>% 
  mutate(NM_GRUPO = case_when(
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
    TRUE ~ "Residential Zoning")) %>% 
  group_by(NM_GRUPO) %>% 
  summarise()

zoning_map <- tm_shape(land_zoning) +
  tm_fill(col = "NM_GRUPO", title = "Land Zoning Group:",
          palette = c("#8dd3C7", "#fdb462", "#bebada", "#b3de69", "#80b1d3",
                      "#bc80bd", "#ccebc5", "#ffffb3", "#fccde5", "#d9d9d9",
                      "#fb8072")) +
  tm_borders(col = "grey30", lwd = 0.5) +
  tm_layout(frame=FALSE, legend.width = 1) + 
  tm_legend(legend.position = c(0.86, 0.02),
            legend.title.size = 0.8, 
            legend.text.size = 0.6)

tmap_save(tm = zoning_map, filename = glue("{output06}zoning_map.pdf"),
          units = "in", height = 4, width = 5)

# Participants address ----------------------------------------------------

address <- read_excel("input/address.xlsx") %>% 
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

tmap_save(address_map, glue("{output06}address_map.png"), height = 4,
          width = 4.5, dpi = 300)

# Comparing clusters ------------------------------------------------------



