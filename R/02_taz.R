output02 <- glue("{output}/02/")

taz <- st_read(glue("{input}/taz.gpkg")) %>% 
  rename(id_taz = cod_taz, neigh = BAIRRO) %>% 
  mutate(id_taz = as.character(id_taz))

# Adding population and income data [PD and AVI] --------------------------

census_tract <- read_census_tract(code_tract = cod, year = 2010) %>% 
  mutate(code_tract = as.character(code_tract)) %>% 
  select(code_tract)

census_data <- read_csv(glue("{input}/Basico_PR.csv")) %>% 
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

taz_pop_income <- census_tract %>% 
  st_transform(crs = 31982) %>% 
  mutate(
    area = st_area(geom),
    income_area = income * area
  ) %>% 
  st_centroid() %>% 
  st_join(taz["id_taz"]) %>% 
  group_by(id_taz) %>% 
  summarise(
    population = sum(population, na.rm = T),
    income_area = sum(income_area, na.rm = T),
    area = sum(area)
  ) %>% 
  mutate(income = income_area / area) %>% 
  st_drop_geometry()

taz <- taz %>% 
  left_join(taz_pop_income, by = "id_taz") %>% 
  mutate(
    area = units::set_units(st_area(geom), km^2),
    PD = population / area
  ) %>% 
  rename(AVI = income) %>% 
  select(-income_area, -population)

rm(census_tract)
rm(census_data)
rm(taz_pop_income)

# Street network density --------------------------------------------------

download_sf <- function(url) {
  temp <- tempfile()
  temp2 <- tempfile()
  download.file(url, destfile = temp)
  unzip(zipfile = temp, exdir = temp2)
  file <- st_read(temp2)
  unlink(c(temp, temp2))
  return(file)
}

url <- "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/EIXO_RUA_SIRGAS.zip"

ippuc_axis <- download_sf(url)

taz_road_length <- ippuc_axis %>% 
  st_join(taz["id_taz"]) %>% 
  mutate(length = units::set_units(st_length(geometry), km)) %>% 
  group_by(id_taz) %>% 
  summarise(road_length = sum(length)) %>% 
  st_drop_geometry()

taz <- taz %>% 
  left_join(taz_road_length, by = "id_taz") %>% 
  mutate(SND = road_length / area)

rm(taz_road_length)

# Proportion of arterial roads --------------------------------------------

taz_par <- ippuc_axis %>% 
  st_join(taz["id_taz"]) %>% 
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

taz <- taz %>% 
  left_join(taz_par, by = "id_taz")

rm(taz_par)
rm(ippuc_axis)

# Density of intersections ------------------------------------------------

intersection <- st_read(glue("{input}/intersection.shp"))

taz_di <- intersection %>% 
  st_transform(crs = 31982) %>% 
  st_join(taz["id_taz"]) %>% 
  group_by(id_taz) %>% 
  summarise(INTERSECTIONS = n()) %>% 
  st_drop_geometry()

taz <- taz %>% 
  left_join(taz_di, by = "id_taz") %>% 
  mutate(DIS = INTERSECTIONS / road_length) %>% 
  select(-INTERSECTIONS)

rm(taz_di)
rm(intersection)

# Traffic signal density --------------------------------------------------

traffic_signal <- st_read(glue("{input}/traffic_lights.shp"))

taz_ts <- traffic_signal %>% 
  st_transform(crs = 31982) %>% 
  st_join(taz["id_taz"]) %>% 
  group_by(id_taz) %>% 
  summarise(SIGNALS = n()) %>% 
  st_drop_geometry()

taz <- taz %>% 
  left_join(taz_ts, by = "id_taz") %>% 
  mutate(TSD = SIGNALS / road_length) %>% 
  select(-SIGNALS)

rm(taz_ts)
rm(traffic_signal)

# Density of commercial and services units ---------------------------------

alvaras <- st_read(glue("{input}/alvara_comercial_ATIVO.shp"))

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
  st_join(taz["id_taz"]) %>% 
  group_by(id_taz) %>% 
  summarise(UNITS = n()) %>% 
  st_drop_geometry()

taz <- taz %>% 
  left_join(taz_units, by = "id_taz") %>% 
  mutate(DCSU = UNITS / area) %>% 
  select(-UNITS)

rm(alvaras)
rm(taz_units)

# Land use diversity index ------------------------------------------------

url <- "https://ippuc.org.br/geodownloads/SHAPES_SIRGAS/ZONEAMENTO_15511_2019_SIRGAS.zip"

land_zoning <- download_sf(url)

land_zoning_taz <- land_zoning %>% 
  st_intersection(taz["id_taz"])

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

taz <- taz %>% 
  left_join(taz_ldi, by = "id_taz")

rm(land_zoning_taz)
rm(zoning_i)
rm(zoning_total)
rm(taz_ldi)

# Bus stop density --------------------------------------------------------

bus_stops <- st_read(glue("{input}/PONTO_DE_ONIBUS.shp"))

taz_bsd <- bus_stops %>% 
  filter(CATEGORIA != "EXPRESSO" & CATEGORIA != "EXPRESSO LIGEIRÃO") %>% 
  st_join(taz["id_taz"]) %>% 
  group_by(id_taz) %>% 
  summarise(STOPS = n()) %>% 
  st_drop_geometry()

taz <- taz %>% 
  left_join(taz_bsd, by = "id_taz") %>% 
  mutate(BSD = STOPS / road_length) %>% 
  select(-STOPS)

rm(taz_bsd)
rm(bus_stops)

# Density of speed cameras ------------------------------------------------

spd_cameras <- st_read(glue("{input}/speed_traps.shp")) # UPDATE SOURCE FILE

taz_dsc <- spd_cameras %>% 
  st_transform(crs = 31982) %>% 
  st_join(taz["id_taz"]) %>% 
  group_by(id_taz) %>% 
  summarise(CAMERAS = n()) %>% 
  st_drop_geometry()

taz <- taz %>% 
  left_join(taz_dsc, by = "id_taz") %>% 
  mutate(DSC = CAMERAS / road_length) %>% 
  select(-CAMERAS)

rm(taz_dsc)
rm(spd_cameras)

# Speeding, exposure and traveled distance --------------------------------

taz_distances <- drivers_lines_sf %>% 
  mutate(
    DISTANCE_TYPE = case_when(
      limite_vel - spd_kmh < 10 & limite_vel - spd_kmh >= -5 ~ "EXPOSURE",
      spd_kmh - limite_vel > 5 ~ "SPEEDING",
      TRUE ~ "NORMAL"
    ),
    LENGTH = st_length(wkt)
  ) %>% 
  st_transform(crs = 31982) %>% 
  st_join(taz["id_taz"]) %>% 
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
  mutate(SPEEDING = DIST_SPD / DIST_TOTAL)

taz <- taz %>% 
  left_join(taz_distances, by = "id_taz")

rm(taz_distances)

# Removing NAs and units --------------------------------------------------

taz <- taz %>% 
  mutate_if(is.numeric, units::drop_units) %>% 
  replace(is.na(.), 0)

# Plotting travel and speeding---------------------------------------------

plot_sp_dist_maps <- function(var, legend) {
  taz %>% 
    ggplot() +
    geom_sf(aes(fill = var), color = NA) + 
    theme_void() +
    scale_fill_viridis_c(option = "D") +
    labs(fill = legend) +
    theme(
      legend.position = c(0.95, 0.20),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.5, "cm")
    )
}

sp_var <- taz %>% select(SPEEDING, DIST_TOTAL) %>% st_drop_geometry()

sp_legend <- c("Speeding:", "Traveled\ndistances [km]:")

sp_dist_maps <- map2(sp_var, sp_legend, plot_sp_dist_maps)

save_sp_maps <- function(plot, names) {
  names <- glue("{output02}{names}.png")
  ggsave(names, plot, device = "png", height = 4.5, width = 4, dpi = 300)
}

sp_names <- c("map_SP", "map_DIST_TOTAL")

map2(sp_dist_maps, sp_names, save_sp_maps)

# Plotting remaining variables -------------------------------------------

plot_var <- function(var, title, palette) {
  tm_shape(taz) + 
    tm_fill(
      col = var, n = 6, 
      style = if_else(var == "DSC", "jenks", "quantile"), 
      palette = palette,
      title = title
    ) +
    tm_borders(col = "black", lwd = 0.2) +
    tm_layout(frame = FALSE, legend.width = 0.5) + 
    tm_legend(
      legend.position = c(0.75,0.00),
      legend.title.size = 0.5, 
      legend.text.size = 0.5
    )
}

var <- c(
  "PD", "LDI", "DIS", "DSC", "TSD", "PAR", "SND", "DCSU", "BSD", "AVI"
)

title <- c(
  "Pop. density\n[inhab./km²]:", "Land use\ndiversity index:",
  "Density of intersections\n[no./km]:", 
  "Density of speed cameras\n[no./km]:",
  "Traffic signal density\n[no./km]:",
  "Proportion of\narterial roads:",
  "Street network density\n[km/km²]:",
  "Density of commercial\nand services units [no./km²]:",
  "Bus stop density\n[no./km]:",
  "Average income\n[BRL]:"
)

palette <- c(
  "Blues", "Greens", "Reds", "Greys", "Oranges", "Purples",
  "YlGn", "BuGn", "YlGnBu", "PuBu"
)


var_maps <- pmap(list(var, title, palette), plot_var)
names(var_maps) <- var

save_var_maps <- function(plot, names) {
  names <- glue("{output02}map_{names}.png")
  tmap_save(
    tm = plot,
    filename = names,
    height = 3.5,
    width = 3,
    units = "in",
    dpi = 300
  )
}

var_names <- var

map2(var_maps, var_names, save_var_maps)
