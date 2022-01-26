output01 <- glue("{output}/01/")

data_import <- function(folder, pattern) {
    names <- list.files(folder, pattern)
    path <- paste(folder, names, sep = "/")
    drivers <- vector(length = 4L)
    drivers <- map(path, read_csv)
    return(drivers)
}

drivers <- data_import(input, "^drivers_pt")

fix_sample <- function(table) {
  table %>% 
    mutate(trip = as.character(TRIP)) %>% 
    rename(
      driver = DRIVER,
      id = ID,
      long = LONG,
      lat = LAT,
      s = S,
      spd_kmh = SPD_KMH,
      valid_time = VALID_TIME,
      time = PR
    ) %>% 
    select(driver, trip, id, long, lat, date, time, s, spd_kmh, valid_time)
}

fix_date <- function(table, var) {
  table %>% 
    mutate(date = lubridate::dmy({{ var }}))
}

drivers <- map(drivers, ~fix_date(table = .x, var = DAY))

drivers <- map(drivers, fix_sample)

drivers_full <- reduce(drivers, bind_rows)

rm(drivers)

# Extracting full time traveled --------------------------------------------

calc_full_time <- function(table) {
  table %>% 
    drop_na(s) %>% 
    select(s) %>% 
    sum() / 3600
}

full_time <- calc_full_time(drivers_full)

# Extracting full travel distance ------------------------------------------

create_linestring <- function(table) {
  table %>% 
    drop_na(long, lat) %>% 
    filter(long != 0, lat != 0) %>% 
    mutate(
      lag = time - lag(time),
      wkt = case_when(
        lag == 1 ~ paste0(
          "LINESTRING (", lag(long), " ", lag(lat), ", ", long, " ", lat, ")"
        ),
        lag > 1 ~ "0",
        lag < 1 ~ "0",
        TRUE ~ NA_character_
      )
    ) %>% 
    filter(wkt != "0") %>% 
    select(-lag) %>% 
    st_as_sf(wkt = "wkt") %>% 
    st_set_crs(4674)
}

calc_dist <- function(sf) {
  sf %>% st_length() %>% sum()
}

drivers_full_sf <- create_linestring(drivers_full)

full_distance <- calc_dist(drivers_full_sf)

# Spatial operations -------------------------------------------------------

create_points <- function(table) {
  table %>% 
    drop_na(long, lat) %>% 
    filter(long != 0, lat != 0) %>% 
    st_as_sf(coords = c("long", "lat")) %>% 
    st_set_crs(4674)
}

filter_cwb_points <- function(points, cwb) {
  points %>% 
    st_join(cwb["name_muni"]) %>% 
    filter(name_muni == "Curitiba") %>% 
    select(-name_muni)
}

extract_speed_limits <- function(points, axis) {
  points %>% 
    st_transform(crs = 31982) %>% 
    st_join(st_buffer(axis["limite_vel"], dist = 10)) %>% 
    filter(!is.na(limite_vel)) %>% 
    distinct(date, time, .keep_all = TRUE) %>% 
    st_transform(crs = 4674)
}

filter_valid_time <- function(points) {
  points %>% filter(valid_time == "Yes")
}

cod <- 4106902
cwb <- read_municipality(code_muni = cod, year = 2010)

axis <- st_read(glue("{input}/eixo_osm+ippuc_lim_velocidade.gpkg"))

drivers_cwb_sf <- drivers_full %>% 
  create_points() %>% 
  filter_cwb_points(cwb) %>% 
  extract_speed_limits(axis) %>% 
  filter_valid_time()

rm(drivers_full)

rm(drivers_full_sf)

rm(axis)

# Valid travel time -------------------------------------------------------

calc_valid_time <- function(points) {
  points %>% drop_na(s) %>% pull(s) %>% sum() / 3600
}

valid_time <- calc_valid_time(drivers_cwb_sf)

# Valid travel distance ---------------------------------------------------

transform_linestring <- function(points) {
  points %>% 
    mutate(coords = st_as_text(geometry)) %>% 
    st_drop_geometry() %>% 
    separate(
      coords, into = c("point", "coords"), sep = "\\s", extra = "merge"
    ) %>% 
    mutate(
      lag = time - lag(time),
      wkt = case_when(
        lag == 1 ~ paste0(
          "LINESTRING (", str_sub(lag(coords), 2, -2), ", ", 
          str_sub(coords, 2, -2), ")"
        ),
        lag > 0 ~ "0",
        lag < 0 ~ "0",
        TRUE ~ NA_character_
      )
    ) %>% 
    filter(wkt != "0") %>% 
    drop_na(wkt) %>% 
    select(-lag, -s, -point, -coords) %>% 
    st_as_sf(wkt = "wkt") %>% 
    st_set_crs(4674)
}

drivers_lines_sf <- transform_linestring(drivers_cwb_sf)

rm(drivers_cwb_sf)  

calc_valid_dist <- function(lines) {
  lines %>% st_length() %>% sum()
}

valid_dist <- calc_valid_dist(drivers_lines_sf)

calc_exp_dist <- function(lines) {
  lines %>% 
    mutate(exp_spd = limite_vel - spd_kmh) %>% 
    filter(exp_spd < 10) %>% 
    st_length() %>% 
    sum()
}

exp_dist <- calc_exp_dist(drivers_lines_sf)

calc_spd_dist <- function(lines) {
  lines %>% 
    mutate(speeding = spd_kmh - limite_vel) %>% 
    filter(speeding > 5) %>% 
    st_length() %>% 
    sum()
}

spd_dist <- calc_spd_dist(drivers_lines_sf)

# Gathering and exporting results -----------------------------------------

results <- c(
  full_time, full_distance, valid_time, valid_dist, exp_dist, spd_dist
)

names(results) <- c(
  "Full travel time [h]", "Full traveled distance [m]", 
  "Valid travel time [h]", "Valid traveled distance [m]",
  "Traveled distance in exposure [m]", "Speeding distance [m]"
)

results <- broom::tidy(results)
write_csv(results, glue('{output01}nds_results.csv'))
