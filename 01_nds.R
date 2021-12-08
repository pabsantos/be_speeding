output01 <- glue("{output}/01/")

# data import -------------------------------------------------------------

data_import <- function(folder, pattern) {
    names <- list.files(folder, pattern)
    path <- paste(folder, names, sep = "/")
    drivers <- vector(length = 2L)
    drivers <- map(path, read_csv)
    return(drivers)
}

drivers <- data_import(input, "^drivers")

# data fix ----------------------------------------------------------------

## Full table 1
drivers[[1]] <- drivers[[1]] %>% 
  mutate(LONG = paste(str_sub(LONG, 1, 3), str_sub(LONG, 4, -1), sep = "."),
         LAT = paste(str_sub(LAT, 1, 3), str_sub(LAT, 4, -1), sep = "."),
         LONG = as.double(LONG),
         LAT = as.double(LAT),
         DATE = lubridate::mdy(DAY_CORRIGIDO),
         PR = na_if(PR, "#VALUE!"),
         PR = na_if(PR, "0"),
         TIME = hms::as_hms(PR)) %>% 
  select(DRIVER, TRIP, ID, LONG, LAT, DATE, TIME, S, SPD_KMH, VALID_TIME)

## Full table 2
drivers[[2]] <- drivers[[2]] %>% 
  mutate(DATE = lubridate::dmy(DAY),
         TIME = hms::as_hms(PR)) %>% 
  select(DRIVER, TRIP, ID, LONG, LAT, DATE, TIME, S, SPD_KMH, VALID_TIME)

## Full table 3
drivers[[3]] <- drivers[[3]] %>% 
  mutate(DATE = mdy(DAY)) %>% 
  filter(SPD_KMH < 200) %>% 
  rename(TIME = PR) %>% 
  select(DRIVER, TRIP, ID, LONG, LAT, DATE, TIME, S, SPD_KMH, VALID_TIME)

## Full table 4
drivers[[4]] <- drivers[[4]] %>% 
  mutate(DATE = mdy(DAY...4)) %>% 
  rename(TIME = PR) %>% 
  filter(LONG != 0, LAT != 0) %>%
  select(DRIVER, TRIP, ID, LONG, LAT, DATE, TIME, S, SPD_KMH, VALID_TIME)
  

## Combining
drivers_full <- reduce(drivers, bind_rows)

rm(drivers)

# Extracting full time traveled --------------------------------------------

full_time <- drivers_full %>% 
  drop_na(S) %>% 
  select(S) %>% 
  sum() / 3600

# Extracting full travel distance ------------------------------------------

## Creating a linestring sf
drivers_full_sf <- drivers_full %>% 
  drop_na(LONG, LAT) %>% 
  mutate(lag = TIME - lag(TIME),
         WKT = case_when(
           lag == 1 ~ paste("LINESTRING (", lag(LONG), " ", lag(LAT), ", ", 
                            LONG, " ", LAT, ")", sep = ""),
           lag > 1 ~ "0",
           lag < 1 ~ "0",
           TRUE ~ NA_character_)) %>% 
  filter(WKT != "0") %>% 
  select(-lag) %>% 
  st_as_sf(wkt = "WKT") %>% 
  st_set_crs(4674)

## Travel distance
full_dist <- drivers_full_sf %>% 
  st_length() %>% 
  sum()

# Spatial operations -------------------------------------------------------

## Creating point sf
drivers_full_sf <- drivers_full %>% 
  drop_na(LONG, LAT) %>% 
  st_as_sf(coords = c("LONG", "LAT")) %>% 
  st_set_crs(4674)

rm(drivers_full)

## Importing Curitiba data
cod <- 4106902
cwb <- read_municipality(code_muni = cod, year = 2010)

## Filtering points inside Curitiba
drivers_cwb_sf <- drivers_full_sf %>% 
  st_join(cwb["name_muni"]) %>% 
  filter(name_muni == "Curitiba") %>% 
  select(-name_muni)

rm(drivers_full_sf)

## Importing road axis
axis <- st_read(glue("{input}/eixo_osm+ippuc_lim_velocidade.gpkg"))

## Extracting speed limits
drivers_cwb_sf <- drivers_cwb_sf %>% 
  st_transform(crs = 31982) %>% 
  st_join(st_buffer(axis["limite_vel"], dist = 10)) %>% 
  filter(!is.na(limite_vel)) %>% 
  distinct(DATE, TIME, .keep_all = TRUE) %>% 
  st_transform(crs = 4674)

rm(axis)

## Removing first trips and selecting valid times
drivers_cwb_sf <- drivers_cwb_sf %>% 
  filter(TRIP != 1,
         VALID_TIME %in% c("yes", "Yes", "True")) %>% 
  select(-VALID_TIME)

# Valid travel time -------------------------------------------------------

valid_time <- drivers_cwb_sf %>% 
  drop_na(S) %>% 
  pull(S) %>% 
  sum() / 3600

# Valid travel distance ---------------------------------------------------

## Transforming into linestring
drivers_lines_sf <- drivers_cwb_sf %>% 
  mutate(coords = st_as_text(geometry)) %>% 
  st_drop_geometry() %>% 
  separate(coords, into = c("point", "coords"), sep = "\\s", 
           extra = "merge") %>% 
  mutate(lag = TIME - lag(TIME),
         WKT = case_when(
           lag == 1 ~ paste("LINESTRING (", str_sub(lag(coords),2,-2), ", ", 
                            str_sub(coords,2,-2), ")", sep = ""),
           lag > 1 ~ "0",
           lag < 1 ~ "0",
           TRUE ~ NA_character_)) %>% 
  filter(WKT != "0") %>%
  drop_na(WKT) %>% 
  select(-lag, -S, -point, -coords) %>%
  st_as_sf(wkt = "WKT") %>%
  st_set_crs(4674)

rm(drivers_cwb_sf)  

## Extracting distance
valid_dist <- drivers_lines_sf %>% 
  st_length() %>% 
  sum()

# Distance in exposure speeds ---------------------------------------------

exp_dist <- drivers_lines_sf %>% 
  mutate(exp_spd = limite_vel - SPD_KMH) %>% 
  filter(exp_spd < 10) %>% 
  st_length() %>% 
  sum()

# Distance in speeding ----------------------------------------------------

spd_dist <- drivers_lines_sf %>% 
  mutate(speeding = SPD_KMH - limite_vel) %>% 
  filter(speeding > 5) %>% 
  st_length() %>% 
  sum()

# Gathering and exporting results -----------------------------------------

results <- c(full_time, full_dist, valid_time, valid_dist, exp_dist, spd_dist)
names(results) <- c("Full travel time [h]", 
                    "Full traveled distance [m]", 
                    "Valid travel time [h]", 
                    "Valid traveled distance [m]",
                    "Traveled distance in exposure [m]", 
                    "Speeding distance [m]")

results <- broom::tidy(results)
write_csv(results, glue('{output01}nds_results.csv'))
