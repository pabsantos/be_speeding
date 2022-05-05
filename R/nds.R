fix_date <- function(data, var) {
  data %>% mutate(DAY = lubridate::dmy({{ var }}))
}

load_sample <- function() {
  if (file.exists("data/nds_sample.rds")) {
    readRDS("data/nds_sample.rds")
  } else {
    nds_vars <- c(
      "DRIVER", "LONG", "LAT", "DAY", "TRIP", "ID", "PR", "TIME_ACUM", 
      "SPD_KMH", "VALID_TIME", "CIDADE", "HIERARQUIA_CTB", "HIERARQUIA_CWB",
      "LIMITE_VEL"
    )
      
    drivers <- nds_load_data(
      "Fulltable", 
      folder = "data", 
      sep = ";",
      vars = nds_vars
    )
    
    drivers <- drivers %>% fix_date(DAY)
    saveRDS(drivers, "data/nds_sample.rds")
    return(drivers)
  }
}

calc_dist <- function(data) {
  data %>% 
    nds_calc_dist(geom = wkt_lines, by = DRIVER, units = "kilometers") %>% 
    pull(DIST) %>% 
    sum()
}

calc_time <- function(data) {
  data %>% 
    nds_calc_time(by = DRIVER, units = "hours") %>% 
    pull(TIME) %>% 
    sum()
}

calc_exp_dist <- function(data, exp) {
  data %>% 
    mutate(EXP_SPD = as.numeric(LIMITE_VEL) - SPD_KMH) %>% 
    filter(EXP_SPD < exp) %>% 
    calc_dist()
}

calc_spd_dist <- function(data, spd) {
  data %>% 
    mutate(SPEEDING = SPD_KMH - as.numeric(LIMITE_VEL)) %>% 
    filter(SPEEDING > spd) %>% 
    calc_dist()
}

extract_dist_summary <- function(data) {
  data %>% 
    nds_calc_dist(geom = wkt_lines, by = ID, units = "kilometers") %>% 
    summary()
}

extract_trips_summary <- function(data) {
  data %>% 
    st_drop_geometry() %>% 
    group_by(DRIVER) %>% 
    summarise(TRIPS = n_distinct(ID)) %>% 
    summary()
}

save_nds_results <- function() {
  nds_results <- c(
    full_time, full_distance, valid_time, valid_distance, valid_trips, 
    exp_distance, spd_distance
  )
  
  names_results <- c(
    "Full travel time [h]", "Full traveled distance [km]", 
    "Valid travel time [h]", "Valid traveled distance [km]", "Valid trips",
    "Traveled distance in exposure [km]", "Speeding distance [km]"
  )
  
  nds_results <- tibble(item = names_results, values = nds_results)
  write_csv(nds_results, "table/nds_results.csv")
}
