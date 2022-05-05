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
