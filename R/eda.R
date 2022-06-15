transform_km <- function(driver_data) {
  driver_data %>% 
    mutate(
      DISTANCE = units::set_units(st_length(wkt_lines), km),
      DISTANCE = units::drop_units(DISTANCE)
    )
}

extract_time_date <- function(driver_data) {
  driver_data %>% 
    st_drop_geometry() %>% 
    mutate(
      dow = lubridate::wday(
        DAY, label = TRUE, abbr = TRUE, locale = "en_US.utf8"
      ),
      hotd = lubridate::hour(lubridate::hms(PR))
    )
}

plot_dotw_dist <- function(driver_data) {
  driver_data %>% 
    group_by(dow) %>% 
    summarise(DISTANCE = sum(DISTANCE)) %>% 
    ggplot(aes(x = dow, y = DISTANCE)) +
    geom_col() +
    labs(x = "Day of the week", y = "Traveled distance (km)") +
    theme_minimal()
}

plot_dotw_trips <- function(driver_data) {
  driver_data %>% 
    group_by(dow) %>% 
    summarise(trips = n_distinct(ID)) %>% 
    ggplot(aes(dow, trips)) +
    geom_col() +
    labs(x = "Day of the week", y = "Trips") +
    scale_y_continuous(breaks = seq(0,150,10), minor_breaks = NULL) +
    theme_minimal()
}

plot_hotd_dist <- function(driver_data) {
  driver_data %>% 
    group_by(hotd) %>% 
    summarise(DISTANCE = sum(DISTANCE)) %>% 
    ggplot(aes(hotd, DISTANCE)) +
    geom_col() +
    scale_x_continuous(breaks = seq(0,23,1), minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    labs(x = "Hour of the day", y = "Traveled distance (km)") +
    theme_minimal()
}

plot_hotd_trips <- function(driver_data) {
  driver_data %>% 
    group_by(ID) %>% 
    slice_head(n = 1) %>% 
    group_by(hotd) %>% 
    summarise(n = n()) %>% 
    ggplot(aes(hotd, n)) +
    geom_col() +
    scale_x_continuous(breaks = seq(0,23,1), minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    labs(x = "Trip start hour", y = "Trips") +
    theme_minimal()
}

plot_hotd_sp <- function(driver_data) {
  driver_data %>% 
    mutate(
      IS_EXPOSURE = if_else(as.numeric(LIMITE_VEL) - SPD_KMH < 10, TRUE, FALSE),
      IS_SPEEDING = if_else(SPD_KMH - as.numeric(LIMITE_VEL) > 5, TRUE, FALSE)
    ) %>% 
    filter(IS_EXPOSURE == TRUE) %>% 
    group_by(hotd, IS_SPEEDING) %>% 
    summarise(DISTANCE = sum(DISTANCE)) %>% 
    pivot_wider(names_from = IS_SPEEDING, values_from = DISTANCE) %>% 
    mutate(SPEEDING = `TRUE` / (`TRUE` + `FALSE`)) %>% 
    ggplot(aes(hotd, SPEEDING)) +
    geom_col() +
    scale_x_continuous(breaks = seq(0,23,1), minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL, breaks = seq(0,0.6,0.1)) +
    labs(x = "Hour of the day", y = "SP") +
    theme_minimal()
}

plot_dotw_sp <- function(driver_data) {
  driver_data %>% 
    mutate(
      IS_EXPOSURE = if_else(as.numeric(LIMITE_VEL) - SPD_KMH < 10, TRUE, FALSE),
      IS_SPEEDING = if_else(SPD_KMH - as.numeric(LIMITE_VEL) > 5, TRUE, FALSE)
    ) %>%
    filter(IS_EXPOSURE == TRUE) %>% 
    group_by(dow, IS_SPEEDING) %>% 
    summarise(DISTANCE = sum(DISTANCE)) %>% 
    pivot_wider(names_from = IS_SPEEDING, values_from = DISTANCE) %>% 
    mutate(SPEEDING = `TRUE` / (`TRUE` + `FALSE`)) %>% 
    ggplot(aes(x = dow, y = SPEEDING)) +
    geom_col() +
    scale_y_continuous(minor_breaks = NULL, breaks = seq(0,0.5,0.1)) +
    labs(x = "Day of the week", y = "SP") +
    theme_minimal()
}

plot_richards_sp <- function(driver_data) {
  driver_data %>% 
    mutate(
      IS_EXPOSURE = if_else(as.numeric(LIMITE_VEL) - SPD_KMH < 10, TRUE, FALSE),
      IS_SPEEDING = if_else(SPD_KMH - as.numeric(LIMITE_VEL) > 5, TRUE, FALSE)
    ) %>%
    filter(IS_EXPOSURE == TRUE) %>%
    group_by(DRIVER, ID, IS_SPEEDING) %>% 
    summarise(DISTANCE = sum(DISTANCE)) %>% 
    pivot_wider(names_from = IS_SPEEDING, values_from = DISTANCE) %>% 
    mutate(
      SPEEDING = `TRUE` / (`TRUE` + `FALSE`),
      SPEEDING = if_else(is.na(SPEEDING), 0, SPEEDING),
      SPEEDING_N = if_else(SPEEDING > 0, 1, 0)
    ) %>% 
    group_by(DRIVER) %>% 
    summarise(
      MEAN_SP = mean(SPEEDING),
      TRIPS_SP = sum(SPEEDING_N) / n_distinct(ID),
      TRIPS = n_distinct(ID)
    ) %>% 
    ggplot(aes(x = TRIPS_SP, y = MEAN_SP)) +
    geom_point(color = "midnightblue", alpha = 0.4, aes(size = TRIPS)) +
    geom_hline(yintercept = 0.2, linetype = 2) +
    geom_vline(xintercept = 0.2, linetype = 2) +
    annotate(
      geom = "text", 
      x = c(0.1, 0.1, 0.7, 0.7), 
      y = c(0.7, 0.1, 0.1, 0.7), 
      label = c(
        "Situational\nspeeding", "Incidental\nspeeding", 
        "Casual\nspeeding", "Habitual\nspeeding"
      ), size = 3
    ) +
    scale_x_continuous(
      limits = c(0,1), minor_breaks = NULL,breaks = seq(0,1,0.2)
    ) +
    scale_y_continuous(
      limits = c(0,1), minor_breaks = NULL, breaks = seq(0,1,0.2)
    ) +
    labs(
      x = "Percent of trips with any speeding",
      y = "Average speeding per trip"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

extract_sp_summary <- function(driver_data, group) {
  driver_data %>% 
    mutate(
      IS_EXPOSURE = if_else(as.numeric(LIMITE_VEL) - SPD_KMH < 10, TRUE, FALSE),
      IS_SPEEDING = if_else(SPD_KMH - as.numeric(LIMITE_VEL) > 5, TRUE, FALSE)
    ) %>% 
    filter(IS_EXPOSURE == TRUE) %>% 
    group_by({{ group }}, IS_SPEEDING) %>% 
    summarise(DISTANCE = sum(DISTANCE)) %>% 
    pivot_wider(names_from = IS_SPEEDING, values_from = DISTANCE) %>%
    mutate(across(`FALSE`:`TRUE`, ~if_else(is.na(.x), 0, .x))) %>% 
    mutate(speeding = `TRUE` / (`TRUE` + `FALSE`)) %>% 
    summary()
}

save_eda_plots <- function() {
  plots <- list(
    dotw_dist, dotw_trips, dotw_sp, hotd_dist, hotd_trips, hotd_sp, richards_sp
  )
  
  names <- c(
    "dotw_dist", "dotw_trips", "dotw_sp", "hotd_dist", "hotd_trips", "hotd_sp",
    "richards_sp"
  )
  
  names <- paste0("plot/", names, ".png")
  
  save_plots <- function(plots, names) {
    ggsave(names, plots, device = "png", dpi = 300, width = 6, height = 3.5)
  }
  
  map2(plots, names, save_plots)
  
}
