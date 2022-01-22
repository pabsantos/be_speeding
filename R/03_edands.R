output03 <- glue("{output}/03/")

drivers_lines_sf <- drivers_lines_sf %>% 
  mutate(DISTANCE = units::set_units(st_length(WKT), km),
         DISTANCE = units::drop_units(DISTANCE))

# Distance traveled per day of the week -----------------------------------

theme_set(theme_minimal())

drivers_time_date <- drivers_lines_sf %>% 
  st_drop_geometry() %>% 
  mutate(dow = wday(DATE, label = TRUE, abbr = TRUE),
         hotd = hour(TIME),
         IS_AM = am(TIME))

dotw_dist <- drivers_time_date %>% 
  group_by(dow) %>% 
  summarise(DISTANCE = sum(DISTANCE)) %>% 
  ggplot(aes(x = dow, y = DISTANCE)) +
  geom_col() +
  labs(x = "Day of the week", y = "Traveled distance (km)")

# Trips per day of the week -----------------------------------------------

dotw_trips <- drivers_time_date %>% 
  group_by(dow) %>% 
  summarise(trips = n_distinct(ID)) %>% 
  ggplot(aes(dow, trips)) +
  geom_col() +
  labs(x = "Day of the week", y = "Trips") +
  scale_y_continuous(breaks = seq(0,150,10), minor_breaks = NULL)

# Distance per hour of the day --------------------------------------------

drivers_time_date <- drivers_time_date %>% 
  mutate(IS_AM = as.factor(IS_AM),
         hotd = as.double(hotd),
         hotd_ampm = case_when(hotd == 0 ~ hotd + 12,
                               hotd >= 1 & hotd < 13 ~ hotd,
                               hotd >= 13 ~ hotd - 12,
                               TRUE ~ NA_real_))

hotd_dist <- drivers_time_date %>% 
  group_by(hotd) %>% 
  summarise(DISTANCE = sum(DISTANCE)) %>% 
  ggplot(aes(hotd, DISTANCE)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0,23,1), minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = "Hour of the day", y = "Traveled distance (km)")
  
# Trips per hour of the day -----------------------------------------------

hotd_trip <- drivers_time_date %>% 
  group_by(ID) %>% 
  summarise(start = min(hotd)) %>% 
  group_by(start) %>% 
  summarise(n = n_distinct(ID)) %>% 
  ggplot(aes(start, n)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0,23,1), minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = "Trip start hour", y = "Traveled distance (km)")

# Speeding per hour of the day --------------------------------------------

drivers_time_date <- drivers_time_date %>% 
  mutate(IS_EXPOSURE = if_else(limite_vel - SPD_KMH < 10, TRUE, FALSE),
         IS_SPEEDING = if_else(SPD_KMH - limite_vel > 5, TRUE, FALSE))

hotd_sp <- drivers_time_date %>% 
  filter(IS_EXPOSURE == TRUE) %>% 
  group_by(hotd, IS_SPEEDING) %>% 
  summarise(DISTANCE = sum(DISTANCE)) %>% 
  pivot_wider(names_from = IS_SPEEDING, values_from = DISTANCE) %>% 
  mutate(SPEEDING = `TRUE` / (`TRUE` + `FALSE`)) %>% 
  ggplot(aes(hotd, SPEEDING)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0,23,1), minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL, breaks = seq(0,0.6,0.1)) +
  labs(x = "Hour of the day", y = "SP")

# Speeding per day of the week --------------------------------------------

dotw_sp <- drivers_time_date %>% 
  filter(IS_EXPOSURE == TRUE) %>% 
  group_by(dow, IS_SPEEDING) %>% 
  summarise(DISTANCE = sum(DISTANCE)) %>% 
  pivot_wider(names_from = IS_SPEEDING, values_from = DISTANCE) %>% 
  mutate(SPEEDING = `TRUE` / (`TRUE` + `FALSE`)) %>% 
  ggplot(aes(x = dow, y = SPEEDING)) +
  geom_col() +
  scale_y_continuous(minor_breaks = NULL, breaks = seq(0,0.5,0.1)) +
  labs(x = "Day of the week", y = "SP")

# Richards speeding analysis -----------------------------------------------

sp_zones <- drivers_time_date %>% 
  filter(IS_EXPOSURE == TRUE) %>%
  group_by(DRIVER, ID, IS_SPEEDING) %>% 
  summarise(DISTANCE = sum(DISTANCE)) %>% 
  pivot_wider(names_from = IS_SPEEDING, values_from = DISTANCE) %>% 
  mutate(SPEEDING = `TRUE` / (`TRUE` + `FALSE`),
         SPEEDING = if_else(is.na(SPEEDING), 0, SPEEDING),
         SPEEDING_N = if_else(SPEEDING > 0, 1, 0)) %>% 
  group_by(DRIVER) %>% 
  summarise(MEAN_SP = mean(SPEEDING),
            TRIPS_SP = sum(SPEEDING_N) / n_distinct(ID),
            TRIPS = n_distinct(ID)) %>% 
  ggplot(aes(x = TRIPS_SP, y = MEAN_SP)) +
  geom_point(color = "midnightblue", alpha = 0.4, aes(size = TRIPS)) +
  geom_hline(yintercept = 0.2, linetype = 2) +
  geom_vline(xintercept = 0.2, linetype = 2) +
  annotate(geom = "text", 
           x = c(0.1, 0.1, 0.7, 0.7), 
           y = c(0.7, 0.1, 0.1, 0.7), 
           label = c("Situational\nspeeding", "Incidental\nspeeding", 
                     "Casual\nspeeding", "Habitual\nspeeding"),
           size = 3) +
  scale_x_continuous(limits = c(0,1), minor_breaks = NULL, 
                     breaks = seq(0,1,0.2)) +
  scale_y_continuous(limits = c(0,1), minor_breaks = NULL, 
                     breaks = seq(0,1,0.2)) +
  labs(
    x = "Percent of trips with any speeding",
    y = "Average speeding per trip"
  ) +
  theme(legend.position = "none")

# Saving plots ------------------------------------------------------------

plots <- list(dotw_dist, dotw_trips, dotw_sp, hotd_dist, hotd_sp, 
              hotd_trip, sp_zones) 

names <- c("dotw_dist", "dotw_trips", "dotw_sp", "hotd_dist", "hotd_sp",
           "hotd_trip", "sp_zones")

names <- glue("{output03}{names}.pdf")

save_plots <- function(plots, names) {
  ggsave(names, plots, device = "pdf", dpi = 300, width = 6, height = 3.5)
}

map2(plots, names, save_plots)

rm(drivers_lines_sf)
rm(drivers_time_date)
