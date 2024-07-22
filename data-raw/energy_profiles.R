library(dplyr)

energy_profiles <- readxl::read_excel(path = "data-raw/energy_profiles.xlsx") %>%
  mutate(
    datetime = lubridate::with_tz(datetime, "Europe/Amsterdam"),
    building = building*7,
    solar = solar*7000
    # heatpump = heatpump*10000
  ) %>%
  mutate(
    price_exported = 0.025,
    price_turn_down = ifelse(price_turn_down < 0, 0, price_turn_down),
    price_turn_up = ifelse(price_turn_up < 0, 0, price_turn_up)
  ) %>%
  select(-heatpump)
  # dutils::decrease_timeseries_resolution(60, "average")
# energy_profiles %>% dutils::dyplot()
usethis::use_data(energy_profiles, overwrite = TRUE)
