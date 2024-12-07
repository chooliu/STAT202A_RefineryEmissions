# Data Access - Colorado Climate Center ========================================
# https://climate.colostate.edu/data_access_new.html
# from three different weather stations 

# parameters
# Maximum Temperature
# Minimum Temperature
# Precipitation
# Snowfall

# Station Name: DENVER-CENTRAL PARK
# Station ID: 052220
# Longitude: -104.86948 Latitude: 39.76746
# Elevation: 5284 ft.
# Max Temperature: 1948-01-01 - 2024-11-05
# Min Temperature: 1948-01-01 - 2024-11-05
# Precipitation: 1948-01-01 - 2024-11-05
# Snowfall: 1948-01-01 - 2024-11-05

# Station Name: DENVER MUSEUM
# Station ID: 052228
# Longitude: -104.9425 Latitude: 39.7489
# Elevation: 5307 ft.
# Max Temperature: 2012-11-01 - 2017-12-05
# Min Temperature: 2012-11-01 - 2017-12-05
# Precipitation: 2010-05-01 - 2018-10-15
# Snowfall: 2010-05-01 - 2018-10-15

# Station Metadata
# Station Name: DENVER WSO CITY
# Station ID: 052225
# Longitude: -104.98333 Latitude: 39.75
# Elevation: 5325 ft.
# Max Temperature: 1872-01-01 - 1974-03-31
# Min Temperature: 1872-01-01 - 1974-03-31
# Precipitation: 1872-01-01 - 1974-03-31
# Snowfall: 1874-02-01 - 1973-12-31



# load data --------------------------------------------------------------------
weather <-
  list.files("Data/WeatherMonitor/", full.names = T) %>%
  map_dfr(
  ~ read_csv(.x, skip = 1,
         col_names = c("Date", "TempMax", "TempMin", "Percip", "Snow")) %>%
  mutate_at(.vars = 2:ncol(.), as.numeric)
  ) %>%
  group_by(Date) %>% summarize_all(mean, na.rm = T)



# elect to take mean and ignore station effect ---------------------------------

weather %>% head


# check ranges of variables ----------------------------------------------------

# pretty sparse, 119 to 1608
table(weather$Snow > 0)
summary(weather$Snow, na.rm = T)
density(weather$Snow, na.rm = T, from = 0, to = 5) %>% plot

# 1332 to 364
table(weather$Percip > 0)
summary(weather$Percip, na.rm = T)
density(weather$Percip, na.rm = T, from = 0, to = 5) %>% plot