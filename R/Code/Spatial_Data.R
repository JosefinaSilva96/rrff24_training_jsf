# Reproducible Research Fundamentals 
# Spatial Data

# Libraries
install.packages(c("sf",
                   "leaflet",
                   "geosphere"),

library(tidyverse)
library(sf) # Simple features
library(leaflet) # Interactive map
library(geosphere) # Great circle distances                dependencies = TRUE)

# Load data 
city_sf<- st_read("https://raw.githubusercontent.com/worldbank/dime-r-training/refs/heads/main/DataWork/DataSets/Final/city.geojson")
head(city_sf)
st_crs(city_sf)   
names(city_sf)

ggplot() +
    geom_sf(data = city_sf)

langata_sf <- city_sf %>%
    filter(NAME_2 == "Langata")


ggplot() +
    geom_sf(data = langata_sf)

schools_sf <- st_as_sf(schools_df,
                       coords = c("longitude", "latitude"),
                       crs = 4326)
schools_df <-
    read_csv("https://raw.githubusercontent.com/worldbank/dime-r-training/refs/heads/main/DataWork/DataSets/Final/schools.csv")

head(schools_df)

schools_sf <- st_as_sf(schools_df,
                       coords = c("longitude", "latitude"),
                       crs = 4326)
head(schools_sf$geometry)

ggplot() +
    geom_sf(data = schools_sf)

ggplot() +
    geom_point(data = schools_df,
               aes(x = longitude,
                   y = latitude))
# Adding a variable with squared km

city_sf <- city_sf %>%
    mutate(area_m = city_sf %>% st_area() %>% as.numeric(),
           area_km = area_m / 1000^2)
# Plotting
ggplot() +
    geom_sf(data = city_sf,
            aes(fill = area_km)) +
    labs(fill = "Area") +
    scale_fill_distiller(palette = "Blues") +
    theme_void()

ggplot() +
    geom_sf(data = city_sf,
            aes(fill = area_km)) +
    geom_sf(data = schools_sf,
            aes(color = "Schools")) +
    labs(fill = "Area",
         color = NULL) +
    scale_fill_distiller(palette = "Blues") +
    scale_color_manual(values = "black") +
    theme_void()


ggplot() +
    geom_sf(data = roads_sf,
            aes(color = highway)) +
    theme_void() +
    labs(color = "Road Type")


ggplot() +
    geom_sf(data = roads_sf,
            aes(color = highway)) +
    theme_void() +
    labs(color = "Road Type")

leaflet() %>%
    addTiles() %>%
    addPolygons(data = city_sf)

leaflet() %>%
    addTiles() %>%
    addPolygons(data = city_sf,
                popup = ~NAME_2)

leaflet() %>%
    addTiles() %>%
    addPolygons(data = city_sf,
                popup = ~NAME_2) %>%
    addCircles(data = schools_sf,
               popup = ~name,
               color = "black")



