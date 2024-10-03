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
leaflet() %>%
    addTiles() %>%
    addPolylines(data = roads_sf)

schools_utm_sf <- st_transform(schools_sf, 32632)
schools_utm_sf$geometry %>% head(2) %>% print()

schools_1km_sf <- schools_sf %>%
    st_buffer(dist = 1000) # Units are in meters. Thanks s2!
ggplot() +
    geom_sf(data = schools_1km_sf)

city_1_sf <- city_sf %>%
    group_by(NAME_1) %>%
    summarise(geometry = st_combine(geometry)) %>%
    ungroup()
ggplot() +
    geom_sf(data = city_1_sf)

roads_sf %>%
    filter(highway == "trunk") %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_buffer(dist = 10)

schools_chull2_sf <- schools_sf %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_convex_hull()
ggplot() +
    geom_sf(data = schools_chull2_sf) +
    geom_sf(data = schools_sf, color = "blue")

city_c_sf <- st_centroid(city_sf)

ggplot() +
    geom_sf(data = city_c_sf)

city_sf %>%
    st_drop_geometry() %>%
    head()

schools_sf %>%
    st_bbox()

motor_sf <- roads_sf %>%
    filter(highway == "motorway")
# Matrix: distance of each school to each motorway
dist_mat <- st_distance(schools_sf, motor_sf)
# Take minimun distance for each school
dist_mat %>% apply(1, min) %>% head()


#DATA TABLE

install.packages("data.table")

# latest development version (only if newer available)
data.table::update_dev_pkg()

# latest development version (force install)
install.packages("data.table", repos="https://rdatatable.gitlab.io/data.table")

# load the package
library(data.table)

# load a built-in dataset
data("iris")

# print the first 5 rows
head(iris)

# transform the data.frame into a tibble
data_frame <- tibble::as.tibble(iris)

# print the first 5 rows
head(data_frame)

# transform the data.frame into a tibble
data_table <- data.table::as.data.table(iris)

# print the first 5 rows
head(data_table)

# show the class of the object
class(data_table)# 1 create a data.table from scratch
data <- data.table(x = 1:100)

# 2 load a data.table from a file (csv, txt, zip and URLs)
data <- fread("data.csv") #best format you can use EVER

# 3 coerce an existing data.frame into a data.table
data_table <- as.data.table(data_frame)

# 4 coerce an existing data.frame into a data.table by "reference"; i.e. you don't have to (re)assing it 
setDT(data_frame)

data_table <- as.data.table(data_frame)

setDT(data_frame)

data_table[1:3, ] # select the first 3 rows

# select rows where Species equals setosa
data_table[Species == "setosa", ]

# we can also avoid to type the comma when we only have a subsetting
data_table[Species == "setosa"  ] 

# select rows where Species equals setosa and Sepal.Length > 5
data_table[Species == "setosa" &
               Sepal.Length >= 5, ] 

data_table[Species == "setosa", Treatment := TRUE][sample(.N, 5), c(5, 10)]


data_table[, Sepal.Length.mean := mean(Sepal.Length), by = Species][, c(5, 6)]
install.packages("arrow")
library(arrow)
library(data.table)

# Generate large data (10000000 milion rows)
n <- 1e8
df <- data.frame(
    id = 1:n,
    value = rnorm(n),
    logical_col = c(TRUE, FALSE),
    date_col = as.Date("1996-06-12") + 0:4,
    factor_col = factor(c("low", "medium", "high", "medium", "low"))
)

