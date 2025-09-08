library(tidyverse)
library(tidymodels)
library(sf)
library(spdep)
library(terra)
library(tmap)
library(SpatialEpi)
library(INLA)
library(leaflet)
library(viridis)
library(finalfit)
theme_set(theme_bw())

burkina_map <- read_rds("burkina/GADM_2.8_BFA_adm0.rds")
class(burkina_map)
burkina_outline <- st_as_sf(burkina_map)

ggplot(burkina_sf) +
    geom_sf()

burkina_provinces <- st_read("burkina/gadm36_BFA_2.shp")
ggplot(burkina_provinces) +
    geom_sf(aes(fill = NAME_1))

burkina_regions <- read_rds("burkina/GADM_2.8_BFA_adm1.rds")
burkina_regions <- st_as_sf(burkina_regions)

ggplot(burkina_regions) +
    geom_sf(aes(fill = NAME_1))


burkina_malaria <- read_csv("burkina/BF_malaria_data.csv")
burkina_malaria_sf <- st_as_sf(
    burkina_malaria,
    coords = c("longitude", "latitude"),
    crs = 4326
)

ggplot() +
    geom_sf(data = burkina_regions) +
    geom_sf(data = burkina_malaria_sf)

bf_landuse <- rast("burkina/BF_land_use.tif")
plot(bf_landuse)

bf_waterbody <- st_read("burkina/BFA_water_areas_dcw.shp")
ggplot() +
    geom_sf(data = burkina_regions, fill = "grey95") +
    geom_sf(data = bf_waterbody)
