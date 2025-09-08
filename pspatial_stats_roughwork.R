library(sf)
library(tidyverse)
library(tmap)
library(gt)
library(tidymodels)
library(terra)
library(leaflet)
library(spdep)
theme_set(theme_bw())

ghana_districts <- st_read("ghs/DiseaseCoverage_ByDistricts.shp")

glimpse(ghana_districts)


ghana_districts <- ghana_districts |>
  mutate(across(c(BCG:MR2), as.numeric))

ghana_regions <- st_read("ghs/DiseaseCoverage_ByRegions.shp")
ghana_subdistricts <- st_read("ghs/GHS_Sub-district_Shapefiles_1.shp")
ggplot() +
  geom_sf(data = ghana_districts, aes(fill = BCG)) +
  scale_fill_viridis_c()


ggplot() +
  geom_sf(data = ghana_districts, aes(fill = YellowFeve)) +
  scale_fill_viridis_c()


ggplot() +
  geom_sf(data = ghana_regions, aes(fill = Population)) +
  scale_fill_viridis_c()

ggplot() +
  geom_sf(data = ghana_subdistricts)


load("open/nz.RDA")
