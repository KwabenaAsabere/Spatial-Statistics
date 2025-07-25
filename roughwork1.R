library(sf)
shp_data <- st_read("rongelap.shp")
plot(shp)

ggplot(data = shp_data) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Map from Shapefile")



ggplot(shp_data) +
  geom_sf(aes(color = val)) +  # or aes(fill = val) if it's a polygon
  scale_color_viridis_c() +
  theme_bw() +
  labs(title = "Spatial Plot Colored by 'val'")


ggplot(shp_data) +
  geom_sf(aes(color = variable, fill = variable), alpha = 0.6) +
  facet_wrap(~ variable) +
  scale_color_viridis_d() +
  theme_minimal()


st_geometry_type(shp_data)


# Core
library(sf)
library(terra)

# Spatial weights and modeling
library(spdep)
library(spatialreg)
library(gstat)

# Visualization
library(tmap)
library(ggplot2)

# Optional extras
library(leaflet)
library(spatstat)
library(CARBayes)







































