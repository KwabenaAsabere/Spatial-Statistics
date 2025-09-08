library(tidyverse)
library(broom)
library(janitor)
library(gt)
library(gtsummary)
theme_set(theme_bw())

library(sf)
library(terra)

philly_sf <- read_rds("philadelphia_tracts.rds")

st_crs(philly_sf)
st_crs(philly_sf) <- sf::st_crs(4326)

st_geometry(philly_sf)

philly_geo <- st_geometry(philly_sf)

philly_sf %>% 
ggplot()+
geom_sf(color = "red")


st_crs(philly_sf)

philly_sf <- st_transform(philly_sf,crs = 4326)

st_geometry(philly_sf)

philly_geo[[1]]

st_bbox(philly_geo)

bike_lanes <- st_read("Bike_Network.shp")


ggplot()+
geom_sf(data = philly_sf,color = NA)+
geom_sf(data = bike_lanes,color = "red")


st_geometry(bike_lanes)

philly_crimes <- read_csv("philly_crimes.csv")
philly_crimes <- philly_crimes %>%
  filter(!is.na(lat), !is.na(lng)) %>%
  filter(lat > 38 & lat < 41,    # Philadelphia bounding box
         lng < -70 & lng > -76)  # longitude should be west of 0


crimes2 <-read_csv("crime2.csv")
class(crimes2)

crimes <- st_as_sf(crimes2,coords = c("Lon","Lat"),crs = 4326)

# st_geometry_type(crimes)

st_crs(crimes)

ggplot()+
geom_sf(data = philly_sf,fill = "grey95")+
geom_sf(data = crimes,color = "red")

ggplot() +
  geom_sf(data = philly_sf, fill = "gray95", color = "white") +
  geom_sf(data = crimes, color = "green", alpha = 0.4, size = 0.5) +
  theme_minimal() +
  labs(title = "Philadelphia Crime Locations")



philly_crimes <- st_as_sf(philly_crimes,coords = c("lng","lat"),crs = 4326)
st_crs(philly_crimes)
st_geometry(philly_crimes)


philly_joined <- st_join(philly_sf,philly_crimes)
View(philly_joined)

crime_counts <- philly_joined %>% 
group_by(GEOID10) %>% 
summarise(n = n())

philly_sf <- left_join(philly_sf,
          st_drop_geometry(crime_counts),
          by = "GEOID10"
          )
sum(is.na(philly_sf$n))

ggplot(philly_sf)+
  geom_sf(aes(fill = n))+
  scale_fill_viridis_c(option = "viridis",na.value = "white")



recent_crimes <- philly_crimes |> 
  filter(dispatch_date >= as.Date("2025-07-01"))

ggplot()+
  geom_sf(data = philly_sf, fill = "grey90")+
  geom_sf(data = recent_crimes,color = "steelblue",alpha = 0.5,size = 0.3)+
  labs(title = "Crimes Since July 2025")


ggplot()+
  geom_sf(data = philly_sf,color = NA)+
  stat_density_2d(
    data = st_coordinates(philly_crimes) |> 
      as.data.frame(),
  aes(x = X, y= Y, fill = after_stat(level)),
    geom = "polygon",alpha = 0.4
  )+
  scale_fill_viridis_c(option = "inferno",direction = -1)+
  theme_void()+
  labs(title = "Crime Density in Philadelphia",
       fill = "Density Level")


homicide <- philly_crimes |> 
  filter(text_general_code == "Homicide - Criminal")

fraud <- philly_crimes |> 
  filter(text_general_code == "Fraud")

ggplot()+
  geom_sf(data = philly_sf,fill = "gray95",color = "white")+
  geom_sf(data = homicide, color = "red")+
  geom_sf(data = fraud,color = "blue",size = 0.8,alpha = 0.4)


ggplot()+
  geom_sf(data = philly_sf,fill = "gray95",color = "white")+
  geom_sf(data = fraud,color = "blue",size = 0.5,alpha = 0.4)



st_coordinates(philly_crimes) |> 
  as.data.frame()

# dists <- st_distance(recent_crimes,recent_crimes)
rm(dists)

# Rasta Data --------------------------------------------------------------

library(terra)
library(raster)

library(datasets)
class(volcano)
volcano

volcano_raster <- rast(volcano)
volcano_raster

volcano_df <- as.data.frame(volcano_raster,xy = TRUE)
names(volcano_df)[3] <- "elevation"

volcano_df |> 
  ggplot()+
  geom_raster(aes(x = x,y = y,fill = elevation))+
  scale_fill_viridis_c()+
  coord_equal()+
  labs(title = "Volcano Elevation Map",fill = "Elevation")


slope <- terrain(volcano_raster,"slope",unit = "degree")
plot(slope)


ggplot(volcano_df, aes(x, y, z = elevation)) +
  geom_contour_filled()

slope_df <- as.data.frame(slope,xy = TRUE)
colnames(slope_df)[3] <- "slope"

ggplot(slope_df, aes(x = x, y = y, fill = slope)) +
  geom_raster() +
  scale_fill_viridis_c(option = "inferno", name = "Slope (°)") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Terrain Slope from Volcano Data",
       x = "X", y = "Y")



# Spatial Hypothesis Testing ----------------------------------------------

nyc <- st_read("Counties.shp")
nys_lyme_data <- read_rds("nys_lyme_data.rds")

nys_lyme_data <- nys_lyme_data |> 
  filter(!is.na(Lyme.Incidence.Rate))

ggplot()+
  geom_sf(data = nyc,color = "black",fill = "gray95")+
  theme_minimal()+
  labs(title = 'New York City Counties')


nys_joined <-  left_join(nyc,st_drop_geometry(nys_lyme_data), by = c("NAME","FIPS_CODE"))

nys_joined<- nys_joined |> 
  filter(!is.na(Lyme.Incidence.Rate))

ggplot()+
  geom_sf(data = nys_joined,color = "black",aes(fill = Lyme.Incidence.Rate))+
  theme_minimal()+
  scale_fill_viridis_c()+
  labs(title = 'New York City Counties')


st_geometry(nys_joined)

library(tmap)
tmap_mode("view")
tm_shape(nys_joined)+
  tm_polygons("Lyme.Incidence.Rate")


nys_joined[["Lyme.Incidence.Rate"]]

summary(nys_joined[["Lyme.Incidence.Rate"]])

nys_joined_clean <- nys_joined |> 
  clean_names()

colnames(nys_joined_clean)

nys_joined_clean |> 
  ggplot(aes(x = lyme_incidence_rate))+
  geom_histogram(color = "white")


nys_joined_clean |> 
  ggplot(aes(x = log(lyme_incidence_rate)))+
  geom_histogram(color = "white")


library(spdep)

lyme_nb <- poly2nb(nys_joined_clean,queen = TRUE)

str(lyme_nb)

lyme_nb |> pluck(1)

neighbors_df <- tibble(
  polygon_id = seq_along(lyme_nb),
  neighbors = map_chr(lyme_nb,toString)
)

neighbors_df2 <- tibble(
  polygon_id = seq_along(lyme_nb),
  name = nys_joined_clean[['name']],
  neighbors = map(lyme_nb, identity)  # or simply: as.list(lyme_nb)
)



neighbors_df3 <- tibble(
  polygon_id = seq_along(lyme_nb),
  neighbors = map(lyme_nb,as.list)  # or simply: as.list(lyme_nb)
)

print(neighbors_df2)

nys_joined_clean <-  nys_joined_clean |> 
  left_join(neighbors_df2, by = "name")

glimpse(nys_joined_clean)


ggplot(nys_joined_clean) +
  geom_sf(aes(fill = lyme_incidence_rate)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(title = "Lyme Disease Incidence (per 100,000)",
       fill = "Rate") +
  theme_minimal()


nys_joined_clean %>%
  mutate(n_neighbors = map_int(neighbors, length)) %>%
  ggplot(aes(x = n_neighbors)) +
  geom_histogram(binwidth = 1, fill = "skyblue",color = "black") +
  labs(title = "Distribution of Neighbor Counts per County")


centroids <- st_centroid(nys_joined_clean)
colnames(centroids)

nys_joined_clean <- nys_joined_clean %>%
  mutate(n_neighbors = map_int(neighbors, length))

nb_obj <- nys_joined_clean[["neighbors"]]

nb_obj
# spatial weights
lw_obj <- nb2listw(lyme_nb,style = "W")
glimpse(lw_obj)

nys_joined_clean <- nys_joined_clean |> 
  mutate(lag_lyme = lag.listw(lw_obj,lyme_incidence_rate))

str(nb_obj)



ggplot(nys_joined_clean) +
  geom_sf(aes(fill = lag_lyme)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  labs(title = "Spatial Lag of Lyme Disease Incidence Rate",
       fill = "Lagged Rate") +
  theme_minimal()




lyme_weights <- lw_obj
str(lyme_weights)

lyme_weights |> pluck("weights") 



nb_list <- unclass(lyme_weights$neighbours)  
wt_list <- lyme_weights$weights

neighbors_long <- tibble(
  polygon_id = seq_along(nb_list),
  neighbor   = nb_list,
  weight     = wt_list
) |>
  unnest(c(neighbor, weight))

print(neighbors_long)

glimpse(neighbors_long)


centroids <- st_centroid(nys_joined_clean)

centroids_df <- centroids |>
  mutate(polygon_id = row_number()) |>
  dplyr::select(polygon_id, geometry) |> 
  st_coordinates() |>
  as_tibble() |>
  bind_cols(polygon_id = 1:nrow(centroids))

colnames(centroids)
class(centroids)

centroids_df


lyme_nb
lyme_weights
lyme_incidence <- nys_joined_clean[["lyme_incidence_rate"]] |> 
  map_dbl(log) |> tibble()

moran.test(lyme_incidence,lyme_weights,alternative = "greater")
local_moran <- localmoran(lyme_incidence,lyme_weights,alternative = "greater")
local_moran[,5]


local_moran_df <- as_tibble(local_moran) |> 
  rename(
    Ii = `Ii`,
    EIi = `E.Ii`,
    VarIi = `Var.Ii`,
    ZIi = `Z.Ii`,
    p_value = `Pr(z > E(Ii))`
  )

nys_joined_clean <- bind_cols(nys_joined_clean, local_moran_df)

local_moran_df

lyme_lisa <- local_moran
names(lyme_lisa)


lyme_incidence <- tibble(log_incidence = log(nys_joined_clean$lyme_incidence_rate))
lyme_incidence

lyme_log_incidence <-  log(nys_joined_clean[["lyme_incidence_rate"]])

moran.test(lyme_log_incidence,lyme_weights,alternative = "greater")
local_moran <- localmoran(lyme_log_incidence,lyme_weights,alternative = "greater")

local_moran
names(local_moran)

lyme_log_incidence



local_moran_df <- as_tibble(local_moran) |> 
  rename(
    Ii = `Ii`,
    EIi = `E.Ii`,
    VarIi = `Var.Ii`,
    ZIi = `Z.Ii`,
    p_value = `Pr(z > E(Ii))`
  )


local_moran_df


MC <- moran.mc(log_incidence,lyme_weights,nsim = 999,
               alternative = "greater")

MC
plot(MC)




































































