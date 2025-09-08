library(tidyverse)
library(sf)
library(spdep)
library(terra)
theme_set(theme_bw())

nyc <- st_read("Counties.shp")

ggplot() +
  geom_sf(data = nyc, fill = "grey95", color = "blue") +
  labs(title = " New York State Counties")
View(nyc)

nys_lyme_data <- read_rds("nys_lyme_data.rds")
nys_lyme_data
glimpse(nys_lyme_data)

ggplot() +
  geom_sf(data = nys_lyme_data, aes(fill = Lyme.Incidence.Rate)) +
  scale_fill_viridis_c(guide = "none") +
  labs(title = " New York State Counties")

nys_lyme_data <- nys_lyme_data |>
  filter(!is.na(Lyme.Incidence.Rate))

lyme_nb <- poly2nb(nys_lyme_data, queen = TRUE)
lyme_nb

str(lyme_nb)
