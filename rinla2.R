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
gambia_sf <- read_rds("gambia_sf.rds")
gm_sf <- read_rds("gambia_map.rds")

head(gambia_sf)
glimpse(gambia_sf)
missing_glimpse(gambia_sf)

gambia_sf_utm <- st_transform(gambia_sf, 32628)
coords <- st_coordinates(gambia_sf_utm)
coords
mesh <- inla.mesh.2d(loc = coords, max.edge = c(5000, 20000), cutoff = 1000)

mesh$n
plot(mesh)
points(coords, col = "red")

## build the SPDE model
spde <- inla.spde2.matern(mesh = mesh, alpha = 2)

s.index <- inla.spde.make.index("spatial", spde$n.spde)
lengths(s.index)

A <- inla.spde.make.A(mesh = mesh, loc = coords)

library(geodata)

r <- elevation_30s("GMB", path = "data/")
dp <- as.points(r)
dim(dp)

gambia_raster <- read_rds("gambia_raster.rds")
gambia_raster <- unwrap(gambia_raster)
plot(gambia_raster)

dim(dp)
ra <- aggregate(r, fact = 5, fun = mean)
dp <- as.points(ra)
dim(dp)
dp

coop <- geom(dp)[, c("x", "y")]
class(coop)
coop

dp_df <- cbind(as.data.frame(dp), geom(dp)[, c("x", "y")])
head(dp_df)

Ap <- inla.spde.make.A(mesh = mesh, loc = coop)

stk.e <- inla.stack(
    tag = "est",
    data = list(y = gambia_sf$positive, Ntrials = gambia_sf$total),
    A = list(1, A),
    effects = list(
        data.frame(intercept = 1, altitude = gambia_sf$alt),
        spatial = s.index$spatial
    )
)

stack_pred <- inla.stack(
    tag = "pred",
    data = list(y = NA, Ntrials = NA),
    A = list(1, Ap),
    effects = list(
        data.frame(intercept = 1, altitude = dp_df$GMB_elv_msk),
        spatial = s.index$spatial
    )
)

stack_full <- inla.stack(stk.e, stack_pred)

formula <- y ~ 0 + intercept + altitude + f(spatial, model = spde)

res <- inla(
    formula,
    family = "binomial",
    Ntrials = Ntrials,
    control.family = list(link = "logit"),
    data = inla.stack.data(stack_full),
    control.predictor = list(
        compute = TRUE,
        link = 1,
        A = inla.stack.A(stack_full)
    )
)

summary(res)

index <- inla.stack.index(stack = stack_full, tag = "pred")$data

prev_mean <- res$summary.fitted.values[index, "mean"]
prev_ll <- res$summary.fitted.values[index, "0.025quant"]
prev_ul <- res$summary.fitted.values[index, "0.975quant"]

pred_df <- tibble(
    x = coop[, 1],
    y = coop[, 2],
    prev_mean = prev_mean,
    LL = prev_ll,
    UL = prev_ul
)

head(pred_df)

pred_vect = vect(pred_df, geom = c("x", "y"), crs = "EPSG:4326")
plot(pred_vect)

r_prev_mean = rasterize(x = pred_vect, y = ra, field = "prev_mean", fun = mean)
r_prev_mean
plot(r_prev_mean)

r_LL <- rasterize(x = pred_vect, y = ra, field = "LL", fun = mean)
plot(r_LL)

r_UL <- rasterize(x = pred_vect, y = ra, field = "UL", fun = mean)
plot(r_UL)

pred_points <- st_as_sf(pred_df, coords = c("x", "y"), crs = 4326)

pred_points$prevalence <- plogis(pred_points$prev_mean)

head(pred_points)

gambia_map <- read_rds("gambia_map.rds")

ggplot() +
    geom_sf(data = gambia_map) +
    geom_sf(data = pred_points, aes(color = prevalence)) +
    scale_color_viridis()
