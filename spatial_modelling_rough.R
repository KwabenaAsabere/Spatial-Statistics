library(sf)
library(tidyverse)
library(terra)
library(spdep)
library(spatialreg)
library(gstat)
theme_set(theme_bw())

url <- "https://byu.box.com/shared/static/dgoago3mz2itvvg2m0u5jc6fkeybyxcw.rdata
"

geo_data <- load(url(url))
lyme_data <- read_rds("nys_lyme_data.rds")

lyme_data <- lyme_data |> 
  filter(!is.na(Lyme.Incidence.Rate))

ggplot()+
  geom_sf(data = lyme_data,aes(fill = Lyme.Incidence.Rate))+
  scale_fill_viridis_c(na.value = "white")

vgm_emp <- variogram(log(Lyme.Incidence.Rate)~ 1,data = lyme_data)

plot(vgm_emp)

plot(vgm_emp, main = "Empirical Variogram of Log Incidence")
vgm_model <- fit.variogram(
  object = vgm_emp,
  model = vgm(psill = 2, model = "Exp", range = 200000, nugget = 0.8)
)


rongelap_data <- read_csv("rongelap.csv")
rongelap_sf <- st_read("rongelap.shp")

load("rongelap-outline.RDATA")
## convert data to sf
rongelap <- st_as_sf(rongelap_data, coords = c("x","y"))

ggplot()+
  geom_sf(data = rongelap_shp,fill = NA)+
  geom_sf(data = rongelap_sf,aes(color = log(val)))+
  scale_color_viridis_c(option = "inferno")



## load the meuse data

meuse <- read_csv("meuse.csv")
meause_grid <- read_csv("meuse.grid.csv")

## convert to sf type
meuse <- st_as_sf(meuse,coords = c("x","y"))
meuse_grid <- st_as_sf(meause_grid,coords = c("x","y"))

meuse <- meuse |> 
  mutate(log_zinc = log(zinc))
st_crs(meuse) <- 28992

ggplot()+
  geom_sf(data = meuse,aes(size = zinc),alpha = 0.5)


vario <- variogram(log(zinc) ~ sqrt(dist),data = meuse)

vario

vario_df <- as.data.frame(vario)
vario_df

vario_df |> 
  ggplot(aes(x = dist, y= gamma))+
  geom_point(color = "steelblue",size = 2)+
  geom_line(color = "steelblue",linewidth = 0.8)


glimpse(meuse)

vario_meuse <- variogram(
  log(zinc)~ 1,
  data = meuse,
  cutoff = 2000,
  width = 50
)

vario_meuse
vario

vario_meuse_df <- as.data.frame(vario_meuse)

ggplot(vario_meuse_df, aes(x = dist, y = gamma)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  labs(
    title = "Empirical Variogram of Log Zinc",
    x = "Distance (m)",
    y = "Semivariance"
  ) +
  theme_minimal()

st_crs(meuse)

vgm_model <- fit.variogram(
  object = vario_meuse,
  model = vgm(psill = 0.7,model = "Exp",nugget = 0.1)
)

vgm_line = variogramLine(vgm_model,maxdist = max(vario_meuse[["dist"]]))

vario_meuse_df |> 
  ggplot(aes(x = dist, y = gamma))+
  geom_point(color = "darkgreen") +
  geom_line(color = "darkgreen") +
  geom_line(data = vgm_line, aes(x = dist, y = gamma), color = "red", linewidth = 1) +
  labs(
    title = "Empirical and Fitted Variogram",
    x = "Distance (m)",
    y = "Semivariance"
  ) +
  theme_minimal()

print(vgm_model)
summary(vgm_model)


models <- c("Exp","Sph","Gau")

vgm_fits <- map(models, ~fit.variogram(
  object = vario_meuse,
  model = vgm(psill = 0.7,model = "Exp",nugget = 0.1)
))

names(vgm_fits) <- models
vgm_fits


model_lines <- imap_dfr(vgm_fits, ~ {
  variogramLine(.x, maxdist = max(vario_meuse[["dist"]])) %>%
    as.data.frame() %>%
    mutate(model = .y)
})

model_lines <- model_lines |> 
  mutate(model = factor(model))

library(ggplot2)

ggplot() +
  geom_point(data =vario_meuse_df, aes(x = dist, y = gamma), color = "black", size = 2) +
  geom_line(data = vario_meuse_df, aes(x = dist, y = gamma), color = "black") +
  geom_line(data = model_lines, aes(x = dist, y = gamma, color = model), linewidth = 1.1) +
  labs(
    title = "Empirical and Fitted Variogram Models",
    x = "Distance (m)",
    y = "Semivariance",
    color = "Model"
  ) +
  theme_minimal()

glimpse(model_lines)



ggplot() +
  geom_point(data = vario_meuse_df, aes(x = dist, y = gamma), color = "black", size = 2) +
  geom_line(data = vario_meuse_df, aes(x = dist, y = gamma), color = "black") +
  geom_line(data = model_lines, aes(x = dist, y = gamma, color = model, linetype = model), linewidth = 1.1) +
  scale_color_manual(values = c("Exp" = "red", "Gau" = "forestgreen", "Sph" = "blue")) +
  scale_linetype_manual(values = c("Exp" = "solid", "Gau" = "dashed", "Sph" = "dotdash")) +
  labs(
    title = "Empirical and Fitted Variogram Models",
    x = "Distance (m)",
    y = "Semivariance",
    color = "Model",
    linetype = "Model"
  ) +
  theme_minimal()


cv_results <- imap_dfr(vgm_fits, ~ {
  cv <- krige.cv(log_zinc ~ 1, meuse, model = .x)
  tibble(
    model = .y,
    RMSE = sqrt(mean(cv$residual^2)),
    ME = mean(cv$residual),
    MAE = mean(abs(cv$residual))
  )
})

cv_results %>% arrange(RMSE)


















































