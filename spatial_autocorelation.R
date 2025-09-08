library(tidyverse)
library(tidymodels)
library(sf)
library(spdep)
library(terra)
library(tmap)
theme_set(theme_bw())


ny_counties <- read_sf("Counties.shp")

ggplot(ny_counties) +
  geom_sf(aes(fill = POP2020)) +
  scale_fill_viridis_c()


nys_lyme_data <- read_rds("nys_lyme_data.rds")


ggplot(nys_lyme_data) +
  geom_sf(aes(fill = Lyme.Incidence.Rate)) +
  scale_fill_viridis_c()


colombia_malaria <- read_rds("colombia_malaria.rds")
class(colombia_malaria)

nb <- poly2nb(colombia_malaria, queen = TRUE)
nbw <- nb2listw(nb, style = "W", zero.policy = TRUE)

gmoranMC <- moran.mc(colombia_malaria$log_incidence, nbw, nsim = 999)

gmoranMC
names(gmoranMC)

gmoranMC_df <- tibble(
  simulated_I = gmoranMC$res
)

gmoranMC_df |>
  ggplot(aes(x = simulated_I)) +
  geom_histogram(fill = "steelblue", color = "white") +
  geom_vline(
    xintercept = gmoranMC$statistic,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  )


x <- colombia_malaria$log_incidence
wx <- lag.listw(x = nbw, var = x, zero.policy = TRUE)
wx

moran_df <- tibble(
  x = x,
  wx = wx
)
moran_df

moran_df |>
  ggplot(aes(x = x, y = wx)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_vline(xintercept = mean(x, na.rm = TRUE), linetype = "dashed") +
  geom_hline(yintercept = mean(wx, na.rm = TRUE), linetype = "dashed") +
  labs(
    x = "Log Incidence rate",
    y = "Spatially lagged log incidence",
    title = "Moran Scatterplot of Malaria Incidence"
  )

lmoran <- localmoran(colombia_malaria$log_incidence, nbw)
head(lmoran)

colombia_malaria <- colombia_malaria |>
  mutate(
    lmI = lmoran[, 1],
    lmZ = lmoran[, 4],
    lmp = lmoran[, 5]
  )

x <- colombia_malaria$log_incidence
wx <- lag.listw(
  x = nbw,
  var = colombia_malaria$log_incidence,
  zero.policy = TRUE
)

mx <- mean(x, na.rm = TRUE)
mwx <- mean(wx, na.rm = TRUE)

colombia_malaria <- colombia_malaria |>
  mutate(
    quadrant = case_when(
      x >= mx & wx >= mwx & lmp <= 0.05 ~ "High-High",
      x <= mx & wx <= mwx & lmp <= 0.05 ~ "Low-Low",
      x >= mx & wx <= mwx & lmp <= 0.05 ~ "High-Low",
      x <= mx & wx >= mwx & lmp <= 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    ),
    spatial_lag = wx
  )

colombia_malaria |>
  ggplot(aes(x = log_incidence, y = spatial_lag, color = quadrant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  geom_vline(xintercept = mx, linetype = "dashed") +
  geom_hline(yintercept = mwx, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "pink",
      "Low-High" = "skyblue",
      "Not significant" = "grey70"
    )
  )


tmap_mode("plot")
tm_shape(colombia_malaria) +
  tm_polygons(
    fill = "quadrant",
    fill.scale = tm_scale_categorical(
      values = c("red", "blue", "lightpink", "skyblue2", "white"),
      labels = c(
        "High-High",
        "Low-Low",
        "High-Low",
        "Low-High",
        "Non-significant"
      )
    )
  ) +
  tm_title(text = "LISA Cluster Map of Malaria in Colombia", fontface = "bold")


x <- as.vector(scale(colombia_malaria$log_incidence))
wx <- lag.listw(x = nbw, var = x, zero.policy = TRUE)

mp_df <- tibble(
  x = x,
  wx = wx
)

fit <- lm(wx ~ x, data = mp_df)
aug_fit <- augment(fit)

tidy(fit)

aug_fit
