gambia <- read_rds("gambia_raster.rds")
gambia_map <- read_rds("gambia_map.rds")
class(gambia_map)

gambia <- rast(gambia)
gambia_vect <- vect(gambia_map)

ggplot(gambia_map)+
  geom_sf()

terra::extract(gambia,gambia_vect)

vals <- terra::extract(gambia,gambia_vect,fun = mean,na.rm = TRUE)
vals
gambia_map <- gambia_map |> 
  mutate(values = vals$GMB_elv_msk)

ggplot(gambia_map,aes(fill =values))+
  geom_sf()+
  scale_fill_viridis_c(name = "Mean value") +
  labs(title = "Mean raster values per district") +
  theme_minimal()


vals <- st_as_sf(terra::extract(gambia,gambia_vect,fun = mean,na.rm = TRUE,bind = TRUE))

ggplot(vals) +
  geom_sf(aes(fill = GMB_elv_msk)) +
  scale_fill_viridis_c(name = "Mean value") +
  labs(title = "Mean raster values per district") +
  theme_minimal()


africa_tb <- read_rds("africa_tb_data.rds")
head(africa_tb)

load("tb/afrilandcover.rda")

class(afrilandcover)

afrilandcover
plot(africa)
africa <- read_rds("tb/africa_gadm.rds")
africa
class(africa)




load("tb/africountries.rda")
class(africountries)
ggplot(africountries)+
  geom_sf()

africountries_vect <- vect(africountries)
afrilandcover_rast <- terra::rast(afrilandcover)

detach("package:raster", unload = TRUE)
library(terra)
afrilandcover_rast <- rast(afrilandcover)



africa_vals <- st_as_sf(terra::extract(afrilandcover,africountries_vect,
                                       fun = mean,na.rm = TRUE))

trainDat
predictor_names <-  names(predictors)
predictor_names

split <- initial_split(trainDat, prop = 0.8)
trainDat_train <- training(split)
trainDat_test <- testing(split)

formula_catboost <- as.formula(paste0("temp ~",paste(predictor_names,collapse = "+")))
print(formula_catboost)

catboost_recipe <- recipe(formula_catboost,data = trainDat_train)

catboost_spec <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune()
  
) |> 
  set_engine(engine = "catboost") |> 
  set_mode(mode = "regression")


grid_regular <- grid_regular(
  trees(range = c(500, 2000)),
  learn_rate(range = c(-3, -0.5)),   # log10 scale
  tree_depth(range = c(4, 10)),
  sample_size = sample_prop()
)

set.seed(123)
grid_latin <- grid_latin_hypercube(
  trees(range = c(500, 2000)),
  learn_rate(range = c(-3, -0.5)),
  tree_depth(),
  sample_size = sample_prop(),
  size = 20
)

grid_random <- grid_random(
  trees(),
  learn_rate(range = c(-3, -0.5)), 
  tree_depth(),
  sample_size = sample_prop(),
  size = 20   # 20 random combinations
)

catboost_grid <- grid_space_filling(trees(range = c(500, 2000)),
                                    learn_rate(range = c(-3, -0.5)),   # log10 scale
                                    tree_depth(range = c(4, 10))
                                    )


catboost_wf <- workflow() |> 
  add_recipe(recipe = catboost_recipe) |> 
  add_model(catboost_spec)


catboost_blockfolds <- spatialsample::spatial_block_cv(trainDat, v = 5)
spatialsample::autoplot(catboost_blockfolds)

catboost_controls <- tune::control_resamples(save_pred = TRUE,
                                             save_workflow = TRUE)

library(finetune)
catboost_tuning <- tune_grid(
  catboost_wf,
  resamples = catboost_blockfolds,
  grid = catboost_grid,
  control = catboost_controls
)


catboost_tuning |> 
  collect_metrics() 

final_model <- fit_best(catboost_tuning)
final_model
















