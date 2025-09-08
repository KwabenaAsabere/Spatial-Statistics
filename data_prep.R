#preparation of data for the intro to rasters in terra workshop

library(terra)

#extent of GBR found from the Great Barrier Reef Marine Park boundary, downloaded from: https://geohub-gbrmpa.hub.arcgis.com/datasets/bac38dff14ae4ff9a1c9de5d234e26f8_30/explore

#Steps below for saving GBR MPA Boundary to 4326 lon/lat; done once
gbr_boundary_original <- vect("data/Great_Barrier_Reef_Marine_Park_Boundary_94_-8322827438515261938.gpkg")

gbr_boundary <- project(gbr_boundary_original, "EPSG:4326") #project to 4326 for the workshop

writeVector(gbr_boundary, "data/gbr_MPA_boundary.gpkg")

gbr_boundary <- vect("data/gbr_MPA_boundary.gpkg")

ext(gbr_boundary)

#sea surface temperature data for an area covering the Great Barrier Reef retrieved from the NOAA ERDDAP server: https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html
#Daily data retrieved for 2020 - 2023: there was a mass coral bleaching event in 2022 so expect to see that in the temperature signature
#Downloaded as 3 year time slices as trying to download the whole lot as one wasn't working

## File Download - run once, then saved
tmp <- tempfile()
options(timeout = 6000)

#this is a 219 MB download
download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST%5B(2020-05-31T12:00:00Z):1:(2021-05-31T12:00:00Z)%5D%5B(-10):1:(-25)%5D%5B(142):1:(155)%5D", tmp)

gbr_area_sst <- rast(tmp)

#data is inverted, so need to flip
gbr_area_sst_corrected <- flip(gbr_area_sst)

#this is a 219 MB download
download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST%5B(2021-06-01T12:00:00Z):1:(2022-05-31T12:00:00Z)%5D%5B(-10):1:(-25)%5D%5B(142):1:(155)%5D", tmp)

gbr_area_sst2 <- rast(tmp)

gbr_area_sst_corrected2 <- flip(gbr_area_sst2)

#this is a 219 MB download
download.file("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?CRW_SST%5B(2022-06-01T12:00:00Z):1:(2023-05-31T12:00:00Z)%5D%5B(-10):1:(-25)%5D%5B(142):1:(155)%5D", tmp)

gbr_area_sst3 <- rast(tmp)

gbr_area_sst_corrected3 <- flip(gbr_area_sst3)

gbr_area_sst_stack <- c(gbr_area_sst_corrected, gbr_area_sst_corrected2, gbr_area_sst_corrected3)

plot(gbr_area_sst_stack, fun = function()lines(gbr_boundary))
#looks good!

#save as cdf
writeCDF(gbr_area_sst_stack, "data/gbr_area_sst_2020_2023.nc", overwrite = TRUE)

## END download script

#read above file back in for quicker data prep operations down the track
gbr_daily_temp <- rast("data/gbr_area_sst_2020_2023.nc") |>
  subset(1, negate = TRUE) #drop the first raster - accidentally included 31 May 2020 (i.e. only 1 day - so don't want to include when averaging by month)

#save a daily tiff
names(gbr_daily_temp) <- time(gbr_daily_temp) |>
  format("%Y_%m_%d")

time(gbr_daily_temp) <- NULL
writeRaster(gbr_daily_temp, "data/gbr_daily_temp.tif", overwrite = T)

#create monthly mean temp geotiff
gbr_monthly_temp <- tapp(gbr_daily_temp, index = "yearmonths", fun = mean)

writeRaster(gbr_monthly_temp, "data/gbr_monthly_temp.tif")

mean_monthly_temp <- global(gbr_monthly_temp, mean, na.rm = TRUE)

mean_monthly_temp$yearmonth <- rownames(mean_monthly_temp)

plot(mean_monthly_temp$mean, type = "b")

#get a single day's data for the end of the time series above for use in teaching
download.file("https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.nc?CRW_SST%5B(2023-05-30T12:00:00Z):1:(2023-05-31T12:00:00Z)%5D%5B(-5):1:(-45)%5D%5B(110):1:(165)%5D", tmp)

sst_one_day <- rast(tmp) |>
  subset(2) |>
  flip()

plot(sst_one_day)

time(sst_one_day) <- NULL

#save a single rast from the daily set for data
writeRaster(sst_one_day, "data/gbr_temp_2023_05_31.tif", overwrite = T)

### Script for data prep for GBR zones ##

#data was downloaded from GBRMPA: https://geohub-gbrmpa.hub.arcgis.com/datasets/6dd0008183cc49c490f423e1b7e3ef5d_53/

gbr_zones <- vect("data/Great_Barrier_Reef_Marine_Park_Zoning_20_4418126048110066699.gpkg")

sort(unique(gbr_zones$TYPE)) #list reef names

#get only habitat protection zones
hpzs <- subset(gbr_zones, TYPE == "Habitat Protection Zone", NSE = TRUE)

plet(hpzs, "NAME") #can click on zones to get the names: I'm going to filter for just two big-ish ones for the workshop

hpzs_subset <- subset(hpzs, NAME %in% c("HP-21-5296", "HP-16-5126" ), NSE = TRUE) |>
  rev() # reverse so that first row is the northernmost zone - make life easier for plotting

#project to Australian Albers projection - this is appropriate for mainland Australia and covers most of the GBR. There aren't any other EPSG code defined crs's suitable for the whole GBR that I could find. This is equal area
hpzs_subset_projected <- project(hpzs_subset, "epsg:3577")

writeVector(hpzs_subset_projected, "data/gbr_habitat_protection_zones.gpkg", overwrite = TRUE)
