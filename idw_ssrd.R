library(sf)
library(sp)
library(dplyr)
library(terra)
library(tmap)
library(ggplot2)
library(leaflet)
library(spatstat)
library(gridExtra)
library(raster)
library(gstat)
library(sp)


data <- read.csv("average_ssrd_values.csv")

coordinates(data) <- ~lon+lat
x_min <- min(90)
x_max <- max(145)
y_min <- min(-14)
y_max <- max(12)
step_size <- 0.05 


grid_data <- expand.grid(x = seq(from = x_min, to = x_max, by = step_size),
                         y = seq(from = y_min, to = y_max, by = step_size))
coordinates(grid_data) <- ~x+y
grid_sp <- SpatialPointsDataFrame(grid_data, data.frame(id = row.names(grid_data)))


idw_result <- idw(formula = ssrd ~ 1, data, newdata = grid_sp, idp = 5.0)

idw_result_sf <- st_as_sf(idw_result)
st_crs(idw_result_sf) <- 4326
idw_result_sf <- st_transform(idw_result_sf, st_crs(indonesia_boundary))
clipped_idw <- st_intersection(idw_result_sf, st_union(indonesia_boundary))

