
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

install.packages("readxl")
install.packages("rgdal")
install.packages("stars")

library(sf)
library(ggplot2)
library(readxl)
library(raster)
library(rgdal)
library(terra)
indonesia_boundary <- st_read("idn_admbnda_adm0_bps_20200401.shp")

if (is.na(st_crs(indonesia_boundary))) {
  indonesia_boundary <- st_set_crs(indonesia_boundary, 4326)
}
#slice data
library(ncdf4) #library to read and process netcdf data
ssrd <- nc_open("ssrd.nc" )
lon <- ncvar_get(ssrd, "longitude")
lat <- ncvar_get(ssrd, "latitude")
time <- ncvar_get(ssrd, "time")
library(chron) #deal with chronological objects
tunits <- ncatt_get(ssrd,"time","units") #tunits <- ncatt_get(era,"longitude","units")
#convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
chron(time/24, origin=c(tmonth, tday, tyear) ) #this function is of great help. It can convert the hours since format to the format we are quite familiar with.
#get Variables :u10,v10, ssrd. similar to get dimension data, we are going to use the same method, ncvar_get()
ssrd_array <- ncvar_get(ssrd,"ssrd") #get the Surface solar radiation downwards
dim(ssrd_array) #dimension is 501 * 186 *8. Think about the Figure 1. The reason why it is called array is it is composed of 8 slices 
dlname <- ncatt_get(ssrd,"ssrd","long_name")
dunits <- ncatt_get(ssrd,"ssrd","units")
fillvalue <- ncatt_get(ssrd,"ssrd","_FillValue")
library(lattice)
library(RColorBrewer)
#Get a single time slice of the data using ssrd_array
ssrd_slice <- ssrd_array[,,1,2] 
#The ssrd_slice is actually a matrix. class(ssrd_slice)
# What does 2 in ssrd_array[,,2] indicate?  What if I want to slice all four time slice for "07/01/19"? 
length(na.omit(as.vector(ssrd_slice))) /length(as.vector(ssrd_slice)) #8.5% are valid
dim(ssrd_slice )
image(ssrd_slice, col=rev(brewer.pal(10,"RdBu")) )
max_rad <- max(ssrd_slice, na.rm=TRUE)
max_rad
lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat) #we now have a matrix that has 93186 rows and 2columns. 93186=501*186
ssrd_vec <- as.vector( ssrd_slice) 
length(ssrd_vec)
ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 
#Creating a spatial object from a lat/lon table
library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )
library(tmap)
tmap_mode("view")
tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")

library(terra)
library(raster)

idnssrd_sp <- raster::as(idnssrd_raster, "SpatialPixelsDataFrame")

library(tmap)
tmap_mode("view")
tm_shape(idnssrd)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")
ssrd <- nc_open("ssrd.nc" )
ssrd_array <- ncvar_get(ssrd,"ssrd")
ssrd_slice <- ssrd_array[,,1,] 
dim(ssrd_slice )
lonlattime <- as.matrix( (expand.grid(lon, lat, time))) #lon and lat are what we extracted in step 2.
dim(lonlattime) #we now have a matrix that has 93186 rows and 2columns. 93186=501*186
ssrd_vec <- as.vector( ssrd_slice) 
length(ssrd_vec)
ssrd_df <- data.frame( cbind( lonlattime,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat","time" ,"ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 
write.csv(ssrd_df_value, "ssrd_df_value.csv", row.names = FALSE)
#Creating a spatial object from a lat/lon table
library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )
library(tmap)
tmap_mode("view")

library(gstat)
library(sp)





IDN_Protected <- st_read("Protected.shp")
library(terra)
IDN_alt<-rast("IDN_msk_alt.vrt") 
IDN_slope <- terrain(IDN_alt, "slope")
IDN_aspect <- terrain(IDN_alt, "aspect")
plot(IDN_slope)
IDN_cov<-rast("IDN_msk_cov.vrt") 
IDN_roads <- st_read("IDN_roads.shp")
IDN_grid <- st_read("grid.geojson")
pop=rast("pop.vrt") 



st_crs(clipped_idw)
st_crs(IDN_grid)
IDN_grid <- st_transform(IDN_grid, st_crs(clipped_idw))
distances_grid <- st_distance(clipped_idw,IDN_grid)
min_distances_grid <- apply(distances_grid, 1, min)
clipped_idw$to_nearest_road <- min_distances
clipped_idw$to_nearest_grid <- min_distances_grid

distances_2 <- st_distance(clipped_idw,IDN_roads)
min_distances <- apply(distances, 1, min)
clipped_idw$to_nearest_road <- min_distances
library(sf)
library(dplyr)
clipped_idw$var1_vr <- NULL
s <- extract(IDN_slope, sp_population_vect)
clipped_idw$s<- slope
clipped_idw$s$ID <- NULL
clipped_idw$s <- extract(IDN_slope, sp_population_vect)[, "slope"]
sp_population <- as(clipped_idw, "Spatial")
sp_population_vect <- vect(sp_population)
p <- extract(pop, sp_population_vect)
clipped_idw$p<- p
clipped_idw$p$ID <- NULL
clipped_idw$p <- extract(pop, sp_population_vect)[, "idn_msk_pop"]
land <- extract(IDN_cov, sp_population_vect)
clipped_idw$land<- land
clipped_idw$land$ID <- NULL
clipped_idw$land <- extract(IDN_cov, sp_population_vect)[, "IDN_msk_cov"]
a <- extract(IDN_aspect, sp_population_vect)
clipped_idw$a<- a
clipped_idw$a$ID <- NULL
clipped_idw$a <- extract(IDN_aspect, sp_population_vect)[, "aspect"]

  clipped_idw_raster <- rast(clipped_idw_non_protected)
  sp_population <- as(clipped_idw_non_protected, "Spatial")
  sp_population_vect <- vect(sp_population)
  values(clipped_idw_raster) <- clipped_idw$var1_pred
  pre <- extract(clipped_idw_raster, sp_population_vect)
  clipped_idw_non_protected$predicted<- pre
  clipped_idw_non_protected$predicted$ID <- NULL
  clipped_idw_non_protected$vr1_prd <- NULL
clipped_idw_non_protected <- na.omit(clipped_idw_non_protected)
st_write(clipped_idw_clean, "data_7.shp")

st_write(clipped_idw, "data_5.shp")
idw=st_read("data_5.shp")




st_crs(indonesia_boundary) <- st_crs(4326) 
IDN_Protected <- st_transform(IDN_Protected, st_crs(indonesia_boundary))
indonesia_boundary <- st_make_valid(indonesia_boundary)
IDN_Protected <- st_make_valid(IDN_Protected)
indonesia_boundary_geom <- st_geometry(indonesia_boundary)
IDN_Protected_geom <- st_geometry(IDN_Protected)
non_protected_areas <- st_difference(indonesia_boundary_geom, IDN_Protected_geom)
non_protected_areas_sf <- st_sf(geometry = non_protected_areas)
tmap_options(check.and.fix = TRUE)

tm_IDN_NON_Pro=tm_shape(non_protected_areas_sf) +
  tm_borders(col = "gray", lwd = 1) +
  tm_fill(col = "#21918c") +
  tm_layout(main.title = "Non protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))
print(tm_IDN_NON_Pro)
tm_IDN_Pro=tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "gray") + 
  tm_shape(IDN_Protected) +
  tm_fill(col = "#21918c") + 
  tm_layout(main.title = "Protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))
print(tm_IDN_Pro)



st_crs(IDN_Protected_union_sf) <- 4326
library(sf)
IDN_Protected_union_sf <- st_make_valid(IDN_Protected_union_sf)
intersections <- st_intersects(clipped_idw_clean,IDN_Protected)
clipped_idw_non_protected <- clipped_idw_clean[lengths(intersections) == 0, ]
library(dplyr)
clipped_idw_non_protected <- clipped_idw_non_protected %>%
  filter(!rowSums(is.na(.)))
st_write(clipped_idw_non_protected, "data_8.shp")
clipped_idw_non_protected=st_read("data_8.shp")

tmap_mode("view")
tm_clipped_out <- tm_shape(indonesia_boundary) +
  tm_borders() +
  tm_shape(clipped_idw_non_protected) +
  tm_dots(size = 0.0005, col = clipped_idw_non_protected$vr1_prd, palette = "magma", style = "quantile")+
  tm_layout(main.title = "IDW non protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))
print(tm_clipped_out)

tmap_mode("view")
tm_clipped_out <- tm_shape(indonesia_boundary) +
  tm_borders() +
  tm_shape(clipped_idw_non_protected) +
  tm_dots(col = "vr1_prd", size = 0.001, palette = "viridis")+
  tm_layout(main.title = "IDW non protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))
print(tm_clipped_out)













































st_write(clipped_idw_outside_protected, "data_protected_areas.shp")

indonesia_boundary_non_protected <- st_difference(indonesia_boundary, IDN_Protected)
plot(indonesia_boundary_non_protected)


