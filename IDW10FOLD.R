library(gstat)
library(sp)
library(sf)



data <- read.csv("SSRD_AVE.csv")
indonesia = st_read("idn_admbnda_adm0_bps_20200401.shp")
wind_sf<- st_as_sf( data, coords = c(  "lon", "lat")  )
print(st_crs(wind_sf))
st_crs(wind_sf) <- 4326
wind_sf = st_transform(wind_sf, 4326)
st_crs(indonesia) <- 4326
indonesia = st_transform(indonesia, st_crs(wind_sf))
coor = as.data.frame(st_coordinates(wind_sf))
wind_sf$x = coor$X
wind_sf$y = coor$Y
wind_nogeom = st_drop_geometry(wind_sf) #get rid of geometry but keep all other attributes
wind_nogeom=na.omit(wind_nogeom)
gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs
st_bbox(indonesia)
#1 degree = 111.1 km, so 0.01 is around 1.11km which is 1110 metre; 0.1 is 11.1km, roughly 11100 metre
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template #check basic information about the raster template we created
idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred)
idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)
names(idw_mask) = c( "predicted","observed" )
tmap_mode("view") #tmap can also do raster plot
tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE)
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(wind_sf$ssrd), wind_sf$ssrd)
null #1.656786 is the baseline. 
n_idp = 20 #examine power ranging from 1 to 20
n_fold =10
rmse <- rep(NA, n_fold) #generate 10 NA
set.seed(7713)
kf <- sample(1:n_fold, nrow(wind_nogeom), replace=TRUE)
va = data.frame( c(1:n_idp), NA)
colnames(va) =c("idp","rmse") 
for (j in 1:n_idp) 
{
  for (i in 1:n_fold) {
    test <- wind_nogeom[kf == 1, ]
    train <- wind_nogeom[kf != 1, ]
    gs <- gstat(formula=ssrd~1, locations=~x+y, data=train, nmax=Inf, set=list(idp=j))
    pre = predict(gs, test, debug.level=0 )
    rmse[i] <- RMSE(test$ssrd, pre$var1.pred)
  }
  va[j,2] = (mean(rmse) )
}
va[which(va$rmse==min(va)),]
library(ggplot2)
ggplot(va) +
  geom_point(aes(x = idp, y= rmse))+
  geom_hline(yintercept=min(va), linetype="dashed", color = "red")+
  theme_classic()
par(mfrow = c(2,1)) # 2row by 3 columns
idp_list= c(7:8)
for (i in 7:8){
  gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=i)) #data should be in data frame format
  idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
  plot(idw$var1.pred, main=paste0("IDW interpolation, idp=", i)) 
}
par(mfrow = c(1,1)) # 2row by 3 columns
idp_list= c(7:8)
for (i in 7:8){
  gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=i)) #data should be in data frame format
  idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
  plot(idw$var1.pred, main=paste0("IDW interpolation, idp=", idp_list[i])) 
}
gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=7)) #data should be in data frame format
idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred, main=paste0("IDW interpolation, idp=", 7))




