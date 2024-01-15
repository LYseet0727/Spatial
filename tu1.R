pop=rast("pop.vrt")
library(dplyr)
library(sf)

IDW_F_L1 <- clipped_idw_non_protected %>%
  filter(!land %in% c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 15, 20, 21, 22))
IDW_F_L1 <- IDW_F_L1 %>%
  filter(p < 800)
IDW_F_L1 <- IDW_F_L1 %>%
  filter(s < 20)
IDW_F_L1 <- IDW_F_L1 %>%
  filter(t_nrst_r >= 150)
 
 coordinates <- st_coordinates(IDW_F_L1)
  
  data_with_coords <- cbind(IDW_F_L1, lon = coordinates[,1], lat = coordinates[,2])
  
  write.csv(data_with_coords, "IDW_F_L1.csv", row.names = FALSE)
  
  
  IDN_Protected <- st_read("IDN_Protected_union.shp")
  tmap_options(check.and.fix = TRUE)
  F_IDN_Pro=tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") + 
    tm_shape(IDN_Protected) +
    tm_fill(col = "lightgreen") + 
    tm_layout(main.title = "Protected area")+
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)  

  IDN_Protected <- st_read("IDN_Protected_union.shp")
  tmap_options(check.and.fix = TRUE)
  F_IDN_Pro=tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") + 
    tm_fill(col = "lightgreen",alpha = 0.7) + 
    tm_shape(IDN_Protected) +
    tm_fill(col = "purple") + 
    tm_layout(main.title = "Protected area")+
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)  
  
  IDN_cov<-rast("IDN_msk_cov.vrt") 
  tmap_options(check.and.fix = TRUE)
  F_IDN_Pro=tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") + 
    tm_shape(IDN_cov) +
    tm_raster( palette = "-RdYlBu", title = "Land Cover"  ) +
    tm_layout(main.title = "land cover", 
              legend.position = c("left", 0.17), 
              legend.text.size = 0.5, 
              legend.title.size = 0.6)+
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)    


  color_vector <- c("Unsuitable" = c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 15, 20, 21, 22), "Suitable" = c(9, 10, 13, 14, 16, 17, 18, 19))
  
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_cov) +
    tm_raster(style = "cat", breaks = color_vector, palette = c("lightgreen", "purple")) +
    tm_layout(main.title = "Suitability",legend.show = FALSE) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  
  print(F_IDN_Pro)
  
  


  # pop=rast("pop.vrt") 
  # tmap_options(check.and.fix = TRUE)
  # F_IDN_Pro=tm_shape(indonesia_boundary) +
  #   tm_borders(lwd = 1, col = "black") + 
  #   tm_shape(pop) +
  #   tm_raster( palette = "RdYlBu", title = "Population density" ,breaks = c(0,100,400, 800,10000,20000) ) +
  #   tm_layout(main.title = "Population density (per km^2)", 
  #             legend.position = c("left", 0.17), 
  #             legend.text.size = 0.5, 
  #             legend.title.size = 0.6)+
  #   tm_compass(type = "arrow", position = c("right", "top")) +
  #   tm_scale_bar(position = c(0.01, 0.01))
  # print(F_IDN_Pro)      
    

  color_breaks <- c(0, 100, 400, 800, 2000,  20000)
  color_palette <- colorRampPalette(c("lightblue","purple", "blue", "orange","red"))(length(color_breaks) - 1)
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(pop) +
    tm_raster(
      palette = color_palette,
      title = "Population density",
      breaks = color_breaks
    ) +
    tm_layout(
      main.title = "Population density (per km^2)",
      legend.position = c("left", 0.12),
      legend.text.size = 0.5,
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)
  
  
  
  colors <- c("green", "purple")  
  
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(pop) +
    tm_raster(style = "fixed", breaks = c(0, 800, 200000), palette = colors) +  
    tm_layout(main.title = "Suitability", legend.show = FALSE) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)
  


  color_vector <- c("Unsuitable" = "idn_msk_pop >= 800", "Suitable" = "idn_msk_pop < 800")

  colors <- c("Unsuitable" = "green", "Suitable" = "purple")
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(pop) +
    tm_raster(style = "cat", breaks = NULL, palette = colors) + 
    tm_layout(
      main.title = "Suitability",
      legend.show = FALSE  
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)

  IDN_alt<-rast("IDN_msk_alt.vrt") 
  tmap_options(check.and.fix = TRUE)
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_alt) +
    tm_raster(palette = "-RdYlBu", title = "Elevation") +  
    tm_layout(
      main.title = "Elevation",
      legend.position = c("left", 0.17),
      legend.text.size = 0.5,
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)    
 
  IDN_slope <- terrain(IDN_alt, "slope")
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_slope) +
    tm_raster(palette = "-RdYlBu", title = "Slope") +  
    tm_layout(
      main.title = "Slope",
      legend.position = c("left", 0.17),
      legend.text.size = 0.5,
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)    
  
  
  colors <- c("green", "purple")  
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_slope) +
    tm_raster(style = "fiexd", breaks = c(0, 20, 100), palette = colors) +  
    tm_layout(main.title = "Suitability", legend.show = FALSE) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)
  
  IDN_roads <- st_read("IDN_roads.shp")
  tmap_options(check.and.fix = TRUE)
  
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_roads) +
    tm_lines(col = "blue") +  
    tm_layout(
      main.title = "Roads",
      legend.position = c("left", 0.17),
      legend.text.size = 0.5,
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)    

  
  IDN_grid <- st_read("grid.geojson")
  tmap_options(check.and.fix = TRUE)
  
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_grid) +
    tm_lines(col = "blue") +  
    tm_layout(
      main.title = "Grids",
      legend.position = c("left", 0.17),
      legend.text.size = 0.5,
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)    
  
  
  clipped_idw=st_read("clipped_idw_5.0.shp")
  tmap_mode("plot")
  tm_shape(clipped_idw) +
    tm_dots(col = "var1_pred", size = 0.001, palette = "-RdYlBu") +
    tm_layout(
      main.title = "IDW Interpolation of ssrd",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))

  
  
  clipped_idw=st_read("data_7.shp")
  tmap_mode("plot")
  tm_shape(clipped_idw) +
    tm_dots(col = "t_nrst_r", size = 0.001, palette = "-RdYlBu", 
            breaks = c(0,1000, 20000, 40000, 50000, 100000, 150000, 200000, 250000)) +
    tm_layout(
      main.title = "Distance to the nearest road",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))

  
  clipped_idw=st_read("data_7.shp")
  tmap_mode("plot")
  tm_shape(clipped_idw) +
    tm_dots(col = "t_nrst_g", size = 0.001, palette = "-RdYlBu", 
            breaks = c(0,1000, 20000, 40000, 50000, 100000, 150000, 200000, 250000)) +
    tm_layout(
      main.title = "Distance to the nearest grid",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))
  
  IDN_roads <- st_read("IDN_roads.shp")
  tmap_options(check.and.fix = TRUE)
  
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_roads) +
    tm_lines(col = "blue") +  # Adjust color as needed
    tm_layout(
      main.title = "Roads",
      legend.position = c("left", 0.17),
      legend.text.size = 0.5,
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)    
  

  IDN_grid <- st_read("grid.geojson")
  tmap_options(check.and.fix = TRUE)
  
  F_IDN_Pro <- tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(IDN_grid) +
    tm_lines(col = "blue") +  
    tm_layout(
      main.title = "Grids",
      legend.position = c("left", 0.17),
      legend.text.size = 0.5,
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "top")) +
    tm_scale_bar(position = c(0.01, 0.01))
  print(F_IDN_Pro)    
  

  clipped_idw=st_read("clipped_idw_5.0.shp")
  clipped_idw$var1_pred <- clipped_idw$var1_pred/12342857.14
  clipped_idw <- clipped_idw %>%
    rename(capacity_GW = var1_pred)
  tmap_mode("plot")
  tm_shape(clipped_idw) +
    tm_dots(col = "capacity_GW", size = 0.001, palette = "-RdYlBu") +
    tm_layout(
      main.title = "Capacity_GW",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))
  
  
  
  clipped_idw=st_read("data_7.shp")
  tmap_mode("plot")
  tm_shape(clipped_idw) +
    tm_dots(col = "t_nrst_r", size = 0.001, palette = "-RdYlBu", 
            breaks = c(0,1000, 20000, 40000, 50000, 100000, 150000, 200000, 250000)) +
    tm_layout(
      main.title = "Distance to the nearest road",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))
  
  
  clipped_idw=st_read("data_7.shp")
  tmap_mode("plot")
  tm_shape(clipped_idw) +
    tm_dots(col = "t_nrst_g", size = 0.001, palette = "-RdYlBu", 
            breaks = c(0,1000, 20000, 40000, 50000, 100000, 150000, 200000, 250000)) +
    tm_layout(
      main.title = "Distance to the nearest grid",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))
  
  

  tmap_mode("plot")
  tm_shape(IDW_F_L1) +
    tm_dots(col = "predicted", size = 0.001, palette = "-RdYlBu") +
    tm_layout(
      main.title = "Suitable points with ssrd",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6
    ) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))
  

  
  library(readxl)
  INDEX <- read_xlsx("INDEX.xlsx")
  INDEX <- st_as_sf(INDEX, coords = c("lon", "lat"), crs = 4326, agr = "constant")
  
  
  tmap_mode("plot")
  tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(INDEX) +
    tm_dots(col = "Suitability Index", size = 0.01, palette = "-RdYlBu") +
    tm_shape(IDN_grid) +
    tm_lines(col = "blue") + 
    tm_layout(
      main.title = "Suitability Index & Grid map",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))
  
  
  Point <- read_xlsx("POINTS.xlsx")
  Point <- st_as_sf(Point, coords = c("lon", "lat"), crs = 4326)
  tm_shape(indonesia_boundary) +
    tm_borders(lwd = 1, col = "black") +
    tm_shape(Point) +
    tm_dots(col = "green", size = 0.25) +
    tm_shape(IDN_grid) +
    tm_lines(col = "blue") +  
    tm_layout(
      main.title = "Selected plants & Grid map",
      legend.position = c(0.01, 0.13),
      legend.width = 1,           
      legend.text.size = 0.4,       
      legend.title.size = 0.6) +
    tm_compass(type = "arrow", position = c("right", "0.7")) +
    tm_scale_bar(position = c(0.01, 0.01))
  
  