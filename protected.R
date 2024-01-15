
IDN_Protected_0 <- st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp_0/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
IDN_Protected_1 <- st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp_1/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
IDN_Protected_2 <- st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp_2/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")

IDN_Protected_combined <- rbind(IDN_Protected_0, IDN_Protected_1, IDN_Protected_2)
IDN_Protected_union <- st_union(IDN_Protected_combined)
IDN_Protected_union_sf <- st_as_sf(IDN_Protected_union)
st_write(IDN_Protected_union_sf, "IDN_Protected_union.shp")




