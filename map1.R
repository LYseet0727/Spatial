install.packages("ggspatial")

library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
indonesia_sf <- st_read("/Users/siqinlilv/Desktop/spacial energy/idn_admbnda_adm4_bps_20200401.shp")


data <- read_excel("/Users/siqinlilv/Desktop/spacial energy/power plant Indonesia.xlsx")
colnames(data)
data <- data %>%
  filter(!is.na(longitude) & !is.na(latitude))
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)


ggplot() +
  geom_sf(data = indonesia_sf, fill = "white", color = "black") +
  geom_sf(data = data_sf, aes(size = capacity_mw, color = status), alpha = 0.7) +
  scale_color_manual(values = c("existing" = "green", "construction" = "orange", "planned" = "blue")) +
  theme_minimal() +
  labs(title = "Renewable Energy Power Plants in Indonesia",
       subtitle = "Size indicates capacity, color indicates status",
       color = "Status") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))


