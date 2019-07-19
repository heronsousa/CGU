library(rgdal)    
library(ggplot2)

shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR")

shp_df <- fortify(shp)

ggplot() +
  geom_polygon(data = shp_df, aes(x = long, y = lat, group = group), color='grey', fill='white') +
  coord_map()