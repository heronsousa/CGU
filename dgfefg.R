library(googleway)

set_key("GOOGLE_MAP_KEY")

lat <- c(4,41) #India lat boundaries
lon <- c(68,99) #India long boundaries
center = c(mean(lat), mean(lon))

google_map(location = c(22, 85), zoom = 6)






# setwd("~/")
# 
# 
# library(rgdal)
# 
# shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
# 
# 
# sum_estado$Cod_UF <- c(27, 16, 29, 23, 53, 32, 26, 41, 33, 42)
# 
# brasil <- merge(shp, sum_estado, by.x='CD_GEOCUF', by.y='Cod_UF')
# proj4string(brasil) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
# Encoding(brasil$NM_ESTADO) <- "UTF-8"
# brasil$Valor[is.na(brasil$Valor)] <- 0
# 
# 
# install.packages('RColorBrewer')
# display.brewer.all()
# 
# install.packages('scales')
# install.packages('httpuv', type='binary')
# install.packages("leaflet")
# library(leaflet)
# 
# pal <- colorBin("Blues",domain = NULL) #cores do mapa
# state_popup <- paste0("<strong>Estado: </strong>",
#                       brasil$NM_ESTADO,
#                       "<br><strong>Valor: </strong>",
#                       brasil$Valor)
# leaflet(data = brasil) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(fillColor = ~pal(brasil$Valor),
#               fillOpacity = 0.8,
#               color = "#BDBDC3",
#               weight = 1,
#               popup = state_popup) %>%
#   addLegend("bottomright", pal = pal, values = ~brasil$Valor,
#             title = "Montante gasto em cada estado",
#             opacity = 1)

