library(dplyr)
library(ggrepel)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plotly)
library(mapproj)
library(viridis)
library(ggmap)

register_google(key='AIzaSyC3EhdFNmI8x9rSyGPDWln6FKkLj6QrDIM')

# rm_accent <- function(str,pattern="all") {
#   if(!is.character(str))
#     str <- as.character(str)
#   pattern <- unique(pattern)
#   if(any(pattern=="Ç"))
#     pattern[pattern=="Ç"] <- "ç"
#   symbols <- c(
#     acute = "áéíóúÁÉÍÓÚýÝ",
#     grave = "àèìòùÀÈÌÒÙ",
#     circunflex = "âêîôûÂÊÎÔÛ",
#     tilde = "ãõÃÕñÑ",
#     umlaut = "äëïöüÄËÏÖÜÿ",
#     cedil = "çÇ"
#   )
#   nudeSymbols <- c(
#     acute = "aeiouAEIOUyY",
#     grave = "aeiouAEIOU",
#     circunflex = "aeiouAEIOU",
#     tilde = "aoAOnN",
#     umlaut = "aeiouAEIOUy",
#     cedil = "cC"
#   )
#   accentTypes <- c("´","`","^","~","¨","ç")
#   if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
#     return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
#   for(i in which(accentTypes%in%pattern))
#     str <- chartr(symbols[i],nudeSymbols[i], str)
#   return(str)
# }
# 
# registros$Nome_da_Unidade_de_Saúde <- rm_accent(registros$Nome_da_Unidade_de_Saúde)
# 
# latlon <- data.frame()
# 
# for(i in registros$Nome_da_Unidade_de_Saúde){
#   latlon <- rbind.data.frame(latlon, geocode(paste(i, "Brasil", sep=" ")))
# }
# registros <- cbind.data.frame(registros, latlon)
# 
# latlon_mun <- data.frame()
# for(i in registros_coords$Municipio){
#   latlon_mun <- rbind.data.frame(latlon_mun, geocode(paste(i, "Brasil", sep=" ")))
# }
# colnames(latlon_mun) <- c('lon_mun', 'lat_mun')
# registros_coords <- cbind.data.frame(registros_coords, latlon_mun)
# 
# 
# write.csv(registros_coords, file='registros_coords.csv', row.names = F)

registros_coords <- read.csv('C:/Users/heronrs/Documents/registros_coords.csv')

dados = melt(registros_coords, id=c('Gestao', 'Estado', 'Municipio', 'Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', 'lat', 'lon','lon_mun', 'lat_mun'), variable.name="Ano", value.name = "Valor")
dados_oss_cg <- dados %>% group_by(Nome_da_Unidade_de_Saúde, lat, lon) %>% summarize(Valor = sum(Valor, na.rm=TRUE))
dados_oss_cg$Valor_pn <- prettyNum(dados_oss_cg$Valor, big.mark = ",")
dados_oss_cg$Valor_pn <- paste("R$", dados_oss_cg$Valor_pn)

shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
shp_df <- fortify(shp)

map_oss = dados_oss_cg %>%
  arrange(Valor) %>%
  mutate(description=paste(Nome_da_Unidade_de_Saúde, "\n", "Valor: ", Valor_pn, sep="")) %>%
  ggplot() +
  geom_polygon(data = shp_df, aes(x = long, y = lat, group = group), color='gray39', fill='gray79') +
  geom_point(aes(x=lon, y=lat, size=Valor, text=description, color=Valor, alpha=Valor)) +
  scale_size_continuous(range=c(2,10)) +
  scale_color_viridis(option="inferno", trans="log10" ) +
  scale_alpha_continuous(trans="log10") +
  labs(y=NULL, x=NULL) +
  theme_void() +
  theme(legend.position = 'none')

ggplotly(map_oss, tooltip="text")



dados_mun_cg <- dados %>% group_by(Municipio, lat_mun, lon_mun) %>% summarize(Valor = sum(Valor, na.rm=TRUE))
dados_mun_cg$Valor_pn <- prettyNum(dados_mun_cg$Valor, big.mark = ",")
dados_mun_cg$Valor_pn <- paste("R$", dados_mun_cg$Valor_pn)

map_municipio = dados_mun_cg %>%
  arrange(Valor) %>%
  mutate(description=paste(Municipio, "\n", "Valor: ", Valor_pn, sep="")) %>%
  ggplot() +
  geom_polygon(data = shp_df, aes(x = long, y = lat, group = group), color='gray39', fill='gray79') +
  geom_point(aes(x=lon_mun, y=lat_mun, size=Valor, text=description, color=Valor)) +
  scale_size_continuous(range=c(2,10)) +
  scale_color_viridis(option="inferno", trans="log10" ) +
  scale_alpha_continuous(trans="log10") +
  labs(y=NULL, x=NULL) +
  theme_void() +
  theme(legend.position = 'none')

ggplotly(map_municipio, tooltip="text")
