
library(readxl)
library(tidyverse)
library(maps)
library(mapproj)

setwd("C:/Users/heronrs/Documents/Respostas OSS/")

registros = data.frame()
for(i in files_list){
  arquivo <- read_excel("ES.xlsx", col_names=c('Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', '2016', '2017', '2018', 'Latitude', 'Longitude'),
                        col_types=c('guess', 'guess', 'numeric', 'numeric', 'numeric', 'guess', 'guess'), skip = 1)
  #Define à qual estado pertence cada registro, de acordo com o nome do arquivo
  #Retira do nome do arquivo a sigla do estado
  estado <- 'ES'
  #Cria uma nova coluna "Estado", replicando sua sigla pelo numero de registro que há no arquivo
  arquivo$Estado <- rep(estado, nrow(arquivo))
  registros <- rbind.data.frame(registros,arquivo)

rm(arquivo)
rm(dados)

Br <- map_data("world") %>% filter(region=="Brazil")

ggplot() + 
  geom_polygon(data=Br, aes(x=long, y=lat, group=group), fill="grey", alpha=0.3) +
  geom_point(data=registros, aes(x=Longitude, y=Latitude))

rm(data_br)

