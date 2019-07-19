library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(maps)
library(viridis)
library(rgdal)
library(plotly)
library(xlsx)

options(scipen=999)
options(digits = 10)

# # Parte do cógido foi comentado, pois seria necessário somente para a leitura inicial dos arquivos e foi criado um novo arquivo
# # com todos os dados. Caso queira adicionar novos registros, basta descomentar e executar o código.
# # *Para descomentar: selecione todo o código comentado e use o atalho "CTRL+SHIFT+C"
# 
#
#
# # Para uma melhor identificação, os arquivos foram renomeados antes da leitura.
# # 1 se o arquivo pertencer a gestão estadual e 2 se for municipal, além da sigla do estado e o nome da capital.
# # Ex.: '1DF Brasília', '2RJ Rio de Janeiro'
#
# dir <- "C:/Users/heronrs/Documents/Respostas OSS/"
# 
# # Vetor com os nomes arquivos
# lista_arq = list.files(path=dir)
# 
# # Cria data frame com os dados das respostas recebidas
# registros = data.frame()
# 
# for(i in lista_arq){
# 
#   arquivo <- read_excel(paste0(dir,i), col_names=c('Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', '2016', '2017', '2018', '2019'),
#                         col_types=c('guess', 'guess', 'numeric', 'numeric', 'numeric', 'numeric'), skip = 1)
# 
#   # Define à qual estado e municipio pertence cada registro, de acordo com o nome do arquivo
#   # Nome do arquivo
#   nome_arq <- sub("\\..*", "", i)
#   gestao <- substr(nome_arq, 0, 1)
#   # Retira do nome do arquivo a sigla do estado
#   estado <- substr(nome_arq, 2, 3)
#   # Retira do nome do arquivo o municipio
#   municipio <- substr(nome_arq, 5, nchar(nome_arq))
# 
#   # Identificando à qual gestão pertence cada registro
#   if(gestao==1){
#     arquivo$Gestao <- rep('Estadual', nrow(arquivo))
#   }
#   else{
#     arquivo$Gestao <- rep('Municipal', nrow(arquivo))
#   }
# 
#   # Cria uma nova coluna "Estado", replicando sua sigla pelo numero de registros que há no arquivo
#   arquivo$Estado <- rep(estado, nrow(arquivo))
#   # Cria uma nova coluna "Municipio", replicando seu nome pelo numero de registros que há no arquivo
#   arquivo$Municipio <- rep(municipio, nrow(arquivo))
# 
#   registros <- rbind.data.frame(registros,arquivo)
#   rm(arquivo)
# }
# 
# rm(i)
# rm(lista_arq)
# rm(gestao)
# rm(nome_arq)
# rm(estado)
# rm(municipio)
# rm(dir)
# 
# #Transforma a coluna Estado em factor
# registros$Estado <- factor(registros$Estado)
# 
# # Chave para validar acesso ao Google Maps API
# register_google(key='AIzaSyC3EhdFNmI8x9rSyGPDWln6FKkLj6QrDIM')
# 
# latlon <- data.frame()
# 
# # Atraves da função geocode, obtem a latitude e longitude de cada registro da coluna 'Nome_da_Unidade_de_Saude'
# for(i in registros$Nome_da_Unidade_de_Saúde){
#   latlon <- rbind.data.frame(latlon, geocode(paste(i, "Brasil", sep=" ")))
# }
# # Adiciona as colunas lat e lon ao data frame 'registros'
# registros <- cbind.data.frame(registros, latlon)
# 
# latlon_mun <- data.frame()
# 
# # Obtem latitude e longitude de cada municipio
# for(i in registros$Municipio){
#   latlon_mun <- rbind.data.frame(latlon_mun, geocode(paste(i, "Brasil", sep=" ")))
# }
# 
# colnames(latlon_mun) <- c('lon_mun', 'lat_mun')
# # Adiciona as colunas lat_mun e lon_mun ao data frame registros
# registros <- cbind.data.frame(registros, latlon_mun)
# 
# # As coordenadas recebidas do google maps podem não ser exatas, foi necessário criar um arquivo
# # para consertar manualmente os possiveis erros nas coordenadas
# 
# # Adiciona os dados ao arquivo csv já existente
# write.table(registros, 'registros_coords.csv', sep=',',append=TRUE, row.names=FALSE, col.names=FALSE)
# 
# rm(latlon)
# rm(latlon_mun)
# rm(i)
# rm(registros)

# Leitura do arquivo csv com todos os dados das OSS e suas coordenadas corrigidas
registros_coords <- read.csv('C:/Users/heronrs/Documents/registros_oss.csv')
registros <- registros_coords  %>% arrange(Nome_da_Unidade_de_Saude)
# Formatando as colunas dos anos com os respectivos valores usando a função 'melt' do pacote 'reshape2'
dados = melt(registros, id=c("NUm", 'Gestao', 'Estado', 'Municipio', 'Nome_da_Unidade_de_Saude', 'CNPJ_da_OSS', 'lat', 'lon','lon_mun', 'lat_mun'), variable.name="Ano", value.name = "Valor")
rm(registros_coords)

registros$NUm <- 1:452

registros$X2019 <- NULL


# Summarize dos valores de cada OSS mantendo as colunas lat e lon
dados_oss_cg <- dados %>% group_by(NUm, Nome_da_Unidade_de_Saude, Estado) %>% summarize(Valor = sum(Valor, na.rm=TRUE)) %>% arrange(Estado)
dados_oss_cg <- dados_oss_cg 
write.table(dados_oss_cg, 'Dados OS.csv', sep='¨',append=F, row.names=FALSE, col.names=T)
dados_oss_cg$Valor<-NULL
write.xlsx(dados_oss_cg, file, sheetName = "OSS", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
# Cria a coluna Valor_pn que possui o valor total de cada OSS com a separação decimal e adiciona o "R$"
dados_oss_cg$Valor_pn <- prettyNum(dados_oss_cg$Valor, big.mark = ",")
dados_oss_cg$Valor_pn <- paste("R$", dados_oss_cg$Valor_pn)

# Leitura do arquivo BRUFE250GC_SIR na pasta Mapa, que é o shapefile do Brasil com a divisão de estados
# Definir o diretório onde esta a pasta Mapa. Não esquecer de colocar "\\." após do nome do diretório
shp <- readOGR("C:/Users/heronrs/Documents/Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

# Transforma o shapefile em data frame
shp_df <- fortify(shp)

map_oss = dados_oss_cg %>%
  # Ordena o data frame de acordo com o valor, para que os maiores valores apareçam por cima
  arrange(Valor) %>%
  # Prepara o texto que será mostrado no mapa
  mutate(description=paste(Nome_da_Unidade_de_Saude, "\n", "Valor: ", Valor_pn, sep="")) %>%
  ggplot() +
  geom_polygon(data = shp_df, aes(x = long, y = lat, group = group), color='gray39', fill='gray79') +
  geom_point(aes(x=lon, y=lat, size=1, text=description, color=Valor)) +
  scale_color_viridis(option="inferno", trans="log10", begin=1, end=0) +
  scale_alpha_continuous(trans="log10") +
  labs(y=NULL, x=NULL) +
  theme_void() +
  theme(legend.position = 'none')

map <- ggplotly(map_oss, tooltip="text")

# Plot do mapa
map

# Exporta o arquivo html do mapa
htmlwidgets::saveWidget(as_widget(map), "mapa_oss.html")
