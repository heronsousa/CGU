#Analise das respostas dos Formulários OSS

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(maps)
library(mapproj)
library(viridis)
library(ggrepel)
library(rgdal)
library(plotly)
library(ggmap)

options(scipen=999)
options(digits = 10)

dir <- "C:/Users/heronrs/Documents/Respostas OSS/"

#Vetor com os nomes arquivos
lista_arq = list.files(path=dir)

#Cria data frame com os dados das respostas recebidas
registros = data.frame()

for(i in lista_arq){
  
  arquivo <- read_excel(paste0(dir,i), col_names=c('Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', '2016', '2017', '2018', '2019'),
                        col_types=c('guess', 'guess', 'numeric', 'numeric', 'numeric', 'numeric'), skip = 1)
  
  #Define à qual estado e municipio pertence cada registro, de acordo com o nome do arquivo
  #Nome do arquivo
  nome_arq <- sub("\\..*", "", i)
  gestao <- substr(nome_arq, 0, 1)
  #Retira do nome do arquivo a sigla do estado
  estado <- substr(nome_arq, 2, 3)
  #Retira do nome do arquivo o municipio
  municipio <- substr(nome_arq, 5, nchar(nome_arq))
  
  #Identificando à qual gestão pertence cada registro
  if(gestao==1){
    arquivo$Gestao <- rep('Estadual', nrow(arquivo))
  }
  else{
    arquivo$Gestao <- rep('Municipal', nrow(arquivo))
  }
  
  #Cria uma nova coluna "Estado", replicando sua sigla pelo numero de registros que há no arquivo
  arquivo$Estado <- rep(estado, nrow(arquivo))
  #Cria uma nova coluna "Municipio", replicando seu nome pelo numero de registros que há no arquivo
  arquivo$Municipio <- rep(municipio, nrow(arquivo))
  
  registros <- rbind.data.frame(registros,arquivo)
  rm(arquivo)
}

rm(i)
rm(lista_arq)
rm(gestao)
rm(nome_arq)
rm(estado)
rm(municipio)
rm(dir)

#Transforma a coluna Estado em factor
registros$Estado <- factor(registros$Estado)

#Formatando as colunas dos anos com os valores usando a função 'melt' do pacote 'reshape2'
dados = melt(registros, id=c('Gestao', 'Estado', 'Municipio', 'Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS'), variable.name="Ano", value.name = "Valor")


#Separando os dados em estadual e municipal para contagem de OSS
estadual_count <- registros[which(registros$Gestao == 'Estadual'),]
municipal_count <- registros[which(registros$Gestao == 'Municipal'),]

#Separando os dados em estadual e municipal para representação dos montantes
estadual_sum <- dados[which(dados$Gestao == 'Estadual'),]
municipal_sum <- dados[which(dados$Gestao == 'Municipal'),]

#Contagem de OSS em cada gestao, estado e municipio
count_gestao <- registros %>% count(Gestao)
count_estado <- estadual_count %>% count(Estado)
count_municipio <- municipal_count %>% count(Municipio)

rm(municipal_count)
rm(estadual_count)

#Summarize dos valores por ano e por estado
sum_ano <- group_by(dados, Ano) %>% summarize(Valor = sum(Valor, na.rm=TRUE))
sum_estado <- group_by(dados, Estado) %>% summarize(Valor = sum(Valor, na.rm=TRUE))

sum_ano_municipio <- group_by(municipal_sum, Ano) %>% summarize(Valor = sum(Valor, na.rm=TRUE))
sum_ano_estado <- group_by(estadual_sum, Ano) %>% summarize(Valor = sum(Valor, na.rm=TRUE))

sum_estado_estadual <- group_by(estadual_sum, Estado) %>% summarize(Valor = sum(Valor, na.rm=TRUE))
sum_municipio_municipal <- group_by(municipal_sum, Municipio) %>% summarize(Valor = sum(Valor, na.rm=TRUE))

rm(estadual_sum)
rm(municipal_sum)
rm(dados)

#--------------------------------------------------------------------------------------------------------------------------------------------------------

#Plotando valor pelo ano
ggplot(sum_ano, aes(x=Ano, y=Valor, fill=Ano)) +
  geom_bar(stat = "identity", colour='black') +
  geom_text(aes(label=format(round(as.numeric(Valor), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle('Montante dos valores das OSS por ano (Gestão estadual e municipal)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Valor (em R$)') +
  theme_bw()

#Plotando valor pelo ano na gestão municipal
ggplot(sum_ano_municipio, aes(x=Ano, y=Valor, fill=Ano)) +
  geom_bar(stat = "identity", colour='black') +
  geom_text(aes(label=format(round(as.numeric(Valor), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle('Montante dos valores das OSS por ano (Gestão municipal)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Valor (em R$)') +
  theme_bw()

#Plotando valor pelo ano na gestão estadual
ggplot(sum_ano_estado, aes(x=Ano, y=Valor, fill=Ano)) +
  geom_bar(stat = "identity", colour='black') +
  geom_text(aes(label=format(round(as.numeric(Valor), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle('Montante dos valores das OSS por ano (Gestão estadual)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Valor (em R$)') +
  theme_bw()



#Plotando valor por estado na gestão estadual
ggplot(sum_estado_estadual, aes(x=Estado, y=Valor, fill=Estado)) +
  geom_bar(stat = "identity", colour='black') +
  scale_y_log10(breaks=c(10^6, 10^7, 10^8, 10^9, 10^10, 10^11)) +
  coord_cartesian(ylim = c(10^6, 10^7, 10^8, 10^9, 10^10, 10^11)) +
  geom_text(aes(label=format(round(as.numeric(Valor), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.4), vjust=-0.25) +
  scale_colour_brewer("Diamond\nclarity") +
  ggtitle('Montante dos valores das OSS por estado (Gestão estadual)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Valor (em R$)') +
  theme_bw()

#Plotando valor por estado na gestão municipal
ggplot(sum_municipio_municipal, aes(x=Municipio, y=Valor, fill=Municipio)) +
  geom_bar(stat = "identity", colour='black') +
  scale_y_log10(breaks=c(10^6, 10^7, 10^8, 10^9, 10^10)) +
  coord_cartesian(ylim = c(10^6, 10^7, 10^8, 10^9, 10^10)) +
  geom_text(aes(label=format(round(as.numeric(Valor), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.4), vjust=-0.25) +
  ggtitle('Montante dos valores das OSS por municipio (Gestão municipal)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Valor (em R$)') +
  theme_bw()

#--------------------------------------------------------------------------------------------------------------------------------------------------------

rm(sum_ano)
rm(sum_estado)
rm(sum_ano_estado)
rm(sum_ano_municipio)
rm(sum_municipio_municipal)
rm(sum_estado_estadual)

#--------------------------------------------------------------------------------------------------------------------------------------------------------

#Plotando quantidade de OSS por gestão
ggplot(data=count_gestao, aes(x=Gestao, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Gestao, xend=Gestao, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantidade de OSS por gestão", y=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

#Plotando quantidade de OSS em cada estado
ggplot(data=count_estado, aes(x=Estado, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Estado, xend=Estado, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantidade de OSS por estado (Gestão Estadual)", y=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

#Plotando quantidade de OSS por municipio
ggplot(data=count_municipio, aes(x=Municipio, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Municipio, xend=Municipio, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantidade de OSS por municipio (Gestão municipal)", y=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

rm(count_gestao)
rm(count_estado)
rm(count_municipio)


#--------------------------------------------------------------------------------------------------------------------------------------------------------

dados_gerais_estadual <- read_excel("C:/Users/heronrs/Documents/Arquivos csv/Dados gerais OSS - Estaduais.xlsx")

q <- group_by(dados_gerais_estadual, Primeiro_contato) %>% count(Primeiro_contato)
ggplot(data=q, aes(x=Primeiro_contato, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Primeiro_contato, xend=Primeiro_contato, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Respostas no primeiro contato (Gestão estadual)", y=NULL, x=NULL) +
  xlab('') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()


asd <- dados_gerais_estadual[which(dados_gerais_estadual$Primeiro_contato == 'Sim'),]
z <- group_by(asd, Resposta_limesurvey) %>% count(Resposta_limesurvey)
ggplot(data=z, aes(x=Resposta_limesurvey, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Resposta_limesurvey, xend=Resposta_limesurvey, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Respostas do formulário no limesurvey", y=NULL, x=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

sla <- dados_gerais_estadual[which(dados_gerais_estadual$Resposta_limesurvey == 'Sim'),]
s <- group_by(sla, Possui_OSS) %>% count(Possui_OSS)
ggplot(data=s, aes(x=Possui_OSS, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Possui_OSS, xend=Possui_OSS, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantos estados possuem OSS", y=NULL, x=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()


rm(dados_gerais_estadual)
rm(q)
rm(asd)
rm(z)
rm(sla)
rm(s)



dados_gerais_municipal <- read_excel("C:/Users/heronrs/Documents/Arquivos csv/Dados gerais OSS - Municipios.xlsx")

q <- group_by(dados_gerais_municipal, Primeiro_contato) %>% count(Primeiro_contato)
ggplot(data=q, aes(x=Primeiro_contato, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Primeiro_contato, xend=Primeiro_contato, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Respostas no primeiro contato (Gestão municipal)", y=NULL, x=NULL) +
  xlab('') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()


asd <- dados_gerais_municipal[which(dados_gerais_municipal$Primeiro_contato == 'Sim'),]
z <- group_by(asd, Resposta_limesurvey) %>% count(Resposta_limesurvey)
ggplot(data=z, aes(x=Resposta_limesurvey, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Resposta_limesurvey, xend=Resposta_limesurvey, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Respostas do formulário no limesurvey", y=NULL, x=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

sla <- dados_gerais_municipal[which(dados_gerais_municipal$Resposta_limesurvey == 'Sim'),]
s <- group_by(sla, Possui_OSS) %>% count(Possui_OSS)
ggplot(data=s, aes(x=Possui_OSS, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Possui_OSS, xend=Possui_OSS, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantos municipios possuem OSS", y=NULL, x=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()


rm(dados_gerais_municipal)
rm(q)
rm(asd)
rm(z)
rm(sla)
rm(s)



#--------------------------------------------------------------------------------------------------------------------------------------------------------

# Chave para validar acesso ao Google Maps API
# register_google(key='AIzaSyC3EhdFNmI8x9rSyGPDWln6FKkLj6QrDIM')
# 
# latlon <- data.frame()
#
# Atraves da função geocode, obtem a latitude e longitude de cada registro da coluna 'Nome_da_Unidade_de_Saude'
# for(i in registros$Nome_da_Unidade_de_Saúde){
#   latlon <- rbind.data.frame(latlon, geocode(paste(i, "Brasil", sep=" ")))
# }
# Adiciona as colunas lat e lon ao data frame 'registros'
# registros <- cbind.data.frame(registros, latlon)
# 
# latlon_mun <- data.frame()
#
# Obtem latitude e longitude de cada municipio
# for(i in registros$Municipio){
#   latlon_mun <- rbind.data.frame(latlon_mun, geocode(paste(i, "Brasil", sep=" ")))
# }
#
# colnames(latlon_mun) <- c('lon_mun', 'lat_mun')
# Adiciona as colunas lat_mun e lon_mun ao data frame registros
# registros <- cbind.data.frame(registros, latlon_mun)
# 
# As coordenadas recebidas do google maps podem não ser exatas, foi necessário criar um arquivo
# para consertar manualmente os possiveis erros
#
# Adiciona os dados ao arquivo csv já existente 
# write.table(registros, 'registros_coords.csv', sep=',',append=TRUE, row.names=FALSE, col.names=FALSE)
# 
# rm(latlon)
# rm(latlon_mun)
# rm(i)
rm(registros)

registros_coords <- read.csv('C:/Users/heronrs/Documents/registros_coords.csv')

dados = melt(registros_coords, id=c('Gestao', 'Estado', 'Municipio', 'Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', 'lat', 'lon','lon_mun', 'lat_mun'), variable.name="Ano", value.name = "Valor")
rm(registros_coords)

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
  geom_point(aes(x=lon, y=lat, size=1, text=description, color=Valor)) +
  scale_color_viridis(option="inferno", trans="log10", begin=1, end=0) +
  scale_alpha_continuous(trans="log10") +
  labs(y=NULL, x=NULL) +
  theme_void() +
  theme(legend.position = 'none')

p <- ggplotly(map_oss, tooltip="text")
htmlwidgets::saveWidget(as_widget(p), "mapa_oss.html")



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


rm(dados)
rm(dados_mun_cg)
rm(dados_oss_cg)
rm(shp_df)
rm(shp)
rm(map_municipio)
rm(map_oss)

