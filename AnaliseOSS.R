#Analise das respostas dos Formulários OSS

install.packages('httpuv', type='binary')
install.packages('shiny', type='binary')

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(maps)
library(mapproj)
library(viridis)
library(ggrepel)
library(plotly)
library(shiny)


options(scipen=999)
options(digits = 10)

#Define o diretório onde estão os arquivos
setwd("C:/Users/heronrs/Documents/sla/")

#Vetor com os nomes arquivos
files_list = list.files()

#Cria data frame com os dados das respostas recebidas
registros = data.frame()

for(i in files_list){
  
  arquivo <- read_excel(i, col_names=c('Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', '2016', '2017', '2018', 'Latitude', 'Longitude'),
                        col_types=c('guess', 'guess', 'numeric', 'numeric', 'numeric', 'guess', 'guess'), skip = 1)
  
  #Define à qual estado e municipio pertence cada registro, de acordo com o nome do arquivo
  #Nome do arquivo
  nome_arq <- sub("\\..*", "", i)
  #Retira do nome do arquivo a sigla do estado
  estado <- substr(nome_arq, 0, 2)
  #Retira do nome do arquivo o municipio
  municipio <- substr(nome_arq, 4, nchar(nome_arq))
  
  #Cria uma nova coluna "Estado", replicando sua sigla pelo numero de registros que há no arquivo
  arquivo$Estado <- rep(estado, nrow(arquivo))
  #Cria uma nova coluna "Municipio", replicando seu nome pelo numero de registros que há no arquivo
  arquivo$Municipio <- rep(municipio, nrow(arquivo))
  
  registros <- rbind.data.frame(registros,arquivo)
  rm(arquivo)
}

#Transforma a coluna Estado em factor
registros$Estado <- factor(registros$Estado)

#Reformatando as colunas dos anos com os valores usando a função 'melt' do pacote 'reshape2'
dados = melt(registros, id=c('Estado', 'Municipio', 'Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', 'Latitude', 'Longitude'), variable.name="Ano", value.name = "Valor")

w <- registros %>% count(Estado)

sum_ano <- group_by(dados, Ano) %>% summarize(Valor = sum(Valor, na.rm=TRUE))
sum_estado <- group_by(dados, Estado) %>% summarize(Valor = sum(Valor, na.rm=TRUE))

#Plotando valor pelo ano
x <- ggplot(dados, mapping=aes(x=Ano, y=Valor))

ggplot(sum_ano, aes(x=Ano, y=Valor, fill=Ano)) +
  geom_bar(stat = "identity", colour='black') +
  geom_text(aes(label=format(round(as.numeric(Valor), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle('Montante dos valores das OSS por ano') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Valor (em R$)') +
  theme_bw()

#Plotando valor por estado
y <- ggplot(sum_estado, aes(x=Estado, y=Valor, fill=Estado))

y + geom_bar(stat = "identity", colour='black') +
  scale_y_log10(breaks=c(10^6, 10^7, 10^8, 10^9, 10^10)) +
  coord_cartesian(ylim = c(10^6, 10^7, 10^8, 10^9, 10^10)) +
  geom_text(aes(label=format(round(as.numeric(Valor), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.4), vjust=-0.25) +
  ggtitle('Montante dos valores das OSS por estado') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Valor (em R$)') +
  theme_bw()

#Plotando quantidade de OSS em cada estado
ggplot(data=w, aes(x=Estado, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Estado, xend=Estado, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantidade de OSS por estado", y=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()





rm(registros)
rm(sum_ano)
rm(sum_estado)
rm(x)
rm(y)
rm(w)
rm(merged)
rm(estado)
rm(files_list)
rm(i)

dados_cg <- dados %>%  filter(dados$Latitude!=0)

oss <- group_by(dados_cg, Estado, Nome_da_Unidade_de_Saúde, CNPJ_da_OSS, Latitude, Longitude) %>% summarize(Valor = sum(Valor, na.rm=TRUE))

Br <- map_data("world") %>% filter(region=="Brazil")



p = oss %>%
  arrange(Valor) %>%
  mutate( description=paste("Nome: ", Nome_da_Unidade_de_Saúde, "\n", "Valor: ", Valor, sep="")) %>%
  ggplot() +
  geom_polygon(data=Br, aes(x=long, y=lat, group=group), color='black', fill="grey", alpha=0.3) +
  geom_point(aes(x=Longitude, y=Latitude, size=Valor, text=description, color=Valor, alpha=Valor)) +
  scale_size_continuous(range=c(5,8)) +
  scale_color_viridis(option="inferno", trans="log10" ) +
  scale_alpha_continuous(trans="log10") +
  ggtitle('Localização de cada unidade com seu respectivo valor (em R$)') +
  ylab('Latitude') +
  xlab('Longitude') +
  theme(axis.text = element_text(colour='black'),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw()

p=ggplotly(p, tooltip="text")
p



dados_gerais_estados <- data.frame()
dados_gerais_estados <- as.data.frame(dados_gerais_estados)
colnames(dados_gerais_estados) <- c('Estado', 'Resposta_do_primeiro_email', 'Resposta_no_limesurvey', 'Possui_OSS')
dados_gerais_estados$Estado <- c('Acre', 'Alagoas', 'Amapá', 'Amazonas', 'Bahia', 'Ceará', 'Distrito Federal', 'Espírito Santo','Goias', 'Maranhão', 
                                 'Mato Grosso', 'Mato Grosso do Sul', 'Minas Gerais', 'Pará', 'Paraíba', 'Paraná', 'Pernambuco', 'Piauí', 'Rio de Janeiro', 
                                 'Rio Grande do Norte', 'Rio Grande do Sul', 'Rondônia', 'Roraima', 'Santa Catarina', 'São Paulo', 'Sergipe', 'Tocantins')

dados_gerais_estados$Resposta_do_primeiro_email <- c('Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Não', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Não', 'Não', 'Não', 'Sim', 'Não', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Não')
dados_gerais_estados$Resposta_no_limesurvey <-     c('Não', 'Sim', 'Sim', 'Não', 'Sim', 'Não', 'Sim', 'Sim', 'Sim', 'Sim', 'Não', 'Não', 'Sim', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Sim', 'Não', 'Sim', 'Não', 'Não', 'Sim', 'Não')
dados_gerais_estados$Possui_OSS <-                 c('Sem resposta','Sim', 'Sim', 'Sem resposta','Sim', 'Não', 'Sim', 'Sim', 'Sem resposta','Sem resposta','Sem resposta','Sem resposta','Não','Sem resposta', 'Sem resposta','Sem resposta','Sem resposta','Sem resposta','Sem resposta','Sem resposta','Não', 'Sem resposta','Sim', 'Sem resposta', 'Não', 'Não', 'Sem resposta')



ggplot(data=dados_gerais_estados, aes(x=Resposta_no_limesurvey)) +
  geom_bar() +
  geom_text(aes(label=format(round(as.numeric(count(Resposta_no_limesurvey)), 2))), position=position_dodge(width=0.4), vjust=-0.25) +
  theme_bw()

q <- group_by(dados_gerais_estados, Resposta_do_primeiro_email) %>% count(Resposta_do_primeiro_email)
ggplot(data=q, aes(x=Resposta_do_primeiro_email, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Resposta_do_primeiro_email, xend=Resposta_do_primeiro_email, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Respostas no primeiro contato", y=NULL, x=NULL) +
  xlab('') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()


z <- group_by(dados_gerais_estados, Resposta_no_limesurvey) %>% count(Resposta_no_limesurvey)
ggplot(data=z, aes(x=Resposta_no_limesurvey, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Resposta_no_limesurvey, xend=Resposta_no_limesurvey, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Respostas do formulário no limesurvey", y=NULL, x=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

s <- group_by(dados_gerais_estados, Possui_OSS) %>% count(Possui_OSS)
ggplot(data=s, aes(x=Possui_OSS, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Possui_OSS, xend=Possui_OSS, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantos estados possuem OSS", y=NULL, x=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()







