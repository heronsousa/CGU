library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)

#Define o diretório onde estão os arquivos
setwd("C:/Users/heronrs/Documents/Respostas OSS/")

#Vetor com os nomes arquivos
files_list = list.files()

#Cria data frame com os dados das respostas recebidas
sla = data.frame()
for(i in files_list){
  arquivo <- read_excel(i, col_names=c('Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', '2016', '2017', '2018'),
                        col_types=c('guess', 'skip', 'numeric', 'numeric', 'numeric'), skip = 1)
  #Define à qual estado pertence cada registro, de acordo com o nome do arquivo
  #Retira do nome do arquivo a sigla do estado
  #estado <- substr(i, 0, 2)
  #Cria uma nova coluna "Estado", replicando sua sigla pelo numero de registro que há no arquivo
  #arquivo$Estado <- rep(estado, nrow(arquivo))
  sla <- rbind.data.frame(sla,arquivo)
  rm(arquivo)
}

#rm(estado)
rm(files_list)
rm(i)



newDados = melt(sla, id='Nome_da_Unidade_de_Saúde', variable.name="Ano", value.name = "Valor")

newNewDados = reshape(newDados, v.names='Valor', idvar='Ano', timevar='Nome_da_Unidade_de_Saúde', direction = 'wide')













