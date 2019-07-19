#Analise extratos Caixa

library(readr)
library(data.table)
library(lubridate)

#Define o diret�rio onde est�o os arquivos
setwd("C:/Users/heronrs/Documents/Dados Extratos/")

#Obs.: No arquivo "GECAM_2016 - 06 a 10A", possui um header nas linhas 9933 e 9934

#Vetor com os nomes arquivos
files_list = list.files()

#Cria data frame com os dados de cada extrato, de acordo com a formata��o dos arquivos
dados = data.frame()
for(i in files_list){
  print(paste("Lendo", i, sep = " "))
  arquivo <- read.fwf(i , c(28, 22, 7, 21, 21, 7, 8, 7, 21, 21, 11), skip=2, 
                   col.names=c('data_movimenta��o', 'valor', 'agdeb', 'contadeb', 'cpf1deb', 'canal', 'bcocred', 'agcred', 'contacred', 'cpf1cred', 'finalidade'))
  print(nrow(arquivo))
  dados <- rbind.data.frame(dados,arquivo)
  rm(arquivo)
}

#Convertendo a coluna data_movimenta��o de factor para date
dados$data_movimenta��o <- ymd_hms(dados$data_movimenta��o)
#Subsetting com base na data inicial de vig�ncia do TAC (04/setembro/2017)
dados <- dados[dados$data_movimenta��o >= ymd("2017-09-04"),]

#options(digits=20)
#data[1,5]

str(dados)














