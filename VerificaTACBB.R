library(utils)
library(iotools)
library(RMySQL)
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)
options(scipen=999)
options(OutDec=",")

# essa funçao foi criada para dar overwrite na funçao input.file
# do pacote iotools, que estava dando problema na chamada do método
# dstrfw(...). Por isso setamos os parâmetros no código até a função
# retornar o DF com os valores carregados.
input.file <- function(file_name, formatter = mstrsplit, ...) {
  if (is.character(file_name)) {
    input = file(file_name, "rb")
    on.exit(close(input))
  } else {
    stop("'file_name' must be a character string to a file path.")
  }
  
  n = file.info(file_name)$size
  dstrfw(readBin(input, what="raw", n=n), widths = c(1,1,9,6,12,4,50,14,8,3,50,20,17,17,14,1,3,4,12,34),
         col_types = c('character', 'character','character','character','character','character','character',
                       'character', 'character','character','character','character','numeric', 'numeric', 
                       'character','character', 'character', 'character','character', 'character'), nsep = NA, strict=FALSE, skip=1)
}



#Dados dos extratos BB, cedidos pelo Jorge Rodrigo (FNS/MS)
caminho_raiz <- 'C:/Users/hugoaal/Downloads/Dados Extratos/'
txt_files_list = list.files(caminho_raiz,recursive = TRUE, pattern = '*debcgu',include.dirs = TRUE)
dados = data.frame()
# repetição para ler todos os arquivos da pasta
for(i in seq_along(txt_files_list)){
  arquivo <- paste0(caminho_raiz,txt_files_list[i])
  #base <- read.fwf(arquivo, widths = c(1,1,9,6,12,4,50,14,8,3,50,20,17,17,14,1,3,4,12,34), col.names = c('tipo_registro','tipo_conta', 'numero_contrato', 'numero_convenio', 'conta', 'agencia', 'nome_agencia', 'CNPJ_titular_conta', 'data_movimentacao','tipo_movimentacao', 'historico_movimentacao', 'numero_documento_movimentacao', 'valor_movimentacao', 'saldo_conta_movimentacao','CNPJ_CPF_beneficiario','tipo_beneficiario', 'banco_beneficiario', 'agencia_beneficiario', 'conta_beneficiario', 'filler'), skip=1, colClasses= c('character','character','character','character','character','character','character','factor', 'character','character','character','character','numeric', 'numeric', 'factor','character', 'character', 'character','character', 'character' ))
  base <- input.file(arquivo)
  #Pega a última linha do arquivo lido (linha=trailler)
  trailler <- base[nrow(base),]
  # Faz o parse da última linha (trailler) conforme a quantidade de 
  #caracteres definidas no layout, copiando até o caractere 35 da
  #última linha
  trailler$ajuste <- paste0(trailler$V1, trailler$V2, 
                            trailler$V3, trailler$V4, 
                            trailler$V5, trailler$V6, 
                            substr(trailler$V7, 1,2))
  trailler$tipo_registro <- substr(trailler$ajuste, 1,1)
  trailler$quantidade_registro <- as.numeric(substr(trailler$ajuste, 2,18))
  trailler$valor_total <- as.numeric(substr(trailler$ajuste, 19,35))
  trailler$ajuste <- NULL
  #Testa se a quantidade de registros lidos, excluindo-se o último que é trailller,
  # é igual o campo quantidade de registros do trailler
  # Se nao for, printa a inconsistência
  if ((nrow(base)-1) != trailler$quantidade_registro) {
    cat(paste0('[PROBLEMA] Quantidade de registros lidos no arquivo ', 
                 arquivo,' = ', as.character(nrow(base)-1) , 
                 ' é diferente da quantidade indicada no trailler ', trailler$quantidade_registro ))
  }
  #retira do DF o último campo, que o trailler
  base <- base[-nrow(base),]
  #acrescenta o nome do arquivo na coluna V21 do DF
  base$V21 <- txt_files_list[i]
  # acrescenta no DF geral (dados) o DF específico (base)
  dados <- rbind.data.frame(dados,base)
  cat(paste0('finalizado a leitura do arquivo ', txt_files_list[i], '.\n'))
}
# limpa a memória, apagando o DF temporário
rm(base)
rm(trailler)
# Nomeia as colunas do DF com as respectivas correspondências 
# das colunas do layout
names(dados) <- c('EX_tipo_registro','EX_tipo_conta', 'EX_numero_contrato', 
                  'EX_numero_convenio', 'EX_conta', 'EX_agencia', 'EX_nome_agencia',
                  'EX_CNPJ_titular_conta', 'EX_data_movimentacao','EX_tipo_movimentacao', 
                 'EX_historico_movimentacao', 'EX_numero_documento_movimentacao', 'EX_valor_movimentacao',                   
                 'EX_saldo_conta_movimentacao','EX_CNPJ_CPF_beneficiario','EX_tipo_beneficiario', 'EX_banco_beneficiario', 
                'EX_agencia_beneficiario', 'EX_conta_beneficiario', 'EX_filler', 'EX_arquivo_origem')
#convertendo a coluna EX_data_movimentacao de character 
#para data (pacote lubridate)
dados$EX_data_movimentacao <- ymd(dados$EX_data_movimentacao)
#subsetting com base na data inicial de vigência do TAC (04/setembro/2017)
dados <- dados[dados$EX_data_movimentacao >= ymd("2017-09-04"),]
#ajustando a coluna EX_valor_movimentacao de character para numeric
dados$EX_valor_movimentacao <- as.numeric(paste0(substr(dados$EX_valor_movimentacao, 1, nchar(dados$EX_valor_movimentacao)-2),".", substr(dados$EX_valor_movimentacao, nchar(dados$EX_valor_movimentacao)-1, nchar(dados$EX_valor_movimentacao))))

#Verificando o primeiro item do TAC 
#Saques com valor maior que 800,00
# selecionando as linhas de saque

saques_maior_800 <- dados[grepl("*SAQUE*", dados$EX_historico_movimentacao),]
#retirando as linhas de tarifa
saques_maior_800 <- saques_maior_800[!grepl("*TARIFA*", saques_maior_800$EX_historico_movimentacao),]

#retirando as linhas que contenham 'SAQUE EM POUPANCA',
# que na verdade são os resgates de poupança para a conta corrente
saques_maior_800 <- saques_maior_800[!grepl("*POUPANCA*", saques_maior_800$EX_historico_movimentacao),]

#retirando os saques menores que 800 reais
saques_maior_800 <- saques_maior_800[saques_maior_800$EX_valor_movimentacao>=800,]
#agrupando por mês e sumarizando isso por valor
resultado_saques_maior_800 <- saques_maior_800 %>% group_by(month=floor_date(EX_data_movimentacao, "month")) %>%
  summarize(amount=sum(EX_valor_movimentacao), n = n())
#excluindo dia da data
resultado_saques_maior_800$month <- format(as.Date(resultado_saques_maior_800$month), "%Y-%m")

#plotando por mes e valor
ggplot(data=resultado_saques_maior_800, aes(x=factor(month), y=amount, colour=month, fill=month)) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label=format(round(as.numeric(amount), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_grey(start = 0, end = .9) +
  ggtitle("Vedação 1 - Saques maiores que R$800,00") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab( "Mês" ) +
  ylab( "Valor total")
  
#plotando por mês e número de saques
ggplot(data=resultado_saques_maior_800, aes(x=factor(month), y=n, colour=month, fill=month)) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_grey(start = 0, end = .9) +
  ggtitle("Vedação 1 - Saques maiores que R$800,00") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab( "Mês" ) +
  ylab( "Número de Saques Irregulares")

#plotando por mês as médias dos valores sacados
ggplot(data=resultado_saques_maior_800, aes(x=factor(month), y=(amount/n), colour=month, fill=month)) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label=format(round(as.numeric(amount/n), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_grey(start = 0, end = .9) +
  ggtitle("Vedação 1 - Saques maiores que R$800,00") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab( "Mês" ) +
  ylab( "Valor médio do Saque Irregular")

# Partindo para a segunda obrigação do TAC
# Todos os saques devem ter identificação (CNPJ ou CPF)
# do beneficiário ou de quem realiza o saque
saques_sem_identificacao <- dados[grepl("*SAQUE*", dados$EX_historico_movimentacao),]
saques_sem_identificacao <- saques_sem_identificacao[!grepl("*TARIFA*", saques_sem_identificacao$EX_historico_movimentacao),]
#retirando as linhas que contenham 'SAQUE EM POUPANCA',
# que na verdade são os resgates de poupança
saques_sem_identificacao <- saques_sem_identificacao[!grepl("*POUPANCA*", saques_sem_identificacao$EX_historico_movimentacao),]
# retirando os casos em que há a identificação do beneficiário
saques_sem_identificacao <- saques_sem_identificacao[(!grepl('([0-9]{2}[\\.]?[0-9]{3}[\\.]?[0-9]{3}[\\/]?[0-9]{4}[-]?[0-9]{2})|([0-9]{3}[\\.]?[0-9]{3}[\\.]?[0-9]{3}[-]?[0-9]{2})', saques_sem_identificacao$EX_CNPJ_CPF_beneficiario) | saques_sem_identificacao$EX_CNPJ_CPF_beneficiario == '00000000000000' ),]

# agrupando por mês e somando os casos
resultado_saques_sem_identificacao <- saques_sem_identificacao %>% group_by(month=floor_date(EX_data_movimentacao, "month")) %>%
  summarize(amount=sum(EX_valor_movimentacao), n= n())

#excluindo dia da data
resultado_saques_sem_identificacao$month <- format(as.Date(resultado_saques_sem_identificacao$month), "%Y-%m")
#plotando por mês e número de saques
ggplot(data=resultado_saques_sem_identificacao, aes(x=factor(month), y=n, colour=month, fill=month)) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_grey(start = 0, end = .9) +
  ggtitle("Vedação 2 - Saques sem identificação") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab( "Mês" ) +
  ylab( "Quantidade de Saques")

#plotando por mês e somatório dos valores dos saques sem identificação
ggplot(data=resultado_saques_sem_identificacao, aes(x=factor(month), y=amount/1000000, colour=month, fill=month)) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label=round(amount/1000000, digits=3)), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_grey(start = 0, end = .9) +
  ggtitle("Vedação 2 - Saques sem identificação") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab( "Mês" ) +
  ylab( "Valor total (em milhões)")

#plotando por mês a o valor médio dos saques sem identificação
ggplot(data=resultado_saques_sem_identificacao, aes(x=factor(month), y=amount/n, colour=month, fill=month)) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label=format(round(as.numeric(amount/n), 2), nsmall=1, big.mark=".")), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_grey(start = 0, end = .9) +
  ggtitle("Vedação 2 - Saques sem identificação") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab( "Mês" ) +
  ylab( "Valor médio do Saque sem Identificação")

#Verificando a terceira vedação: Saídas de Recursos para
# CNPJs cuja natureza jurídica seja 1203, 1301, 1201, 1236, 1244

# Primeiramente, vamos acrescentar a natureza jurídica de cada
# beneficiário ao dataframe
#removendo as linhas cujo EX_CNPJ_CPF_beneficiario é nulo
dados_com_natureza_juridica_beneficiario <- dados[!is.na(dados$EX_CNPJ_CPF_beneficiario), ]
#removendo as linhas cujo EX_CNPJ_CPF_beneficiario é vazio ou só tem espaço
dados_com_natureza_juridica_beneficiario <- dados_com_natureza_juridica_beneficiario[!str_trim(dados_com_natureza_juridica_beneficiario$EX_CNPJ_CPF_beneficiario)=='',]
#pegando todos os CNPJS únicos (distintos)
cnpjs <- unique(dados_com_natureza_juridica_beneficiario$EX_CNPJ_CPF_beneficiario)
#preparando para incluir como parâmetro na query
cnpjs <- paste0("'",as.character(cnpjs), "'")
cnpjs <- paste(cnpjs, ",", sep ="", collapse = "\n")
cnpjs <- substr(cnpjs, 1,nchar(cnpjs)-1)

queryNaturezaJuridica <- paste0("SELECT CNPJ.CodNaturezaJuridica, CNPJ.Cnpj 
                                FROM CNPJv2.CNPJ CNPJ
                                WHERE (CNPJ.Cnpj IN (", cnpjs, "))") 

conBDI <- dbConnect(dbDriver("MySQL"), user="hugoaal", password="", dbname="CNPJv2",
                    host="esfinge")

resultadoNaturezaJuridica <- dbSendQuery(conBDI, queryNaturezaJuridica)
resultadoNaturezaJuridica <- fetch(resultadoNaturezaJuridica, n = -1) 
rm(queryNaturezaJuridica)
rm(cnpjs)
#incluindo as naturezas jurídicas na minha base
dados_com_natureza_juridica_beneficiario <- merge(dados_com_natureza_juridica_beneficiario, resultadoNaturezaJuridica, by.x = "EX_CNPJ_CPF_beneficiario", by.y = "Cnpj", all.x = TRUE)
colnames(dados_com_natureza_juridica_beneficiario)[22] <- "CNPJ_Cod_Natureza_Juridica_Beneficiario"
naturezas_juridicas_vedadas <- c('1203', '1031', '1201', '1236', '1244')
#filtrando apenas as linhas que contenham as naturezas jurídicas vedadas
dados_com_natureza_juridica_beneficiario <- filter(dados_com_natureza_juridica_beneficiario, CNPJ_Cod_Natureza_Juridica_Beneficiario %in% naturezas_juridicas_vedadas)
#resetando os levels do factor após o subsetting
#(lembrando que no R, depois de um subset, os levels se mantêm).
dados_com_natureza_juridica_beneficiario$EX_historico_movimentacao <- factor(dados_com_natureza_juridica_beneficiario$EX_historico_movimentacao)


# Carga dos dados dos CNPJs de todos os orgaos e entidades da APU nos 3 níveis da Federação
# SQL = Select * From CNPJ Where CodNaturezaJuridica in 
# (1015,1023,1031,1040,1058,1066,1074,1082,1090,1104,1112,1120,1139,1147,
# 1155,1163,1171,1180,1198,1201,1210,1228,1236,1244,1252,1260,1279,2011,2038) 
path.CNPJ <-'C:/Users/1511 MXTI/Documents/Teletrabalho/Tarefa XXX - Aprendizado R/Dados/CNPJ/CNPJ_APU.csv'
CNPJ.publicos <- fread(path.CNPJ, 
                       header=TRUE,
                       sep=",",
                       colClasses = rep("character", 32))

#Convertendo meus grandes DF para Large ffdf (sai da memória e vai para disco)
dados[sapply(dados, is.character)] <- lapply(dados[sapply(dados, is.character)], 
                                      as.factor)
dados <- as.ffdf(dados)
CNPJ.publicos[sapply(CNPJ.publicos, is.character)] <- lapply(CNPJ.publicos[sapply(CNPJ.publicos, is.character)], 
                                             as.factor)
CNPJ.publicos <- as.ffdf(CNPJ.publicos)
names(CNPJ.publicos) <- paste0('CNPJ_', names(CNPJ.publicos))

# Agregando os dados da tabela CNPJ no DF dos extratos
summary(dados)
nrow(merged.dados)
merged.dados <- merge.ffdf(dados, CNPJ.publicos, by.x='CNPJ_titular_conta', by.y= 'Cnpj', all.x=TRUE)

#Carregando agora os dados de todas as Organizaçoes Sociais
# SQL = Select * From CNPJ Where CodNaturezaJuridica IN (3301,3042) 
path.dados.os <- 'C:/Users/1511 MXTI/Documents/Teletrabalho/Tarefa XXX - Aprendizado R/Dados/CNPJ/CNPJ_OrganizacaoSocial.csv'
CNPJ.dados.os <- fread(path.dados.os, 
                       header=TRUE,
                       sep=",",
                       colClasses = rep("factor", 32))
CNPJ.dados.os <- as.ffdf(CNPJ.dados.os)
#Salvando para continuar amanhã...
save.ffdf(CNPJ.dados.os,CNPJ.publicos,dados, merged.dados, 
          dir='C:/Users/1511 MXTI/Documents/Teletrabalho/Tarefa XXX - Aprendizado R/Dados/ffdf',
          overwrite = TRUE)
load.ffdf(dir='C:/Users/1511 MXTI/Documents/Teletrabalho/Tarefa XXX - Aprendizado R/Dados/ffdf')

#testando os beneficiários que são OS
temp <- ffmatch(dados$CNPJ_CPF_beneficiario, CNPJ.dados.os$Cnpj)
merged.dados$beneficiario_is_OS <- dados$CNPJ_CPF_beneficiario %in% CNPJ.dados.os$Cnpj

hist.ff(merged.dados$CodNaturezaJuridica, breaks=nrow(merged.dados$CodNaturezaJuridica))
nrow(merged.dados$CodNaturezaJuridica[is.na.ff(merged.dados$CodNaturezaJuridica)])
unicos_cnpj_beneficiarios <- unique.ff(merged.dados$CNPJ_CPF_beneficiario)[]
unicos_cnpj_beneficiarios <- as.vector(unicos_cnpj_beneficiarios)
NROW(unicos_cnpj_beneficiarios)

#length(dados$CNPJ_CPF_beneficiario[dados$CNPJ_CPF_beneficiario %in% CNPJ.dados.os$Cnpj])

#ffwhich(dados, dados$CNPJ_CPF_beneficiario == CNPJ.dados.os$Cnpj)
# Buscando os beneficiários das transações que estão registrados como OS na base do CNPJ
#idx.CNPJ.beneficiarios.os <- ffwhich(CNPJ.dados.os)


save.image("~/Teletrabalho/Tarefa XXX - Aprendizado R/bb.RData")
