setwd("C:/Users/heronrs/Documents/Arquivos csv/")

#Diretório onde estão os arquivos csv
diretorio <- "C:/Users/heronrs/Documents/Arquivos csv/"

#Nome do arquivo de saída
saida <- 'Arquivos.csv'

filenames <- list.files(diretorio, pattern='*.csv', full.names=TRUE)

i=0
for(files in filenames){
  arquivo_csv <- read.csv2(files)
  print(paste("Lendo",files,sep = " "))
  if(i==0){
    write.table(arquivo_csv,saida, sep=';', row.names=FALSE, na='NA')
  }
  else{
    write.table(arquivo_csv,saida, sep=';',append=TRUE, row.names=FALSE, col.names=FALSE, na='NA')
  }
  rm(arquivo_csv)
  i=i+1
}
