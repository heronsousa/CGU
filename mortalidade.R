library(reshape2)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(plyr)

mortalidade <- read.csv("~/scripts/mortalidade1.csv")

# Retirando os caracteres 'tx_' de cada nome da coluna dos anos
colunas <- names(mortalidade)
colunas[4:13] <- str_sub(colunas[4:13], start=4)
names(mortalidade) <- colunas

# Guardando os nomes das três primeiras colunas, do df mortalidade, no vetor "colunas"
colunas <- colunas[1:3]

# Separando os anos e valores em colunas, mantendo as colunas do vetor "colunas"
mortalidade = melt(mortalidade, id=colunas, variable.name="Ano", value.name = "Valor")

macrorregioes <- read.csv2("~/scripts/macrorregioes.csv")

temp <- mortalidade %>% 
  inner_join(macrorregioes, by=c("macroregiao" = "macrorregiao"))

temp <- temp[,c(4,5,6,9,11)]
temp <- temp[!duplicated(temp),]

regiao <- function(estado){
  filtro <- temp %>%
    filter(uf==estado)
  
  # Plotando o gráfico
  plot <- ggplot(filtro, aes(x=Ano, y=Valor, group=no_macrorregiao)) +
            geom_line(aes(color=no_macrorregiao), size=1) +
            geom_point(aes(color=no_macrorregiao), size=2) +
            coord_cartesian(xlim = c(-2, 13)) +
            # Definindo a qual macroregiao corresponde cada linha do gráfico
            # Filtrando pelo ano 2008, a primeira coluna, labels estarão a esquerda
            geom_text_repel(data=filtro %>% filter(Ano=='2008'),
                            aes(label = no_macrorregiao),
                            fontface='bold',
                            nudge_x = -2,
                            direction = "y") +
            # Filtrando pelo ano 2017, a ultima coluna, labels estarão a direita
            geom_text_repel(data=filtro %>% filter(Ano=='2017'),
                            aes(label = no_macrorregiao) ,
                            fontface = "bold",
                            nudge_x = 2,
                            direction = "y") +
            theme(legend.position="none") +
            labs(title=filtro$uf)
  
  print(plot)
}

for(uf in unique(temp$uf))
  regiao(uf)

