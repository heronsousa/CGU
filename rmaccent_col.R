rm_accent <- function(str,pattern="all") {
  # Rotinas e fun��es �teis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Fun��o que tira todos os acentos e pontua��es de um vetor de strings.
  # Par�metros:
  # str - vetor de strings que ter�o seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos dever�o ser retirados.
  #            Para indicar quais acentos dever�o ser retirados, um vetor com os s�mbolos dever�o ser passados.
  #            Exemplo: pattern = c("�", "^") retirar� os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que s�o "�", "`", "^", "~", "�", "�")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="�"))
    pattern[pattern=="�"] <- "�"
  
  symbols <- c(
    acute = "������������",
    grave = "����������",
    circunflex = "����������",
    tilde = "������",
    umlaut = "�����������",
    cedil = "��"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("�","`","^","~","�","�")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}

registros_coords$Municipio <- rm_accent(registros_coords$Municipio)

registros_coords$Nome_da_Unidade_de_Sa�de <- rm_accent(registros_coords$Nome_da_Unidade_de_Sa�de)
names(registros_coords)[1] <- "Nome_da_Unidade_de_Saude"
names(registros_coords)[3:6] <- c("2016", "2017", "2018", "2019")

write.table(registros_coords, 'registros_oss.csv', sep=',', row.names=FALSE)
