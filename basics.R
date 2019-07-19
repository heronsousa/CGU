?seq    #Help para a função

seq()   #Sequencia vazia com valor 1 como default

seq(3, 5)   #seq(from=3, to=5)

seq(3, length=4)  #Inicio e tamanho da sequencia

seq(3, length=4, by=0.5)  #Inicio, tamanho e passo

#Cria vetor de caractere
?paste

x = paste(1:4); x
class(x)

#Concatena string com vetores
paste("xyz", 1:10)
paste("xyz", c(2,1,5, "teste, 5.4"))
paste("xyz", 1:10, sep="") #'sep' determina algo entre o primeiro e o segundo valor da função paste

#Repetir
?rep

rep(x, times=3) #Repete o vetor x 3 vezes

rep(x, each=3) #Repete cada valor do vetor 3 vezes

rep(x, each=3, times=3) #Combina 

z <- 4:20; z

which(z == 10)  #Retorna a posição do valor o vetor

z[3]