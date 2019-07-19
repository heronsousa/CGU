vector <- c(1, 2, 1, 4,  87, 7, 54)
is.double(vector)

vetor <- c(1L, 2L)
is.integer(vetor)


d <- rep(3, 50)   #Repete (valor, vezes)
seq(1, 15, 3)   #Inicio, fim, passo

w <- c('a', 'b', 'c', 'd', 'e')
w

w[-1]   #Acessa todos os elementos, exceto o primeiro
v = w[-3]   #Acessa todos os elementos, exceto o terceiro
w[1:3]
w[3:5]
w[c(1,3,5)]
w[c(-1,-5)]

x <- rnorm(5)

for(i in x){
  print(i)
}

for(j in 1:5){
  print(x[j])
}

n = 100
a = rnorm(n)
b = rnorm(n)

#Two ways to multiply vector:
c = a*b

d <- rep(NA, n)   #Create and alocating   an empty vector
for(i in 1:n){
  d[i] = a[i]*b[i]
}



