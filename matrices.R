#Matrices
#Indexation:
#Matriz[Rows, Coloumns]      Matriz[Rows,]       Matriz[,Columns]

#matrix()
?matrix
my_data <- 1:20
my_data
A <- matrix(my_data, nrow = 4, ncol = 5)  #Fill the number by col by default
A <- matrix(my_data, 4, 5); A
B <- matrix(my_data, 4, 5, byrow = T); B   #Specify to fill the matrice by row

#rbind() -- Group rows into a matrice
?rbind
r1 <- c('Heron', 'Rodrigues', 'Sousa')
r2 <- c(24,10,1998)
r3 <- c('Quase', 20, 'anos')
C <- rbind(r1,r2,r3); C

#cbind() -- Merge coloumns into a matrice
?cbind
c1 <- 1:5
c2 <- -1:-5
D <- cbind(c1,c2); D


#Named vector
idk <- 1:5; idk

#Names
names(idk)  #Check the names of the vector
names(idk) <- c('a','b','c','d','e'); idk   #Give name for each columns
idk['b']
names(idk) <- NULL  #Remove the names
idk

temp_vec <- rep(c('a','B','zZ'), each= 3); temp_vec
sla <- matrix(temp_vec, 3, 3, byrow = T); sla
rownames(sla) <- c('How', 'are', 'you?')
colnames(sla) <- c('I', 'dont', 'know')
sla[1,1] <- 'q'
sla['How', 'I'] <- 'asd'
sla