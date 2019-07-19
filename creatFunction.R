#Creating function
myplot <- function(data, player=1:10){  #Set the params, put a default one
  Data <- data[player,,drop=F]  
  matplot(t(Data), type='b', pch=15:18, col=c(1:4,6))
  legend('bottomleft', inset=0.01, legend=Players[player], col=c(1:4,6), pch=15:18)
}
myplot(MinutesPlayed, 2)
myplot(Salary)  #Will use the default param
