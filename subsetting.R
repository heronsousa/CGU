#Subsetting

Games
Games[1:3,6:10]
Games[c(1,10),]

max(Games[,1])
min(Games[,1])
Games[,c('2007','2009')]

#Drop
Games[1,]
Games[1,,drop=F]  #'drop' ignore useless dimension, TRUE as default
Games[1,5]
Games[1,5, drop=F]

Data <- MinutesPlayed[1:3,]
matplot(t(Data), type='b', pch=15:18, col=c(1:4,6))
legend('bottomleft', inset=0.01, legend=Players[1:3], col=c(1:4,6), pch=15:18)

Data <- MinutesPlayed[1,]
matplot(t(Data), type='b', pch=15:18, col=c(1:4,6))
legend('bottomleft', inset=0.01, legend=Players[1], col=c(1:4,6), pch=15:18)
Data <- MinutesPlayed[1,,drop=F]
matplot(t(Data), type='b', pch=15:18, col=c(1:4,6))
legend('bottomleft', inset=0.01, legend=Players[1], col=c(1:4,6), pch=15:18)