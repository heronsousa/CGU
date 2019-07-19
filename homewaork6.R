library(ggplot2)

setwd('C:/Users/heronrs/Documents/Leitura de arquivo R/')

movie = read.csv('Section6-Homework-Data.csv')

filt <- (movie$Genre=='action') | (movie$Genre=='adventure') | (movie$Genre=='animation') | (movie$Genre=='comedy') | (movie$Genre=='drama')
filt2 <- (movie$Studio=='Buena Vista Studios') | (movie$Studio=='Fox') | (movie$Studio=='Paramount Pictures') | (movie$Studio=='Universal') | (movie$Studio=='WB')
  
movie_filt <- movie[filt & filt2,]

movie_plot <- ggplot(data = movie_filt, aes(x=Genre, y=Gross...US)) + geom_jitter(aes(color=Studio, size=Budget...mill.)) + geom_boxplot(alpha=0.4, outlier.colour = NA)
movie_plot + xlab('Genre') + ylab('Gross % US') + 
            ggtitle('Domestic Gross % by Genre') +
            theme(axis.title.x = element_text(colour = 'Blue', size=20),
                  axis.title.y = element_text(colour = 'Blue', size=20),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  plot.title = element_text(size=30)
            )

