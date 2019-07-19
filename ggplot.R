getwd()
setwd("C:/Users/heronrs/Downloads")
getwd()

movies <- read.csv("Movie-Ratings.csv")    
colnames(movies) <- c('Film', 'Genre', 'Critic_Rating', 'Audience_Rating', 'Budget_Millions', 'Year')
head(movies)
str(movies)

#---------------Factor-----------------
#Need to be sure what will do with from the data before using factor()

#Transform column Year into factors, we wont use the numbers of each year then we can turn them into factors
factor(movies$Genre)
movies$Year <- factor(movies$Year)
summary(movies)

#--------------Asthetics---------------

library(ggplot2)

ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating))

#add geometry
ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating)) + 
  geom_point()
  
#add colour
ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating, color=Genre)) + 
  geom_point()

#add size
ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating, 
                        color=Genre, size=Budget_Millions)) + 
  geom_point()

#-------- Plotting with layers --------

p <- ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating, 
                             color=Genre, size=Budget_Millions))

#Plot with points
p + geom_point()

#Plot with lines
p + geom_line()

#Multiple layers
p + geom_point() + geom_line()

#------ Overriding asthetics ----------

q <- ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating,
                             color=Genre, size=Budget_Millions))
q + geom_point()

#ex1:
q + geom_point(aes(size=Critic_Rating))

#ex2
q + geom_point(aes(color=Budget_Millions))

#q remains the same
q + geom_point()

#ex3
q + geom_point(aes(x=Budget_Millions)) #Overriding the axis doesnt change the name that we put first
q + geom_point(aes(x=Budget_Millions)) + xlab("Budget Millions $$") #Changing the name

#ex4
p + geom_point() + geom_line()
#reduce line size
p + geom_point() + geom_line(size=1)

#--------- Mapping vs Setting --------

r <- ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating))
r + geom_point()

#add color

#Mapping    -- To map something, use aes. To set something, dont use aes
r + geom_point(aes(color=Genre))  #Map a color to a variable
#Setting
r + geom_point(color="DarkGreen")   # Just set the color
#Error
r + geom_point(aes(color="DarkGreen"))
r + geom_point(color=Genre)

#Mapping
r + geom_point(aes(size=Budget_Millions))
r + geom_point(size=10)
#Error
r + geom_point(aes(size=10))

#---- Histograms and density charts -----

s <- ggplot(data=movies, aes(x=Budget_Millions))
s + geom_histogram(binwidth=10)

#add color
s + geom_histogram(binwidth=10, aes(fill=Genre))
#add a border
s + geom_histogram(binwidth=10, aes(fill=Genre), color='black')

#density chart
s + geom_density(aes(fill=Genre))
s + geom_density(aes(fill=Genre), position = 'stack')

#------- Layer tips ----------

t <- ggplot(data=movies, aes(x=Audience_Rating))
t + geom_histogram(binwidth=10, fill="White", color='blue')

#another way
t <- ggplot(data=movies)
t + geom_histogram(binwidth=10, 
                   aes(x=Critic_Rating),
                   fill="White", color='blue')

#---- Statistical transformation ---

u <- ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating, color=Genre))
u + geom_point() + geom_smooth(fill=NA)

#boxplots
u <- ggplot(data=movies, aes(x=Genre, y=Audience_Rating, color=Genre))
u + geom_boxplot()
u + geom_boxplot(size=1.2)
u + geom_boxplot(size=1.2) + geom_jitter()
u + geom_jitter() + geom_boxplot(size=1.2, alpha=0.2)

u <- ggplot(data=movies, aes(x=Genre, y=Critic_Rating, color=Genre))
u + geom_jitter() + geom_boxplot(size=1, alpha=0.2) 

x <- ggplot(data=movies, aes(x=Genre))
x + geom_bar()

#------------- Facets --------------

v <- ggplot(data=movies, aes(x=Budget_Millions))
v + geom_histogram(binwidth=10, aes(fill=Genre), color='black')

#facets
v + geom_histogram(binwidth=10, aes(fill=Genre), color='black') +
  facet_grid(Genre~., scales='free')

#scatterplot
w <- ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating, color=Genre))
w + geom_point(size=3)
w + geom_point(size=3) + facet_grid(Genre~.)
w + geom_point(size=3) + facet_grid(.~Year)
w + geom_point(aes(size=Budget_Millions)) + 
    geom_smooth(fill=NA) +
    facet_grid(Genre~Year) 

#---------- Coordinates --------------

m <- ggplot(data=movies, aes(x=Critic_Rating, y=Audience_Rating,
                             size=Budget_Millions, color=Genre))
m + geom_point()

#Limits - expecify what we want to see
m + geom_point() + xlim(50,100) + ylim(50, 100)
#Wont work well always
t <- ggplot(data=movies, aes(x=Budget_Millions))
t + geom_histogram(binwidth=10, aes(fill=Genre), color='Black')
t + geom_histogram(binwidth=10, aes(fill=Genre), color='Black') + ylim(0, 50)   #Was cut more than we asked for
#Instead - zoom
t + geom_histogram(binwidth=10, aes(fill=Genre), color='Black') + coord_cartesian(ylim=c(0,50))
#Make the same for the other plot
m + geom_point()+ coord_cartesian(ylim=c(50,100), xlim=c(50,100))

w + geom_point(aes(size=Budget_Millions)) + 
  geom_smooth() +
  facet_grid(Genre~Year) +
  coord_cartesian(ylim=c(0,100))

#----------- Theme --------------

?theme

o <- ggplot(data=movies, aes(x=Budget_Millions))
h <- o + geom_histogram(binwidth=10, aes(fill=Genre), color="Black")

#axes labels
h + xlab("Money axis") + ylab("Number of movies") 
#label formating
h + xlab("Money axis") + ylab("Number of movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=30),
        axis.title.y = element_text(color="Blue", size=30))
#tick mark formating
h + xlab("Money axis") + ylab("Number of movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Blue", size=20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))
#legend  formating
h + xlab("Money axis") + ylab("Number of movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Blue", size=20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1))
#text
h + xlab("Money axis") + ylab("Number of movies") +
  ggtitle("Movies Budget Distribution") + 
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Blue", size=20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="DarkBlue", size=30, family="Courier"),
        )











