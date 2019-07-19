#Import data
?read.csv()

stats <- read.csv(file.choose())
stats

getwd()   #Return the directory
setwd("C:\\Users\\heronrs\\Downloads")  #Set directory
rm(stats)
stats <- read.csv("DemographicData.csv")
stats

#------------- Exploring data -------------------------

stats
nrow(stats)   #Number of the rows
ncol(stats)
head(stats)   #Show the first 6 rows
head(stats, n=10)
tail(stats)   #The last 6 rows
tail(stats, n=1)
str(stats)    #Return the structure of the data from the file
summary(stats)    #Stats from the data
 

#----------------- Using $ sign -----------------------

head(stats)
stats[3,3]
stats[3,"Birth.rate"]
stats$Internet.users    #Access a column and turns it into a vector
stats$Internet.users[2]   #Access a value of this new vector
str(stats)
levels(stats$Income.Group)    #Return the levels from one specific column

#---------- Basic operations with data frame -------------

#Subsetting
stats[1:10,]    
stats[3:9,]
stats[c(4,100),]

#[]
stats[1,]
is.data.frame(stats[1,]) #Dont need drop=F, still a data frame even with just 1 row
stats[,1]
is.data.frame(stats[,1])
is.data.frame(stats[,1, drop=F])  #For column, need drop=F

#Operation with columns
stats$Birth.rate + stats$Internet.users
stats$Birth.rate * stats$Internet.users

#Add column
stats$MyCalc <- stats$Birth.rate + stats$Internet.users
stats$idk <- 1:195
stats

#Remove columns
stats$MyCalc <- NULL
stats$idk <- NULL

#---------- Filtering data frames ---------------------

head(stats)
filter <- stats$Internet.users>95   #In the column Internet.users from stats, verify if each value is greater than 95 and return a vector of TRUE and FALSE
stats[filter,]    #Will print just the rows that is TRUE on the vector 'filter'
stats[stats$Birth.rate>40,]
stats[stats$Birth.rate>40 & stats$Internet.users<2,]  #Combine conditions
stats[stats$Country.Name=='Brazil',]

#------------------ qplot() --------------------------

library(ggplot2)
qplot(data=stats, x=Internet.users)
qplot(data=stats, x=Income.Group, y=Birth.rate, size=I(3))
qplot(data=stats, x=Income.Group, y=Birth.rate, size=I(3), color=I('blue'))
qplot(data=stats, x=Income.Group, y=Birth.rate, geom='boxplot')

#---------------- What we need ---------------------

qplot(data=stats, x=Internet.users, y=Birth.rate)
qplot(data=stats, x=Internet.users, y=Birth.rate, size=I(4))
qplot(data=stats, x=Internet.users, y=Birth.rate, size=I(3), color=Income.Group)  #Each group have a color on the graphic
qplot(data=stats[stats$Country.Name=='Brazil',], x=Internet.users, y=Birth.rate, size=I(3), color=Income.Group)
  
#-------------- Create data frame ----------------------

mydf <- data.frame(Countries_2012_Dataset, Codes_2012_Dataset, Regions_2012_Dataset)  #Similar to the function cbind, will put these vector into a data frame
head(mydf)
#colnames(mydf) <- c('Country', 'Code', 'Region')
mydf <- data.frame(Country=Countries_2012_Dataset, Code=Codes_2012_Dataset, Region=Regions_2012_Dataset)  #Country=Countries_2012_Dataset in the data frame the vector Countries_2012_Dataset will have the name Country. Same for the others vectors

#------------- Merge data frames --------------

head(mydf)
head(stats)
merged <- merge(stats, mydf, by.x='Country.Code', by.y='Code')    #Merge two data frames in a new data frame, taking by reference one columns from each data frame that has the same content ("by.x=''"- the column name of the first data frame we declared, "by.y=''"- the column name of the second data frame we declared)
head(merged)
merged$Country <- NULL    #Remove column duplicated

#------------ Visualizing the new data frame ----------

qplot(data=merged, x=Internet.users, y=Birth.rate, color=Region)
#Shape
qplot(data=merged, x=Internet.users, y=Birth.rate, color=Region, size=I(3), shape=I(18))
#Transparance
qplot(data=merged, x=Internet.users, y=Birth.rate, color=Region, size=I(3), shape=I(19), alpha=I(0.6))
#Title
qplot(data=merged, x=Internet.users, y=Birth.rate, color=Region, size=I(3), shape=I(19), alpha=I(0.6), main='Birth rate x Internet users')



