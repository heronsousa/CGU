#Homework 5

library(ggplot2)

#Set Director and import the file
setwd("C:\\Users\\heronrs\\Downloads")
stats_excel <- read.csv("Section5-Homework-Data.csv")

#Create matrice with all rows that is equal to 1960 or 2013 on the column 'Year'
data1960 <- stats_excel[stats_excel$Year==1960,]
data2013 <- stats_excel[stats_excel$Year==2013,]

#Create data frame with the vectors Country_Code and Life_Expectancy
vector1960 <- data.frame(Country_Code, Life_Expectancy_At_Birth_1960)
vector2013 <- data.frame(Country_Code, Life_Expectancy_At_Birth_2013)

#Merge
stats1960 <- merge(data1960, vector1960, by.x='Country.Code', by.y='Country_Code')
stats2013 <- merge(data2013, vector2013, by.x='Country.Code', by.y='Country_Code')

#Plot
qplot(data=stats1960, x=Fertility.Rate, y=Life_Expectancy_At_Birth_1960, color=Region)
qplot(data=stats1960, x=Fertility.Rate, y=Life_Expectancy_At_Birth_2013, color=Region)  
