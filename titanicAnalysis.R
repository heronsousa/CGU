library(ggplot2)
library(stringr)

setwd("C:/Users/heronrs/Downloads/")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
data_combined <- rbind(train, test.survived)

str(data_combined)

data_combined$Pclass <- as.factor(data_combined$Pclass)
data_combined$Survived <- as.factor(data_combined$Survived)

table(data_combined$Survived)
table(data_combined$Pclass)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x=Pclass, fill=factor(Survived))) +
  geom_bar(width=0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill="Survived")

length(unique(as.character(data_combined$Name)))

#Verifica qual são nomes que estão duplicados
dup <- as.character(data_combined[which(duplicated(as.character(data_combined$Name))), 'Name'])
dup

data_combined[which(data_combined$Name %in% dup),]

?str_detect

misses <- data_combined[which(str_detect(data_combined$Name, "Miss")),]
misses[1:5,]

mrses <- data_combined[which(str_detect(data_combined$Name, "Mrs")),]
mrses[1:5,]

males <- data_combined[which(train$Sex == "male"),]
males[1:5,]









