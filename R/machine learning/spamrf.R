library(randomForest)
library(kernlab)
library(ggplot2)
library(shiny)
library(dplyr)
library(caret)
library(RColorBrewer)
library(colorspace)

data(spam)
#4601 x 58

## -----------------------------------------
##
## Description du jeu de données
##
## -----------------------------------------

# Coup d'oeil aux premières lignes du dataset : 
head(spam)

#Statistiques descriptives
summary(spam)

# Dimensions
dim(spam)

# Le jeu de données classifie les 4601 e-mails et les marque comme spam et non-spam.
# Il se base sur la présence de certains caracètes spéciaux, des parties d'adresses, des
# numéros et des mots.
# Le but sera de construire un algorithme qui permet de juger si un mail reçu est classifié
# en tant que spam.

# Variables quantitatives et qualitatives
sapply(spam, class)

# Quasiment toutes les variables sont quantitatives (compte de caractères) excepté une seule
# variable qui est type

# valeurs de type
levels(spam$type)

y = spam$type
X = spam[,1:57]

## -----------------------------------------
##
## Analyse exploratoire des données
##
## -----------------------------------------


ggplot(data = spam, mapping = aes(x = telnet, y = type)) +
  geom_boxplot()

# barplot 
barplot(spam$make, names.arg = spam$type, xlab ='type', ylab='make', col='blue')    
# correlation 
cor.test(spam$mail, spam$make)


resTable <- table(spam$type)
par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2) + 0.1)
CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10, "Set3"))
plot <- plot(spam$type, col =CUSTOM_COLORS_PLOT(2), main = "Spam vs. Nospam",
             ylim = c(0, 4000), ylab = "Examples Number")
text(x = plot, y = resTable + 200, labels = resTable)
percentage <- round(resTable/sum(resTable) * 100)
labels <- paste(row.names(resTable), percentage)  # add percents to labels
labels <- paste(labels, "%", sep = "")  # ad % to labels
pie(resTable, labels = labels, col = CUSTOM_COLORS_PLOT(2), main = "Spam vs. Nospam")

## -----------------------------------------
##
## Data preprocessing
##
## -----------------------------------------

## Gestion des NAN
# Aucun NaN à déclarer

## Outliers
# Normalisation
X.s <- as.data.frame(scale(X)) 
spam.s <- cbind(X.s, y)

# On filtre les données en-dehors de l'intervalle [-1, 1]
# X.no_outliers <- filter(spam.s[,1:57], scale(spam.s[,1:57])>-1 & scale(spam.s[,1:57])<1)
# y.no_outliers <- filter(spam.s[,58], scale(spam.s[,1:57])>-1 & scale(spam.s[,1:57])<1)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(y, p=0.80, list=F)
# select 20% of the data for validation
y.test <- y[-validation_index]
X.s.test <- X.s[-validation_index,]
# use the remaining 80% of data to training and testing the models
y.train <- y[validation_index]
X.s.train <- X.s[validation_index,]

# Apply to the dataset as well
spam.s.train <- spam.s[validation_index,]
spam.s.test <- spam.s[-validation_index,]

## -----------------------------------------
##
## Modélisation
##
## -----------------------------------------

spam.rf <- randomForest(y ~ ., data=spam.s.train, importance=TRUE,proximity=TRUE)

# TODO: Accuracy en fonction du paramétrage

# Attention aux variables bruitées : ne prendre qu'un subset de variables

## -----------------------------------------
##
## Analyse des performances
##
## -----------------------------------------

