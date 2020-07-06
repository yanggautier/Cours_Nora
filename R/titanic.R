# importer les librairy nécessaire
library(rpart)
library(mlr3data)
library(ggplot2)
library(caret)
# 1.
data(titanic)
titanic

head(titanic)
#On cherche à prédire si un individu est survie ou pas

#Nombre de ligne
nrow(titanic)

#Nombre de feature
ncol(titanic)

#Afficher les type de données des colonnes
sapply(titanic, class)

# Le target est le Survived
# Les variables candidates sont pclass, sex, age, sib_sp, parch, fare, cabin ,embarked

mode(titanic$survived)
mode(titanic$pclass)
mode(titanic$name)
mode(titanic$sex)
mode(titanic$sib_sp)
mode(titanic$age)
mode(titanic$parch)
mode(titanic$ticket)
mode(titanic$fare)
mode(titanic$cabin)
mode(titanic$embarked)

# 2.

#Donner les statistiques descriptives du jeu de données

summary(titanic)

titanic$sex <- factor(titanic$sex)
titanic$sex <- as.numeric(titanic$sex)

titanic$embarked <- factor(titanic$embarked)
titanic$embarked <- as.numeric(titanic$embarked)

titanic$pclass <- factor(titanic$pclass)
titanic$pclass <- as.numeric(titanic$pclass)

titanic$survived <- factor(titanic$survived)
titanic$survived <- as.numeric(titanic$survived)

titanic$parch <- as.numeric(titanic$parch)

titanic$sib_sp <- as.numeric(titanic$sib_sp)

titanic$age[is.na(titanic$age)] <- mean(titanic$age[!is.na(titanic$age)])

#titanic$ticket[is.na(titanic$ticket)]

#Faire les analyses univariées à l’aide de représentations graphiques obtenuesvia ggplot2 
#(histogramme des variables quantitatives,répartition des variablesqualitatives,...


ggplot(data=titanic, aes(x=titanic$age)) +  geom_histogram(color="darkblue", fill="lightblue")
ggplot(data=titanic, aes(x=titanic$fare)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(data=titanic, aes(x=titanic$parch)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(data=titanic, aes(x=titanic$sib_sp)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(data=titanic, aes(x=titanic$sex)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(data=titanic, aes(x=titanic$pclass)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(data=titanic, aes(x=titanic$embarked)) + geom_histogram(color="darkblue", fill="lightblue")
ggplot(data=titanic, aes(x=titanic$survived)) + geom_histogram(color="darkblue", fill="lightblue")

#Faire les analyses bivariées (croisement des variables candidates avec lecritère à modéliser,
#matrice des corrélations, barplots,...)
boxplot(titanic$age ~ titanic$pclass, xlab = "pclass", ylab = "age")

ggplot(data=titanic[1:nrow(titanic),],aes(x=sex,fill=survived))+geom_bar()
ggplot(data=titanic[1:nrow(titanic),],aes(x=pclass,fill=survived))+geom_bar()
ggplot(data=titanic[1:nrow(titanic),],aes(x=age,fill=survived))+geom_bar()
ggplot(data=titanic[1:nrow(titanic),],aes(x=sib_sp,fill=survived))+geom_bar()
ggplot(data=titanic[1:nrow(titanic),],aes(x=parch,fill=survived))+geom_bar()
ggplot(data=titanic[1:nrow(titanic),],aes(x=embarked,fill=survived))+geom_bar()


titanic_prepro <- titanic[c('survived','sex','age','pclass','parch','sib_sp','fare','embarked')]
titanic_prepro

#library(Hmisc)
#mcor <-cor(as.matrix(titanic_prepro))

#library(corrplot)
#corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

sapply(titanic_prepro, class)
titanic$survived

#3.Data Preprocessing
#Identifier par variable les missing values, les outliers (correction à apporter?en fonction de leurs proportions)


#Afficher un graph avec les valeurs manquantes vs les valeurs observées
library(Amelia)
missmap(titanic_train, main = "Missing values vs observed")


sapply(titanic_prepro,function(x) sum(is.na(x)))

dim(titanic_prepro)
unique(titanic_prepro$survived)

titanic_pre2 <- subset(titanic_prepro,!is.na(titanic_prepro$survived))
titanic_pre2$fare[is.na(titanic_pre2$fare)] <- mean(titanic_pre2$fare[!is.na(titanic_pre2$fare)])
titanic_pre2$embarked[is.na(titanic_pre2$embarked)] <- 3

titanic_pre2
sapply(titanic_pre2,function(x) sum(is.na(x)))


cor(titanic_pre2)

library(Hmisc)
mcor <-cor(as.matrix(titanic_pre2))

library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black")


#Data-splitting (80% training/ 20% validation). Pour la classification, est attendue un sampling stratifié (pour tenir de la proportion de 0 et de 1 dans latarget) → utilisation du package caret

set.seed(101)


sample <- sample.int(n = nrow(titanic_pre2), size = floor(.8*nrow(titanic_pre2)), replace = F)
train <- titanic_pre2[sample, ]
test  <- titanic_pre2[-sample, ]


set.seed(3456)
trainIndex <- createDataPartition(titanic_pre2$survived, p = .8, 
                                  list = FALSE, 
                                  times = 1)
titanicTrain <- titanic_pre2[ trainIndex,]
titanicTest  <- titanic_pre2[-trainIndex,]

y_train <- titanicTrain$survived
X_train <- titanicTrain[-train$survived]
y_test <- titanicTest$survived
X_test <- titanicTest[-train$survived]



fit <- rpart(X_train ~ pclass + sex + age + sib_sp + parch + fare + embarked,
             data=X_train,
             method="class")
plot(fit)
text(fit)
