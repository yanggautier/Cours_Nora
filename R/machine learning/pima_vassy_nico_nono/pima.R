# LOAD LIBRARY
library(mlbench)
library(ggplot2)
data(PimaIndiansDiabetes)
pima <- PimaIndiansDiabetes

# Description du jeu de données - voir fichier Markdown

# Analyse exploratoire

# Donner les statistiques descriptives du jeu de données
summary(PimaIndiansDiabetes)


# Faire les analyses univariées via ggplot2

ggplot(PimaIndiansDiabetes,aes(diabetes,fill = diabetes)) + geom_bar() + 
  ggtitle("Negative/Positive Number of Diabetes")

# Faire les analyses bivariées (croisement des variables candidates avec le critère à modéliser, matrice des corrélations, barplots,...)

# Age and diabetes
ggplot(PimaIndiansDiabetes, aes(age,fill = diabetes)) + geom_bar(aes(group=diabetes))+ 
  ggtitle("Negative/Positive Number of Diabetes by age")

# BloodPressure and diabetes
ggplot(PimaIndiansDiabetes, aes(pressure)) + geom_bar(aes(group=diabetes)) + facet_wrap(~diabetes)+ 
  ggtitle("Blood Pressure and diabetes")

# Mass and diabetes
ggplot(PimaIndiansDiabetes, aes(mass,fill = diabetes))+ geom_bar(aes(group=diabetes))+ 
  ggtitle("Mass vs Diabetes")

# Glucose and diabetes
ggplot(PimaIndiansDiabetes, aes(glucose,fill = diabetes))+ geom_bar(aes(group=diabetes))+ 
  ggtitle("Glucose vs Diabetes")

# Pedigree and diabetes
p1 <- ggplot(PimaIndiansDiabetes, aes(x = diabetes, y = pedigree,fill = diabetes)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Pedigree Vs Diabetes")

# Insulin and diabetes
p2 <- ggplot(PimaIndiansDiabetes, aes(insulin, fill = diabetes)) +
  geom_histogram(binwidth=10) +
  theme(legend.position = "bottom") +
  ggtitle("Insulin Vs Diabetes")

gridExtra::grid.arrange(p1, p2, ncol = 2)

# Pregnancies Vs diabetes
p1 <- ggplot(PimaIndiansDiabetes,aes(x = pregnant,fill = factor(diabetes))) + 
  geom_bar(position = "Dodge") +
  theme(legend.position = "bottom") +
  labs(title = "Pregnancies Vs Diabetes")

# Skin Thickness Vs Diabetes 
p2 <- ggplot(PimaIndiansDiabetes, aes(triceps,fill = diabetes)) +
  geom_histogram() +
  theme(legend.position = "bottom") +
  ggtitle("Triceps Vs Diabetes")

gridExtra::grid.arrange(p1, p2, ncol = 2)

# matrix of correlations between the variables

# Relationship of Insulin with Glucose Vs Diabetes
ggplot(PimaIndiansDiabetes, aes(x = glucose, color = diabetes, fill = diabetes)) +
  geom_density(alpha = 0.8) +
  theme(legend.position = "bottom") +
  labs(x = "Glucose", y = "Density", title = "Relationship of Density with Glucose Vs Diabetes")

# Relationship of Pregnancies with Age Vs Diabetes
ggplot(PimaIndiansDiabetes, aes(x = age, y = pregnant)) +
  geom_point(aes(color=diabetes)) + 
  theme(legend.position = "bottom") +
  ggtitle("Relationship of Pregnancies with Age Vs Diabetes")

# Relationship of Insulin with Glucose Vs Diabetes
ggplot(PimaIndiansDiabetes,aes(x=insulin,y=glucose))+
  geom_point(aes(color=diabetes))+
  theme(legend.position = "bottom") +
  ggtitle("Relationship of Insulin with Glucose Vs Diabetes")

# Compute correlation matrix
library(corrplot)
db_cor <- cor(PimaIndiansDiabetes[1:8])
db_cor
corrplot(db_cor, method="number", type = "lower")
# A moderate correlation (0.54) is observed between Pregnancies and Age. 
# The intensity of the colour shows that correlation between variables is weaker or stronger.
# No strong correlation observed between variables. So, no need to drop any of them for analysis


# Data preprocessing

df <- PimaIndiansDiabetes
df
# Nos valeurs numeriques : glucode, pressure, triceps, insulin, mass
class(df$glucose)
class(df$pressure)
class(df$triceps)
class(df$insulin)
class(df$mass)
class(df$age)
# -----------------------------------------------------------------
# si nos valeurs etaient integer, ils auraient fallut les convertir de la sorte
# df$glucose <- as.numeric(df$glucose)
# -----------------------------------------------------------------
# df$age <- as.integer(df$age)
# class(df$age)
# -----------------------------------------------------------------

# Valeures manquantes
zero_value <- list(Column = colSums(df==0), Row = sum(rowSums(df==0)))
zero_value
# Output
#pregnant  glucose  pressure  triceps  insulin     mass     pedigree    age      diabetes 
#111        5       35        227      374         11        0          0        0 

# output : pour tricep : 30% de 0, pour insulin 49% de 0

#Pregnancies
#A woman can have 0 pregnancy but keeping zeros for our model can  false it so we use the bin technics
df$pregnant <- ifelse(df$pregnant==0, "No", "Yes") %>% factor()
summary(df$pregnant)

#insulin : to much zero values so we suppress the column
df$insulin <- NULL

#tripceps : to much zero values so we suppres the column
df$triceps <- NULL

#mass : only 11 zeros values so we put value in clusters/bins
df$mass <- ifelse(df$mass<19,'Underweight',ifelse(df$mass>=19 & df$mass<=25, "Normal", ifelse(df$mass>=25 & df$mass<=30, "Overweight","Obese"))) %>% factor(levels=c("Underweight","Normal", "Overweight","Obese"))
list(BMI = summary(df$mass))
# output
# Underweight      Normal      Overweight       Obese 
# 15              108         180              465 

#glucose 
# The unit of measurement for the 2-hour OGTT in this dataset is assumed to be in milligrams per deciliter (mg/dl). 
# It can be converted to Milimoles per liter (mmol/l) so that we may apply a qualitative test result to the numeric results. 
# Multiplying the current results by 0.0555 will convert them to be measured in mmol/l.
df$glucose <- df$glucose*0.0555
df$glucose <- ifelse(df$glucose<2.2,'Hypoglycemia',ifelse(df$glucose>=2.2 & df$glucose <=7.8, 'Normal', ifelse(df$glucose >= 7.8 & df$glucose<=11.1,'Prediabetes','Diabetes'))) %>% factor()
list(Result = summary(df$glucose))
list(Result = summary(df$glucose)/length(df$glucose))
#Output
#Hypoglycemia       Normal  Prediabetes 
#5                  571          192 

# Blood pressure
# When measuring Blood Pressure, two measures are used:
# * Systolic - Measures the pressure in blood vessels when the heart beats. 
# * Diastolic - Measures the pressure in blood vessels when the heart rests between beats.
# We have only of these two values and we need both to determine if a person is normal or high blood pressure 
# so we suppres this column
df$pressure <- NULL

#Final df
summary(df)
df


# SUITE : DATASPLIT
# set.seed -> equivalent du random forest pour garder les memes split
set.seed(1000)
train_index<- sample(1:nrow(df), 0.8*nrow(df))
test_index <- setdiff(1:nrow(df), train_index)
train <- df[train_index,] 
test <- df[test_index,]
list(train=summary(train), test=summary(test))

# Modélisation avec Random Forests

# get required library
require(randomForest)

# Random Forest explained

# RandomForest(formula, ntree=n, mtry=FALSE, maxnodes = NULL)
# Arguments:
# - Formula: Formula of the fitted model
# - ntree: number of trees in the forest
# - mtry: Number of variables randomly sampled as candidates at each split. By default, it is the square of the number of columns.
# - maxnodes: Set the maximum amount of terminal nodes in the forest
# - importance=TRUE: Whether independent variables importance in the random forest be assessed

# fitting of the model
# first arg is a formula : diabetes ~ . first term is the target / second term is the features
# in this case, diabetes is the target, . character is all the features we have in the train data

# default model
pima.rf=randomForest(diabetes ~ ., na.action = na.roughfix, data = train)
print(pima.rf)


# Grid Search 
# we need the caret library to use the K folder cross validation
# we need caret to evaluate model too

fitControl <- trainControl(
  method = "cv",
  number = 10, 
  search ="grid")

set.seed(825)

# The random numbers are the same, and they would continue to be 
# the same no matter how far out in the sequence we went.

# You will use caret library to evaluate your model.
# The library caret has one function called train() to evaluate almost all machine learning algorithm. 

# the hyper-parameters we want to find: mtry, ntree and maxnodes

# base model for comparision
# ntree: 500
# mtry : sqrt of nb of features

mtry = sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)

rfFit <- train(diabetes ~ ., data = train, 
                 method = "rf", 
                 trControl = fitControl,
                 tuneGrid = tunegrid,
                 metric = "Accuracy",
                 importance = TRUE,
                 maxnodes= 20)
rfFit

# default accuracy : 76%

# PB with Caret : 
# The choice of parameters is left to the developers of the package.
# Only those algorithm parameters that have a large effect 
# (e.g. really require tuning in Khun’s opinion) 
# are available for tuning in caret.
# As such, only mtry parameter is available in caret for tuning. 
# The reason is its effect on the final accuracy and that it must be found 
# empirically for a dataset.

# The ntree parameter is different in that it can be as large as you like, 
# and continues to increases the accuracy up to some point. 
# It is less difficult or critical to tune and could be limited more by compute 
# time available more than anything.

# find best mtry
tuneGrid_1 <- expand.grid(.mtry = c(1: 10)) # mtry between 1 and 10

rf_mtry <- train(diabetes ~ ., data = train, 
                 method = "rf", 
                 trControl = fitControl,
                 tuneGrid = tuneGrid_1,
                 metric = "Accuracy",
                 importance = TRUE,
                 maxnodes= 20)
rf_mtry

# best mtry = 2

# we store the best value of mtry so we can find the best number for ntree and maxnodes
best_mtry <- rf_mtry$bestTune$mtry
best_mtry

# best max nodes according to the best mtry
store_maxnode <- list()

tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(diabetes ~ .,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = fitControl,
                      importance = TRUE,
                      maxnodes = maxnodes)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_nodes <- resamples(store_maxnode)
summary(results_nodes)
# store the best values for maxnodes 
best_maxnodes <- 6

# best number of trees according to the mtry and max_nodes
store_maxtrees <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (ntree in c(250, 300, 350, 400, 500, 550)) {
  set.seed(5678)
  rf_maxnode <- train(diabetes ~ .,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = fitControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = best_maxnodes,
                      ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxnode
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

best_ntree <- 300

# mtry = 2
# maxnodes = 6
# ntree = 300
# final model

tunegrid <- expand.grid(.mtry=best_mtry)

final_rf <- train(diabetes ~ .,
                data = train,
                method = "rf",
                trControl = fitControl,
                tuneGrid = tunegrid,
                metric = "Accuracy",
                ntree = best_ntree,
                maxnodes = best_maxnodes)

final_rf

# predictions
predsTrain <- predict(final_rf, newdata=train)
predsTrain


# confusion matrix & statistics with Caret
confusionMatrix(predsTrain, train$diabetes)

# sensitivity : 0.8679
# specificity : 0.6272
# accuracy : 0.7785

# Generate ROC Curves

predictionDiabetes <- prediction(as.numeric(predsTrain), as.numeric(train$diabetes))
predictionDiabetes

perf <- performance(predictionDiabetes, "tpr", "fpr")
perf

plot.new()
plot(perf, col = "green") 
abline(0, 1, 
       col = "grey")

auc <- performance(predictionDiabetes, "auc")

legend("bottomright", 
       paste(round(as.numeric(auc@y.values), digits = 2)), 
       col = c("green"),
       pch = c(3))

