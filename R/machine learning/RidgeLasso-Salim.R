#install.packages('mlbench')
#install.packages('glmnet')
#install.packages('corrplot')

# Nous utilisons le dataset Boston Housing, qui contient des donn�es sur les biens immobiliers � Boston.
# Nous cherchons � pr�dire MEDV.
# Nous allons d�terminer le nombres d\'observations et le nombres de variables sur toutes les colonnes sauf MEDV.
# La variable � pr�dire (target) est MEDV. Les variables candidates (features) sont toutes les autres.
# Elles sont toutes quantitatives sauf CHAS (1 si l\'�tendue d�limite la rivi�re ; 0 sinon)

library(mlbench)
library(glmnet)
library(ggplot2)

# Importation du dataframe
data(BostonHousing)
Boston <- BostonHousing

head(Boston)
nrow(Boston)
ncol(Boston)

# Comptage des valeurs nulles
sum(is.na(Boston))

summary(Boston)
summary(Boston$medv)

######################################## Corr�lation ###############################################

# S�lectionner uniquement les valeurs quantitatives du dataframe pour la matrice de corr�lation
bostonQuanti <- Filter(is.numeric, Boston)
head(bostonQuanti)
library(corrplot) # Cr�er la matrice de corr�lation
corrMatrice <- cor(bostonQuanti)
corrplot(corrMatrice, type="upper", method="number")

#Le coefficient de corr�lation varie de -1 � 1. Si la valeur est proche de 1, 
#cela signifie qu\'il existe une forte corr�lation positive entre les deux variables.
#Lorsque la valeur est proche de -1, les variables ont une forte corr�lation n�gative.


# De la matrice de corr�lation, nous pouvons voir qu'il existe 
# des niveaux significatifs de corr�lation entre medv, lstat(-0.74) et rm(0.7)
medv <- Boston$medv
ggplot(Boston) + geom_point(aes(x=lstat, y=medv))
ggplot(Boston) + geom_point(aes(x=rm, y=medv))

# Les prix augmentent � mesure que la valeur de RM augmente lin�airement. 
# Il y a peu de valeurs aberrantes et les donn�es semblent �tre plafonn�es � 50. 
# Les prix ont tendance � baisser avec une augmentation de LSTAT.

# Visualisons la distribution et la densit� du r�sultat, MEDV.
# La courbe noire repr�sente la densit�.
# Nous voyons que la valeur m�diane du prix du logement est asym�trique vers la droite, avec un certain nombre de valeurs aberrantes vers la droite.
ggplot(Boston, aes(x=medv)) + geom_histogram(aes(y=..density..), bins=30, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")


################################### Train model ##################################################
# set.seed(100) # Random state for reproducibility
# 
# index = sample(1:nrow(Boston), 0.8*nrow(Boston)) # index for partitioning
# 
# bostonTrain = Boston[index,] # Create the training data
# bostonTest = Boston[-index,] # Create the test data
# 
# fit.lm <- lm(medv~., bostonTrain)
# summary(fit.lm)
# data.frame(coef = round(fit.lm$coefficients,2))
# 
# #predict on test set
# pred.lm <- predict(fit.lm, bostonTest)
# 
# # Root-mean squared error
# rmse.lm <- sqrt(sum((pred.lm - bostonTest$medv)^2)/length(bostonTest$medv))
# 
# # Tableau de r�sultat RMSE & R�
# c(RMSE = rmse.lm, R2 = summary(fit.lm)$r.squared)

################################# Ridge Regression ########################################
set.seed(100) # Random state for reproducibility

index = sample(1:nrow(Boston), 0.8*nrow(Boston)) # index for partitioning

bostonTrain = Boston[index,] # Create the training data
bostonTest = Boston[-index,] # Create the test data

# Predictor variables
x <- model.matrix(medv~., bostonTrain)[,-1]
# Outcome variable
y <- bostonTrain$medv

# Find the best lambda using cross-validation
set.seed(100) 
cv <- cv.glmnet(x, y, alpha = 0)
# Display the best lambda value
cv$lambda.min
cv

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

# Make predictions on the test data
library(dplyr) # Ce package pour %>%
x.test <- model.matrix(medv ~., bostonTest)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()

# Model performance metrics
#install.packages("Metrics")
library(Metrics) # Ce package pour RMSE
r2 <- lm(medv~., bostonTrain)
c(RMSE = rmse(predictions, bostonTest$medv), R2 = summary(r2)$r.squared)

################################# Lasso Regression ########################################
#La seule diff�rence entre le code R utilis� pour la r�gression de ridge est que, 
#pour la r�gression de lasso, vous devez sp�cifier 
#l'argument alpha = 1 au lieu de alpha = 0 (pour la r�gression de ridge).

# Find the best lambda using cross-validation
set.seed(100) 
cv1 <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv1$lambda.min
cv1

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv1$lambda.min)
# Dsiplay regression coefficients
coef(model)

# Make predictions on the test data
x.test <- model.matrix(medv ~., bostonTest)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
r2Lasso <- lm(medv~., bostonTrain)
c(RMSE = rmse(predictions, bostonTest$medv), R2 = summary(r2Lasso)$r.squared)
