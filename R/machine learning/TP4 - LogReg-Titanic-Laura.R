# https://www.masaldzhiyski.com/data-processing/titanic-kaggle/
# https://www.hackerearth.com/fr/practice/machine-learning/machine-learning-algorithms/logistic-regression-analysis-r/tutorial/

#install.packages("glmnet", repos = "http://cran.us.r-project.org")
#install.packages("titanic")
#install.packages("data.table",repos="http://R-Forge.R-project.org")
#install.packages("InformationValue")
library(glmnet)
library(stats)
library(ggplot2)
library(data.table)
library(mltools)
library(InformationValue)

################### Régression Logistique & Régularisée (Lasso/Ridge)
library(titanic)
data("titanic_train")
data("titanic_test")

#titanic_train <- read.csv("/train.csv")
#titanic_test <- read.csv("/test.csv")

################### Exploratory Data Analysis
View(titanic_train)
dim(titanic_train)
head(titanic_train)
str(titanic_train)
summary(titanic_train)

#### Check missing values
colSums(is.na(titanic_train))
colSums(is.na(titanic_test))

#### Quick Data Exploration
summary(titanic_train$Age)
summary(titanic_test$Age)

summary(titanic_train$Fare)
summary(titanic_test$Fare)

ggplot(titanic_train, aes(x=Survived)) + geom_bar()

table(titanic_train$Survived, titanic_train$Sex)
# There are 891 passangers in the training data,
# comprising of 314 female and 577 male passengers,
# out of which 233 females and 109 males survived the disaster.

# The dataset provides information about each passenger’s
# name, age, gender, their port of embarkation, which cabin they booked,
# their ticket number and fare they paid along with number of family members they were travelling with.

#### Did age influence survival?
ggplot(titanic_train, aes(x=Age, y=PassengerId, color = as.factor(Survived))) +                      
  geom_point() + 
  facet_grid(Sex ~.) +
  ggtitle("Survival vs Passenger's Age")+
  xlab("Age") + 
  theme(legend.position = "none")+
  scale_colour_manual(values = c("#FF0000","#0000FF"))

#### Is survival of a passenger related to his/her Pclass and port of embarkation?
ggplot(titanic_train[titanic_train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Pclass) +
  ggtitle("Survival vs Passenger's Pclass and Port of Embarkation")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#FF0000","#0000FF"))


ggplot(titanic_train[titanic_train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Sex) +
  ggtitle("Survival vs Passenger's Sex and Port of Embarkation")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#FF0000","#0000FF"))

################### Pre-processing
# Variables have any correlation with the response variable Survived?
# In the dataset provided, some of the data like Age, Cabin etc. are missing.
# They will be ignored for preliminary exploration of the data.

#### Missing values?
hist(titanic_train$Age)
summary(titanic_train$Age)
titanic_train$Age[is.na(titanic_train$Age)] = 29.07

summary(titanic_test$Age)
titanic_test$Age[is.na(titanic_test$Age)] = 29.07

summary(titanic_train$Fare)
summary(titanic_test$Fare)
titanic_test$Fare[is.na(titanic_test$Fare)] = 32.20


#### Univariate analysis (Variable qualitives)
titanic_train$Sex = (titanic_train$Sex=='male')*1
#titanic_train$Embarked_c = ifelse(titanic_train$Embarked=="C", 1, 0)
#titanic_train$Embarked_s = ifelse(titanic_train$Embarked=="S", 1, 0)
head(titanic_train)

titanic_test$Sex = (titanic_test$Sex=='male')*1
#titanic_test$Embarked_c = ifelse(titanic_test$Embarked=="C", 1, 0)
#titanic_test$Embarked_s = ifelse(titanic_test$Embarked=="S", 1, 0)
head(titanic_test)

titanic_train$Fare = round(titanic_train$Fare, digits = 2)
titanic_test$Fare = round(titanic_test$Fare, digits = 2)

xtabs(~Survived + Pclass, data = titanic_train)


#### New Variables
#### The family sizes
titanic_train$Fsize <- titanic_train$SibSp + titanic_train$Parch + 1 # Passenger + Siblings/Spouses + Parents/Children
titanic_train$IsAlone[titanic_train$Fsize==1] <- 'Alone' # Is the passenger travelling alone?
titanic_train$IsAlone[titanic_train$Fsize!=1] <- 'Not Alone'
titanic_train$IsAlone <- factor(titanic_train$IsAlone)

titanic_test$Fsize <- titanic_test$SibSp + titanic_test$Parch + 1 # Passenger + Siblings/Spouses + Parents/Children


titanic_train <- titanic_train[order(titanic_train$PassengerId),] #Sort the data
ggplot(titanic_train[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='bin', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')
#We can see that there’s a survival penalty to singletons and those with family sizes above 4

#Create a discretized family size variable
titanic_train$FsizeD[titanic_train$Fsize == 1] <- 'singleton'
titanic_train$FsizeD[titanic_train$Fsize < 5 & titanic_train$Fsize > 1] <- 'small'
titanic_train$FsizeD[titanic_train$Fsize > 4] <- 'large'
mosaicplot(table(titanic_train$FsizeD, titanic_train$Survived), main='Family Size by Survival', shade=TRUE) # Plotting

#### Outlier manipulation
#### Age
bx = boxplot(titanic_train$Age)
bx$stats
quantile(titanic_train$Age, seq(0, 1, 0.02))
# We can replace the outliers above 96% of the quantile range 
# and below 4% of the quantile range so that more accuracy is obtained 
# and the data loss is also not very significant.

titanic_train$Age = ifelse(titanic_train$Age>=52, 52, titanic_train$Age)
titanic_train$Age = ifelse(titanic_train$Age<=4, 4, titanic_train$Age)
boxplot(titanic_train$Age)

#### Fare
bx = boxplot(titanic_train$Fare)
bx$stats
quantile(titanic_train$Fare, seq(0, 1, 0.02))
# As can be seen above, the major difference between the values arises above 96% of the quantile.
titanic_train$Fare = ifelse(titanic_train$Fare>=136, 136, titanic_train$Fare)
boxplot(titanic_train$Fare)

#### Clean features
titanic_train = titanic_train[-c(1,4,7,8,9,11,12,14,15)] # Name,Sex, SibSp, Parch, Ticket, Cabin, Embarked
head(titanic_train)

titanic_test = titanic_test[-c(3,6,7,8,10,11)] # Name,Sex, SibSp, Parch, Ticket, Cabin, Embarked
head(titanic_test)


# https://www.kaggle.com/jeremyd/titanic-logistic-regression-in-r
################### Prediction
#### Bulding the model
train <- titanic_train
head(train)
dim(train)

test <- titanic_test
head(test)
dim(titanic_test)

X_train = train[-2]
y_train = train[2]
X_test = test[-1]

#### Set the class of Pclass and Sex as factor
titanic_train$Pclass = as.factor(titanic_train$Pclass)
titanic_train$Sex = as.factor(titanic_train$Sex)

#### Fit the logistic model
logit_fit = glm(Survived~., data = train, family = binomial)
summary(logit_fit)

#### Compute the confidence intervals of coefficients
ci = confint.default(logit_fit); ci

#### Test Accuracy of Model on Training Data
predictTrain = predict(logit_fit, type = "response")
table(train$Survived, predictTrain >= 0.5)

#### Use Model to predict survivability for Test Data
titanic_test$Pclass = as.factor(titanic_test$Pclass)
titanic_test$Sex = as.factor(titanic_test$Sex)

predictTest = predict(logit_fit, type = "response", newdata = titanic_test)
titanic_test$Survived = as.numeric(predictTest >= 0.5)
table(titanic_test$Survived)
head(titanic_test$Survived)

Predictions = data.frame(titanic_test[c("PassengerId","Survived")])
head(Predictions)
#write.csv(file = "TitanicPred", x = Predictions)

# https://www.kaggle.com/bisaria/titanic-lasso-ridge-implementation
#### Ridge
X_train <- data.matrix(X_train)
y_train <- data.matrix(y_train)
X_test <- data.matrix(X_test)

cvfit.ridge = cv.glmnet(X_train, y_train, 
                          family = "binomial", 
                          alpha = 0,
                          type.measure = "class")
coef(cvfit.ridge, s = "lambda.min")

#### Lasso
cvfit.lasso = cv.glmnet(X_train, y_train, 
                          family = "binomial", 
                          alpha = 1,
                          type.measure = "class")
par(mfrow=c(1,2))
plot(cvfit.ridge, main = "Ridge")
plot(cvfit.lasso, main = "Lasso")

#Prediction on training set
#PredTrain = predict(cvfit.ridge, newx=X_train, type="class")
#confusionMatrix(train$Survived, PredTrain)
