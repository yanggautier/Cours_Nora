library(glmnet)
library(mlbench)
library(ggplot2)
library(glmnet)

data("BostonHousing")

boston <- BostonHousing

      # 1. Description du jeu de données

# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's
 
      # 2. Analyse exploratoire des données

summary(BostonHousing)

# 2 variable example
ggplot(boston, aes(x=tax)) + geom_histogram(aes(y=), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

# Plot All variables one by one
library(reshape2)
d <- melt(boston[,-c(4)])
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()

# Scatter pair plots
pairs(boston[,-4], pch = 19, lower.panel = NULL)

# correlation matrix
install.packages("ggcorrplot")
library(ggcorrplot)
# select only quantitative values from the df for the corr matrix
boston_quanti  <- Filter(is.numeric, boston)
head(boston_quanti)
# create the corr matrix
corr <- round(cor(boston_quanti), 2)
head(corr[, 1:13])
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

      # 3. Data Preprocessing
                                                             
# Missing values 
which (is.na(boston))
sum(is.na(boston))

      #   Data-splitting (80% training/ 20% validation)

set.seed(100) # Random state for reproducibility
# index for partitioning
index = sample(1:nrow(boston), 0.8*nrow(boston)) 
train = boston[index,] # Create the training data 
test = boston[-index,] # Create the test data
# Check
dim(train)
dim(test)
summary(train)

      # 4. Modélisation

# Columns for linear regression
cols = colnames(boston)

# Preprocessing of train dataset
library(caret)
pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

# Manually selected pertinent columns :
lr = lm(medv ~ crim + rm + age + dis + lstat, data = train)
# Manually selected not pertinent columns :
lr2 = lm(medv ~ zn + indus + chas + nox + rad + tax + ptratio + b , data = train)
# All columns
lr3 = lm(medv ~ crim + rm + age + dis + lstat + zn + indus + chas + nox + rad + tax + ptratio + b , data = train)

summary(lr)
summary(lr2)
summary(lr3)

# Predicting on train data
predictions = predict(lr, newdata = train)
predictions2 = predict(lr2, newdata = train)
predictions3 = predict(lr3, newdata = train)

# Create the evaluation metrics function
eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  message("R-squared: ",adj_r2) #Adjusted R-squared
  message("RMSE: ",as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

# Evaluation
eval_metrics(lr, train, predictions3, target = 'medv')
eval_metrics(lr2, train, predictions3, target = 'medv')
eval_metrics(lr3, train, predictions3, target = 'medv') # Best Score : all columns (All Variables included)

# Predicting and evaluating the model on test data (with the best fit)
predictions = predict(lr3, newdata = test)
eval_metrics(lr3, test, predictions, target = 'medv')

##########  Ridge regression ########## 

# Cost function = OLS + alpha * summation (squared coefficient values)
# Parameters :
# nlambda: number of regularization parameters to be tested.
# alpha: weighting to be used. In case of ridge regression, the value of alpha is zero.
# family: distribution family to be used. Since this is a regression model, we will use the Gaussian distribution.
# lambda: lambda values to be tried.

# REGULARIZATION
dummies <- dummyVars(medv ~ ., data = boston[,cols]) # create model matrix
train_dummies = predict(dummies, newdata = train[,cols]) # create numeric model matrices (cols=colnames(boston))
test_dummies = predict(dummies, newdata = test[,cols])

# Transform into matrices
x_train = as.matrix(train_dummies)
y_train = train$medv
x_test = as.matrix(test_dummies)
y_test = test$medv

# Check
head(test_dummies);head(test) # Same values
print(dim(train_dummies)); print(dim(test_dummies))

# Semi-Auto lambda tuning
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

# Manual lambda tuning
lambdas <- 10^seq(0, -2, by = -.01)
ridge_reg = glmnet(x_train, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
summary(ridge_reg)

########## Lasso Regularization ##########

lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 

lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x_train)
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
    # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

eval_results(y_train, predictions_train, train)
eval_results(y_test, predictions_test, test)
