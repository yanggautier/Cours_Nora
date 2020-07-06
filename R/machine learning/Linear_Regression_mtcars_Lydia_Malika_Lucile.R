library(ggplot2)
library(stats)
library(glmnet)
library(GGally)
library(ggcorrplot)
library(tidyr)

#################### Data Set informations #####################
data("mtcars")

## Le dataset a ?t? extrait du magazine  Motor Trend US de 1974,
## Il contient  des informations sur 32 voitures (mod?les datant de 1973--74) :
## leur consommation de carburant (en miles par gallon) et 10 autres caract?ristiques (vitesse, poids, etc...)

dim(mtcars)
row.names(mtcars)
colnames(mtcars)

# A data frame with 32 observations on 11 (numeric) variables.
# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors
head(mtcars,5)
tail(mtcars,5)
summary(mtcars)

########################### Objectif ###############################
# Valeur ? pr?dire :
# Fuel economy is the distance travelled per unit volume of fuel used;
# for example, kilometres per litre (km/L) or miles per gallon (MPG),
# where 1 MPG (imperial) ??? 0.354006 km/L. In this case, the higher the value, t
# the more economic a vehicle is 
# (the more distance it can travel with a certain volume of fuel).

## L'?conomie de carburant est un  crit?re important pour les propri?taires de voitures. 
## Le but ici est d'utiliser la r?gression lin?aire pour analyser l'ensemble de donn?es mtcars 
## afin de comprendre la relation entre l'nsemble des caract?ristiques d'une voiture et sa consommation et des miles par gallon
## Par exmple, on pourra conclure  si :
### Une transmission automatique ou manuelle est-elle meilleure pour reduire sa consammation?
### Quelle est la relation entre l'efficacit? ?nerg?tique ( mpg ) et le poids 

########################### AED #################################

## convertion mpg en km/L
data("mtcars")
head(mtcars)
mtcars$mpg <- 2.35215/ mtcars$mpg
head(mtcars)
names(mtcars)[1] <- "lpkm"
head(mtcars)
### Statistique descriptive 
summary(mtcars)

#####  Faire les analyses univari?es (histogramme des variables quantitatives,r?partition des variables qualitatives,...) 

gather(head(mtcars))
ggplot(gather(mtcars), aes(value)) + 
  geom_histogram(bins = 10,  fill="orange") + 
  facet_wrap(~key, scales = 'free_x') + ggtitle("Histogramme de chaque variable")

#### Faire les analyses bivari?es  ###
### (croisement des variables candidates avec le crit?re ? mod?liser, 
### matrice des corr?lations, barplots,...) 

mtcars %>%
  gather(-lpkm, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = lpkm)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw() + 
  ggtitle("Consommation d'essence (L/km) en fonction de chaque variable") +
  ylab("Consommation d'essence (L/km)")


## pairplot
ggpairs(mtcars) + 
  ggtitle("mtcars pairplot")


### heatmap corr


corr <- round(cor(mtcars), 2)
corr


# coor <- cor(mtcars, method = "kendall") #"kendall", "spearman"
# coor

ggcorrplot(corr)
ggcorrplot(corr, method = "circle",color = c("#FC4E07", "white", "#00AFBB"))
ggcorrplot(corr,
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)



 # for pairs.panels()
pairs.panels(mtcars,method = "pearson",hist.col ="#00AFBB" ,density = TRUE,ellipses = TRUE)



########### Data Preprocessing #################

#### missing value
is.na(mtcars)
sum(is.na(mtcars))


### outliers 
library(reshape2)
meltData <- melt(mtcars)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free") + ggtitle("boxplots")


#### scaling
head(mtcars)
scaled.mtcars <- scale(mtcars)
head(scaled.mtcars)
# check that we get mean of 0 and sd of 1
colMeans(scaled.mtcars)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.mtcars, 2, sd)


### Data-splitting (80% training/ 20% validation)

# split train et test
set.seed(12345)
# Random sample indexes
train_index <- sample(1:nrow(scaled.mtcars), 0.8 * nrow(scaled.mtcars))
test_index <- setdiff(1:nrow(scaled.mtcars), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- scaled.mtcars[train_index,]
head(X_train)
dim(X_train)
X_test <- scaled.mtcars[ test_index,]
head(X_test)
dim(X_test)



# ##### Or using 
# library(caret) #this package has the createDataPartition function
# 
# set.seed(123) #randomization`
# 
# #creating indices
# trainIndex <- createDataPartition(mtcars$lpkm,p=0.8,list=FALSE)
# 
# #splitting data into training/testing data using the trainIndex object
# X_Train <- mtcars[trainIndex,] #training data (80% of data)
# 
# X_TEST <- mtcars[-trainIndex,] #testing data (20% of data)
# 
# X_Train

# mod?le de r?gression lin?aire

# linearMod <- lm( lpkm ~. , data=data.frame(X_train))
# summary(linearMod)

linearMod <- lm(lpkm~., data=data.frame(X_train))

summary(linearMod)

####
residual <- linearMod$residuals
plot(y_train~residual,lwd=3, col="blue",main="lpkm vs residual", xlab="residual",ylab = "lpkm")
grid(NA, 5, lwd = 2,col = "darkgray")
###
y_pred_train <- predict(linearMod ,data=data.frame(X_train)  )
plot(y_train ~ y_pred_train,lwd=3, col="blue")
grid(NA, 5, lwd = 2,col = "darkgray")

# (a) Prediction error, RMSE
RMSE(y_pred_train, y_train)
# (b) R-square
R2(y_pred_train, y_train)

############# predictions test
library(caret)
y_pred_test <- predict(linearMod, newdata=data.frame(X_test))
y_pred_test 
# (a) Prediction error, RMSE
y_pred_test
RMSE(y_pred_test, y_test)
# (b) R-square
R2(y_pred_test, y_test)



confint(linearMod)
#################

AIC(linearMod)  

BIC(linearMod) 
