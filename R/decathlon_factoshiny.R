library(FactoMineR)
library(ggplot2)
library(dplyr) #dplyr ne s'utilise qu'avec des dataframes
library(ggfortify) # pour des plots

setwd('C://Users/yangg/Python/Cours Nora/PCA')
decathlon_set <- read.table("Donnees_decathlon.txt", sep ='\t', header = T)
head(decathlon_set)

require(Factoshiny)
decathlon_shiny <- Factoshiny(decathlon_set)


getwd() 
data_credit <- read.table("credit.txt", sep ='\t', header = T)
credit_shiny <- Factoshiny(data_credit)

data("children")

children_shiny <- Factoshiny(children)




