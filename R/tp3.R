# TP3
#1. Définitions
#1.1 Définir le principe de l’Analyse Factorielle des Correspondances (AFC) 
# L'Analyse Factorielle des Correspondances (AFC) est une méthode qui permet d'étudier l'association entre deux variables qualitatives. 
#Cette méthode est basée sur l'inertie.
#Le but de l'Analyse Factorielle des Correspondances consiste à représenter un maximum de l'inertie totale sur le premier axe factoriel, 
#un maximum de l'inertie résiduelle sur le second axe, et ainsi de suite jusqu'à la dernière dimension.
#On montre que le nombre de dimensions de l'espace de représentation est inférieur ou égal à min(m1, m2)-1.

#1.2  Définir le principe de l’Analyse des Correspondances Multiples (ACM) 
#L’analyse des correspondances multiples (ACM) est la méthode factorielle (au sens français du terme) adaptée aux tableaux
#dans lesquels un ensemble d’individus (en lignes) est décrit par un ensemble de variables qualitatives (en colonnes).

#1.3 Expliquer les principales différences avec l’ACP (type de données, métrique utilisée,...) 
#l’analyse en composante principale (ACP) portant sur des variables quantitatives
#l’analyse des correspondances multiples (ACM) portant sur plusieurs variables qualitatives (il s’agit d’une extension de l’AFC)

#1.4 Définir la(les) matrice(s) en entrée de l’ACM 
# Initialement, le tableaux des données se résume à une matrice X, 
#où une ligne correspond à un individu et une colonne à une modalité d'une variable. 
#Ce tableau est composé de 0 et de 1. 

#1.5 Définir les outils d’aide à l’interprétation des résultats en sortie de l’ACM
# Graphique   -utiliser le sous-programme dynGraph de  R

#3. Application de l’AF
# L’objectif est d'appliquer une analyse factorielle des correspondances au jeu de données children  disponible dans la librairie FactominerR 
#3.1 Décrire le jeu de données
library(FactoMineR)
library(ggplot2)
library(dplyr) #dplyr ne s'utilise qu'avec des dataframes
library(questionr) # pour des tableaux
library(ggfortify) # pour des plots
data(children)
children
#3.2

#Analyser les statistiques descriptives 
summary(children)

#Profil ligne moyen
rowMeans(children)

#Profil colonne moyen
colMeans(children)

res.ca <- CA(children, row.sup = 15:18, col.sup = 6:8)

eig.val <- res.ca$eig
barplot(eig.val[,3], names.arg = 1:nrow(eig.val), main = "Variances Expained by PCs (%)", xlab = "Principal Components", ylab = "Percentage of variances", col = "steelblue")

# Add connected line segments to the plot
lines(x= 1:nrow(eig.val), eig.val[,3], type = "b", pch = 19, col = "red")

ca_eig <- data.frame(Dimensions=c("Dim1","Dim2","Dim3","Dim4"), Cumulated.Eigenvalues=c(res.ca$eig[,3]))

ggplot(data = ca_eig, mapping = aes(x=Dimensions, y=Cumulated.Eigenvalues)) + geom_bar(stat = "identity")

#Package factoextra - scree plot
library('factoextra')
fviz_eig(res.ca, addlabels = TRUE, ylim = c(0, 60))

#Inertia results
inertiaDistrib(res.ca, q = 0.99, time = "10000L")

#Scree plot
par(mar= c(2.6, 4.1, 1.1, 2.1))
ggplot2::ggplot(cbind.data.frame(x=1:nrow(res.ca$eig), y=res.ca$eig[,2])) + 
  ggplot2::aes(x=x, y=y) +
  ggplot2::geom_col(fill="blue") + 
  ggplot2::xlab("Dimension") +
  ggplot2::ylab("Pourcentage d'inertie")
  ggplot2::ggtitle("Décomposition de l'inertie totale") + 
  ggplot2::theme_light() + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::scale_x_continuous(breaks = 1::nrow(res.ca$eig))

inertie_totale <- sum(res.ca$eig[1:3,1])
inertie_totale
  
marg.row(res.ca)
round(res.ca$row$contrib)
round(res.ca$col$contrib)

round(res.ca$row$contrib,2)
round(res.ca$col$contrib,2)

plot(res.ca, autoLab = "yes")
plot(res.ca, axes = c(1, 3))
plot(res.ca, invisible = "col")
plot(res.ca, invisible = "row")


require(Factoshiny)
res <- Factoshiny(children)
