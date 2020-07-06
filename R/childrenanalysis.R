library(FactoMineR)
library(ggplot2)
library(dplyr) #dplyr ne s'utilise qu'avec des dataframes
library(questionr) # pour des tableaux
library(ggfortify) # pour des plots

# A data frame with 18 rows and 8 columns. 
# Rows represent the different reasons mentioned, columns 
# represent the different categories (education, age) people belong to.

data(children)

#Résumé statistique
summary(children)

#Profil ligne moyen - raisons
rowMeans(children)

#Profil colonne moyen - éducation
colMeans(children)



# data(wine)
# res.mfa <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#            ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#            num.group.sup=c(1,6))

res.ca <- CA(children, row.sup = 15:18, col.sup = 6:8) #4 dim max
summary(res.ca)

eig.val <- res.ca$eig
barplot(eig.val[, 3], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by PCs (%)",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 3], 
      type = "b", pch = 19, col = "red")



ca_eig <- data.frame(Dimensions=c("Dim1", "Dim2", "Dim3", "Dim4"),
                     Cumulated.Eigenvalues=c(res.ca$eig[,3]))

ggplot(data = ca_eig, mapping = aes(x=Dimensions, y=Cumulated.Eigenvalues)) +
  geom_bar(stat="identity")

#Package factoextra - scree plot
fviz_eig(res.ca, addlabels = TRUE, ylim = c(0, 60))

#Inertia results
inertiaDistrib(res.ca, q = 0.99, time = "10000L")

#Scree plot
par(mar = c(2.6, 4.1, 1.1, 2.1))
ggplot2::ggplot(cbind.data.frame(x=1:nrow(res.ca$eig),y=res.ca$eig[,2])) + 
  ggplot2::aes(x=x, y=y)+ 
  ggplot2::geom_col(fill="blue") + 
  ggplot2::xlab("Dimension") + 
  ggplot2::ylab("Pourcentage d'inertie") + 
  ggplot2::ggtitle("Décomposition de l'inertie totale") + 
  ggplot2::theme_light() + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5)) + 
  ggplot2::scale_x_continuous(breaks=1:nrow(res.ca$eig))

inertie_totale <- sum(res.ca$eig[,1])
inertie_totale #Pas au complet car que 4 dimensions

#Valeurs de ligne et colonnes
mcol <- res.ca$call$marge.col
mrow <- res.ca$call$marge.row

d_marg_r <- data.frame(Education=labels(mcol), Values=c(mcol))
d_marg_c <- data.frame(Reasons=labels(mrow), Values=c(mrow))

#Diagramme en bâtons profil colonne
ggplot(data = d_marg_r, mapping = aes(x=Education, y=Values)) +
  geom_bar(stat="Identity")

#Diagramme en bâtons profil ligne
ggplot(data = d_marg_c, mapping = aes(x=Reasons, y=Values)) +
  geom_bar(stat="Identity")

## Ellipses for all the active elements
ellipseCA(res.ca)
## Ellipses around some columns only
ellipseCA(res.ca,ellipse="col",col.col.ell=c(rep("blue",2),rep("transparent",3)),
          invisible=c("row.sup","col.sup"))

## Not run: 
## Graphical interface
require(Factoshiny)
res <- Factoshiny(children)

## End(Not run)

