library(FactoMineR)
library(ggplot2)
library(GDAtools)

getwd() 
data_credit <- read.table("credit.txt", sep ='\t', header = T)

summary(data_credit)
#66 Clientss
#11 colonnes sur les infos pour chaque client

bcred <- burt(data_credit) #Convertir en tableau binaire

res.mca <- MCA(data_credit, quanti.sup = 11)




famille <- data.frame(Situation=labels(table(data_credit$Famille)/nrow(data_credit)), Valeurs=table(data_credit$Famille)/nrow(data_credit))

ggplot(data=famille, mapping = aes(x=Situation, y=Valeurs)) + geom_bar(stat = "Identity")

par(mfrow=c(3,4))
for(i in 1:11){barplot(table(data_credit[, i]), xlab = "frequency", main = names(data_credit)[i], horiz = 1, las = 1, col = "green4", border = "white")}


require(Factoshiny)
res <- Factoshiny(children)