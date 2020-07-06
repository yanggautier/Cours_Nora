# Exercice 1
# f(x) =x^2


#b
abs(-2)

#c 
sqrt(4)

#d
exp(4)

#e
log(1)

#f 


# Exercice 2
x<-4850/26
x
round (x,digits=3)
#ceiling(x)
#floor(x)
round (x,digits=0)

# Exercice 3
pi

round(pi, digits=3)
round(pi, digits=5)
pi + 12
# pi = 9 

# Exercice 4
c(1:4)

c(2,2,4:10)

seq(10)
seq(10:12)
seq(2,10,3)

rep(10)
rep(1:10)

rep(1,10)

#Exercice 5: 
#V1 = (-1,3.2,-2,8)

V1 <- c(-1,3.2,-2,8)
V1

V2 <- c(-2,-1,0:6)
V2

V3<-seq(0.05,0.2,0.05)
V3

V4<-rep(1,10)
V4

V5<- c('OUI', 'NON')
V5

#Exercice 6:
sort(V1)

#Exercice 7:

V6 =  -3 + 2 *V2
V6
V3+V2

log(V3)

#Exercice 8:

V5[-1]

V8<-tail(V6, n=3)
V8

#Exercice 9:
length(V6)

V<-0
for (i in V6){ 
V <- V + i  }
V
sum(V6)

#Exercice 10:
vec1 <- c(1:12)
vec1
cat(vec1,16,17,18)

#Exercice 11:
vec2 <- seq(0,5,0.5)
vec2

#Exercice 12:
vec3 <- seq(2, 50,2)
vec3

#Exercice 13:
vec4<- sample(1:100,10)
vec4

#Exercice 14:
vec5<-rep(1:10,3)
vec5

#Exercice 15:
data = read.csv("intro/Intro à R/Databoulangerie.csv", sep=";")
data

data$Avec.boulagerie.. <-gsub(",", '.', data$Avec.boulagerie..)
data

names(data)[1]<-"nombre"
names(data)[2]<-"communes"
names(data)[3]<- "boulangerie"

data 
 
data$boulangerie = as.numeric(data$boulangerie)
data$boulangerie2 <- data$communes * data$boulangerie / 100
data
data$boulangerie2 <- floor(data$boulangerie2)
data
pourcentage = sum(data$boulangerie2)*100/sum(data$communes)
pourcentage



#Bonus
matIdentite <- diag(1,10)
matIdentite
matAleatoire<- matrix(runif(10*10),10)
#matAleatoire<-matrix(rexp(20), 10,10)
matAleatoire
