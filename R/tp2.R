# Exercice 1
5^2
abs(-5) #a
sqrt(9)#b
exp(1)#c
log2(2)#d
log10(100)#e
1/10#f
fraction <- function(x)(1/x)
fraction(10)

# Exercice 2
signif(4850/26,3) 

# Exercice 3
round(pi,3)
round(pi,5)

# Exercice 4
c("un", TRUE, 5)
seq(1,10,by=2)
rep(1:2,times=3)

# Exercice 5
v1 <- c(-1, 3.2, -2.8)
class(v1)
v1

v2 <- c(-2, -1, 0, 1, 2, 3, 4, 5, 6)
class(v2)
v2

v3 <- c(0.05, 0.1, 0.15, 0.2)
class(v3)
v3

v4 <- c(1,1,1,1,1,1,1,1,1,1)
class(v4)
v4

v5 <- c("Oui", "Non")
class(v5)
v5

# Exercice 6
v1<-sort(v1)
v1

# Exercice 7
v6 <- 2* v2-3

v6
v3
v2
v7 <- v3+v2
v7

log(v3)
# les calculs appliquenet sur l'ensemble des vecteurs

# Exercice 8
v5[2]
length(v6)
v8 <- v6[7:9]
v8
v8<- tail(v6, n=3)
v8
# Exercice 9
length(v6)

somme <- 0
for (element in v6){
  somme <- somme + element
}
somme

sum(v6)

# Exercice 10

vec1 <- 1:12
vec1
vec1 <- append(vec1 , 16:18)
vec1


# Exercice 11
vec2 <- seq(0,5, by=0.5)
vec2

# Exercice 12
vec3 <- 1:25*2
vec3
vec3 <- seq(2,50,2)
vec3

vec3 = c()
vec3 = numeric()

for (i in 1:50) {
  if (i %% 2 == 0)
    vec3 = append(vec3,i)
}
print(vec3)


# Exercice 13
vec4 <- sample(1:100,10)
vec4


# Exercice 14
vec5 <- rep(1:10,3)
vec5

# Exercice 15
data <- read.csv(file="C:\\Users\\yangg\\Python\\Cours Nora\\R\\boulangerie.csv", sep =";") 
data
attributes(data)
attr(data, "class")
data[,2]


liste <- gsub(",", '.', data$Avec.boulagerie..)
liste_percent <- as.numeric(liste)
liste_percent
liste_number <- liste_percent*data[,2]/100
liste_number
somme = sum(liste_number)
somme
liste_final <- liste_number/somme*100
liste_final
class(data[,3])


names(data)[1] <- "nombre"
names(data)[2] <- "commune"
names(data)[3] <- "boulangerie"

names(data)

data['boulangerie'] <- liste_final
data

# Bonus

matIdentite <- diag(10)
matIdentite


matAleatoire <- matrix(floor(runif(100, min=0, max=20)), 10,10)
matAleatoire
