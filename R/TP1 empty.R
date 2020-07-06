
## INTRO R 


1 # une constante 
2 +(3 * 5)/7 # priorité de calculs classique
3^5 # Puissance
exp(3) # 
sin(pi/2) + cos(pi/2) # 
gamma(5) # 

## Lorsqu'une expression est syntaxiquement incomplète, 
## l'invite de commande change de '> ' à '+ '. 
2-# expression incomplète 
  5* # toujours incomplète 
  3 # complétée 
2-5*3

a = 0
a
toto = "Salut !"
toto1 = "toi"

exp(3) - 13^2 / 5
# Concaténation : 

aa = paste(toto,toto1)
aa
## Taper le nom d'un objet affiche son contenu. Pour une 
## fonction, c'est son code source qui est affiché. 
pi # constante numérique intégrée 
pi = 596868765 # A NE JAMAIS FAIRE 
pi



letters # 
LETTERS #  

matrix # Fonction permettant de créer des matrices 
# Matrice de dim 3x3 contenant des zéros

m0 <-  matrix(0,nrow = 3,ncol = 3)  
m0
# Matrice de dim 4x3 contenant des 1
  
m1 <-  matrix(1,ncol = 3,nrow = 4)  
m1 <-  matrix(1,2,3)  


m1

# Ne pas utiliser '=' pour l'affectation. Les opérateurs 
## d'affectation standard en R sont '<-' et '->'. 

x <- 5 # 
x
5 -> x1 #
x1
x # v 

(x <- 6) #  

y <- x # 
y

x <- y <- 5 
x
y


x <- y <- 6
x
y 
x <- 0 # changer la valeur de 'x'... 
y # ... ne change pas celle de 'y' 






## Pour regrouper plusieurs expressions en une seule commande, 
## il faut soit les séparer par un point-virgule ';', soit les 
## regrouper à l'intérieur d'accolades { } et les séparer par 
## des retours à la ligne. 
x <- 5; y <- 2; x + y # compact; éviter dans les scripts 
x <- 5; # éviter les ';' superflus 
{ # début d'un groupe 
  x <- 5 # première expression du groupe 
  y <- 2 # seconde expression du groupe 
  x + y # résultat du groupe 
} # fin du groupe et résultat 
{x <- 5; y <- 2; x + y} # valide, mais redondant 

### ### NOMS D'OBJETS ### 
## Quelques exemples de noms valides et invalides. 
foo <- 5 # 
oo.123 <- 5 # 
foo_123 <- 5 # 
T123foo <- 5 # Ne pas commencer par des chiffres 
.foo <- 6 #  
.t123foo <- 5 # 

ls()
## Liste des objets dans l'espace de travail. Les objets dont 
## le nom commence par un point sont considérés cachés. 
ls() # l'objet '.foo' n'est pas affiché 
ls(all.names = TRUE) # objets cachés aussi affichés ## R est sensible à la casse 
foo <- 1 
Foo 
FOO 


### ARRONDI

pi = 3.1415926
pi1 = round(pi,2)
pi1


pi2 <- signif(pi,4)
pi
pi2

pi3 <- signif(5.37555,4)
pi4 <- signif(5.37335,4)
pi3
pi4


pi5 <- ceiling(pi)
pi5
pi5 <- floor(pi)
pi5w 

pi6 <- trunc(pi)
pi6
pi7 <- trunc(748575.98989898)
pi7
### ### LES OBJETS R ### 

## MODES ET TYPES DE DONNÉES 

## Le mode d'un objet détermine ce qu'il peut contenir. Les 
## vecteurs simples ("atomic") contiennent des données d'un 
## seul type. 
mode(c(1, 4.1, pi)) # nombres réels 
mode(c(2, 1 + 5i)) # nombres complexes 
mode(c(TRUE, FALSE, TRUE)) # valeurs booléennes 
mode("foobar") # chaînes de caractères 

## Si l'on mélange dans un même vecteur des objets de mode 
## différents, il y a conversion automatique vers le mode pour 
## lequel il y a le moins de perte d'information, c'est-à-dire 
## vers le mode qui permet le mieux de retrouver la valeur 
## originale des éléments. 

c(5, TRUE, FALSE) # conversion en mode 'numeric' 
c(5, "z") # conversion en mode 'character' c(TRUE, "z") # conversion en mode 'character' 
c(5, TRUE, "z") # conversion en mode 'character' 
## La plupart des autres types d'objets sont récursifs. Voici 
## quelques autres modes. 
mode(seq) # une fonction 

mode(list(5, "foo", TRUE)) # une liste 
mode(expression(x <- 5)) # une expression non évaluée 

## LONGUEUR 

## La longueur d'un vecteur est égale au nombre d'éléments 
## dans le vecteur. 
(x <- 1:4) 
length(x) 
## Une chaîne de caractères ne compte que pour un seul 
## élément. 
(x <- "foobar")
length(x) 
## Pour obtenir la longueur de la chaîne, il faut utiliser 
## nchar(). 
nchar(x) 
## Un objet peut néanmoins contenir plusieurs chaînes de 
## caractères. 
(x <- c("f", "o", "o", "b", "a", "r")) 
length(x) 
## La longueur peut être 0, auquel cas on a un objet vide,
## mais qui existe. 
(x <- numeric(0)) # création du contenant 
length(x) # l'objet 'x' existe... 
x[1] <- 1 # possible, 'x' existe 
X[1] <- 1 # impossible, 'X' n'existe pas 

## L'OBJET SPECIAL 'NULL' 


mode(NULL)
# le mode de 'NULL' est NULL 
length(NULL) # longueur nulle 
x <- c(NULL, NULL) # s'utilise comme un objet normal 
x; 
length(x); 
mode(x) # mais donne toujours le vide 

## L'OBJET SPÉCIAL 'NA' 
x <- c(65, NA, 72, 88) # traité comme une valeur 
x + 2 # tout calcul avec 'NA' donne NA 
mean(x) # voilà qui est pire 
mean(x, na.rm = TRUE) # éliminer les 'NA' avant le calcul 
is.na(x) # tester si les données sont 'NA' 

## VALEURS INFINIES ET INDÉTERMINÉES 
1/0 # +infini 
-1/0 # -infini 
0/0 # indétermination 
x <- c(65, Inf, NaN, 88) # s'utilisent comme des valeurs 
is.finite(x) # quels sont les nombres réels? 
is.nan(x) # lesquels ne sont «pas un nombre»? 



## ATTRIBUTS 

## Les objets peuvent être dotés d'un ou plusieurs attributs. 
data(cars) # jeu de données intégré 
cars
attributes(cars) # liste de tous les attributs 
attr(cars, "class") # extraction d'un seul attribut 
## Attribut 'class'. Selon la classe d'un objet, certaines 
## fonctions (dites «fonctions génériques») vont se comporter 
## différemment. 


x <- sample(1:100, 10) # échantillon aléatoire de 10 # nombres entre 1 et 100 
class(x) # classe de l'objet 
plot(x) # graphique pour cette classe 

class(x) <- "ts" # 'x' est maintenant une série chronologique 
plot(x) # graphique pour les séries chronologiques 
class(x) <- NULL; x # suppression de l'attribut 'class' 

## Attribut 'dim'. Si l'attribut 'dim' compte deux valeurs, 
## l'objet est traité comme une matrice. S'il en compte plus 
## de deux, l'objet est traité comme un tableau (array). 


x <- 1:24 # un vecteur 
dim(x)
length(x)

dim(x) <- c(4, 6) # ajoute un attribut 'dim' 
x # l'objet est une matrice 
dim(x) <- c(4, 2, 3) # change les dimensions 
typeof(x)
x # l'objet est maintenant un tableau
## Attribut 'dimnames'. Permet d'assigner des étiquettes (ou 40 Bases du langage R 
## noms) aux dimensions d'une matrice ou d'un tableau. 
dimnames(x) <- list(1:4, c("a", "b"), c("A", "B", "C")) 
dimnames(x) # remarquer la conversion 
x # affichage avec étiquettes 
attributes(x) # tous les attributs de 'x' 
attributes(x) <- NULL; x # supprimer les attributs 

## Attributs 'names'. Similaire à 'dimnames', mais pour les 
## éléments d'un vecteur ou d'une liste. 

names(x) <- letters[1:24] # attribution d'étiquettes 
x # identification facilitée 


### ### VECTEURS ### 

V1 = c(1,10,7,0.1)
V1

V2 = seq(0,10000,100)
V2

V3 = c(round(sqrt(1:100),2))
V3

1/round(sqrt(V2[3] + pi ),2)


x <- c(a = -1, b = 2, c = 8, d = 10) # création d'un vecteur 
names(x) # 
names(x) <- letters[1:length(x)] #  
x[1] #  
x["c"] # 
x[-2] #  
x = x[-2]

x

vector("numeric", 5) # 
numeric(5) # 
logical(5) # 
complex(5) # 
character(5) # 



### ### MATRICES ET TABLEAUX ### 


(x <- matrix(1:12, nrow = 3, ncol = 4)) # créer la matrice 
length(x) # 'x' est un vecteur... 
dim(x) # ... avec un attribut 'dim'... 
class(x) # ... et classe implicite "matrix" 
## Une manière moins naturelle mais équivalente --- et parfois 
## plus pratique --- de créer une matrice consiste à ajouter 
## un attribut 'dim' à un vecteur. 
x <- 1:12 # vecteur simple 
dim(x) <- c(3, 4) # ajout d'un attribut 'dim' 
x; class(x) # 'x' est une matrice! 



matrix(1:12, nrow = 3, byrow = TRUE) 
## Indicer la matrice ou le vecteur sous-jacent est 
## équivalent. Utiliser l'approche la plus simple selon le 
## contexte. 
x[1, 3] # 
x[7] # ... 
x[1, ] #  
x[, 2] # 
nrow(x) # 
ncol(x) # 
dim(x)[2] # idem ## Fusion de matrices et vecteurs. 
x <- matrix(1:12, 3, 4) # 'x'  
y <- matrix(1:8, 2, 4) # 'y' 
z <- matrix(1:6, 3, 2) # 'z' 
x
y
z

rbind(x, 1:4) # 
rbind(x, y) # 
cbind(x, 1:3) # 
cbind(x, z) # 
rbind(x, z) #   
cbind(x, y) #   
## Les vecteurs ligne et colonne sont rarement nécessaires. On 
## peut les créer avec les fonctions 'rbind' et 'cbind', #
# respectivement. 
rbind(1:3) # un vecteur ligne 
cbind(1:3) # un vecteur colonne  
## Un tableau (array) est un vecteur avec un attribut 'dim' de 
## longueur supérieure à 2 et une classe implicite "array". 
## Quant au reste, la manipulation des tableaux est en tous 
## points identique à celle des matrices. Ne pas oublier: 
## les tableaux sont remplis de la première dimension à la 
## dernière! 
x <- array(1:60, 3:5) # tableau 3 x 4 x 5 
length(x) # 'x' est un vecteur... 
dim(x) # ... avec un attribut 'dim'... 
class(x) # ... une classe implicite "array" 
x[1, 3, 2] # l'élément en position (1, 3, 2)... 
x[19] # ... est l'élément 19 du vecteur 
x
