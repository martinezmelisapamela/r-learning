library(foreign)

y<-10:15

#Vectores
recuento <-c(2,10,4)
names(recuento)<-(0:2)
recuento

primero <- c(7,8,6,5,5,4,5,9,8,1,4)
segundo <- c(1,5)
match(primero,segundo)

#Matrices
z <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
dim(z)
nrow(z) #nro de filas
ncol(z) #nro de columnas

### Manipulando elementos de matrices

z1<-c(1,2,3)
z2<-c(4,5,6)
z3<-cbind(z1,z2)
class(z3)

#Cadenas de caracteres
a1<-"abc"
a2<-"1234"
a3 <- c(a1,a2)

#Factores
municipio<-c("ovd","gij","avl")
Municipio<-factor(municipio)
Municipio
class(Municipio)
levels(Municipio)

#Listas
ejemplolista <- list(nombre="Pedro")
ejemplolista

#DataFrames
x1 <- 1:10
x2 <- 11:20
x3 <- letters[1:10]
a <- data.frame(x1,x2,x3)
summary(a)
#Para trabajar con las variables podemos utilizar $nombre
#por ejemplo: dataframe iris
a<-iris #guardo el DF en la vble a
head(a) #veo los primeros elementos del DF
a$Sepal.Length #exploro la observacion relacionada a la long. del sepalo
min(a$Sepal.Length) #busco el minimo para esta observacion
summary(a) #muesta la info estadistica del DF
b<-summary(a) #guardo en b esos valores estadisticos
###Normalmente quiero acceder a los datos que cumplan con una condicion
#por ejemplo si hago esto:
a$Sepal.Length>5 #devuelve un objeto de verdaderos/falsos
#entonces para extraer un subconj. del DF con esa codicion, hago:
a[a$Sepal.Length>5,]
#lo puedo almacenar en una vble.
A1<-a[a$Sepal.Length>5,]

#----------------------
#Estructuras de control
#----------------------
#Si bien R tiene todas las mismas estructuras de control que los demas
#lenguajes, es conveniente utilizar alguna funcion de la familia "apply"
#Por ejemplo:
(X <- matrix(1:10,nrow=2))
mediafila <- numeric(2)
for (i in 1:2){
  mediafila[i] <- mean (X[i,])
}
mediafila
apply(X, 1, mean)
apply(X,2,sum)
apply(X,2,sqrt)

#Si queremos aplicar una función a unos valores seleccionadas a
#traves de los niveles de un factor utilizaremos tapply
#tapply(x, INDEX, fun = NULL,...,simplify=TRUE)

tapply(iris$Sepal.Length,iris$Species,
       function(y) sum(log(y)))

tapply(iris$Sepal.Length,iris$Species, sum)

tapply(iris$Sepal.Length,iris$Species, min)
