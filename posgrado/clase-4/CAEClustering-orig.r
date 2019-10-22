
# Particionado

## Preparación y análisis inicial de datos

#Usaremos los datos de USArrests como conjuntos de datos de demostración, con el ratio de crímenes por estado de USA. Este dataset está incluido en R.
# Usaremos solo un subconjunto de los datos tomando 15 filas aleatorias entre las 50 filas en el conjunto de datos. Esto se hace usando la función **sample()**. A continuación, estandarizamos los datos utilizando la función **scale()**:



# Cargamos librerías
# factoextra: para visualización de clusters 
#Si el paquete factoextra no se instala correctamente, 

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/factoextra")

#Carga de paquetes
library("factoextra")



set.seed(123)
ss <- sample(1:50, 15)   # Seleccionamos 15 filas de las 50 que tiene este dataset
df <- USArrests[ss, ]    
df.scaled <- scale(df)   # Transformación de variables entre 0 y 1


#Hay muchas funciones R para calcular distancias entre pares de observaciones:

#* `dist()` [paquete stats]: Acepta solo datos numéricos como entrada.
#* `get_dist()` [paquete factoextra]: Acepta solo datos numéricos como entrada. Admite medidas basadas en el cálculo de correlación (*pearson*, *kendall* y *spearman*).
#* `daisy()` [paquete cluster]: Admite variables nominales u ordinales. En ese caso, se utilizará automáticamente el coeficiente de Gower (? Daisy para ver en qué consiste este coeficiente).

#Calculamos la distancia euclídea

dist.eucl <- dist(df.scaled, method = "euclidean")

## Mostramos las distancias
round(as.matrix(dist.eucl)[1:5, 1:5], 1)

#La función `dist()` calcula la distancia euclidea por defecto, pero puede calcular otras distancias como  *euclidean*, *maximum*, *manhattan*, *canberra*, *binary*, *minkowski*.

#Vamos a calcular ahora otras distancias.

# Cargamos librería
dist.cor <- get_dist(df.scaled, method = "pearson")

## Mostramos las distancias
round(as.matrix(dist.cor)[1:5, 1:5], 1)

#Finalmente, probamos a calcular distancias cuando hay alguna variable no numérica.

# Cargamos librería para utilizar algoritmos de clustering
library(cluster)

# Cargamos datos
data(flower)

#Observamos que hay varias variable discretas y ordinales.
str(flower)

# Calculamos distancias
dd <- daisy(flower)

## Mostramos las distancias
round(as.matrix(dd)[1:5, 1:5], 2)


#Finalmente, visualizamos distancias entre ejemplos. 
#Cuanto más rojo sea el color, mayor similitud. Más azul, baja similitud.


library(factoextra)
fviz_dist(dist.eucl)


## K-Means

#Empezaremos por el método **k-means**

data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

#La función *k-means* es muy simple de usar, 
#se encuentra en el paquete `stats`. Tiene los siguientes argumentos
#kmeans(x, centers, iter.max = 10, nstart = 1)

#con
# x: matriz o data frame de nvariables numéricas.
#  centers: Número de clusters (*k*), generando entonces *k* centroides distintos aleatoriamente o conjunto inicial de centroides. or a set of initial (distinct) cluster centers. 
# iter.max: Número máximo de iteraciones (10 por defecto).
#  nstart: Número de centroides iniciales generados 
#cuando el parámetro `centers`es un número. 
#Se recomienda  nstart > 1.


#Esta función retorna
#* cluster: Un vector de enteros indicando el cluster asignado a cada ejemplo.
#* centers: matriz de centroides.
#* totss: error total.
#* withinss: un vector de dimensión *k* con la distancia intra cluster.
#* within-cluster: un vector de dimensión *k* con la distancia inter cluster.
#* tot.withinss: sum(withinss)
#* size: numero de ejemplos de cada cluster.


#Veamos el agrupamiento producido con k=3. Vamos a visualizar un gráfico de dispersión cuyos dos ejes son dos variables del conjunto de datos, y a cada muestra se le dará **un color** *de acuerdo al grupo al que se le ha asignado*, finalmente, mostramos el modelo obtenido.
mdl <- kmeans(df, centers=3)

par(mfrow(1,1))
plot(df[,1], df[,2], col = mdl$cluster, main="df1-df2, k3, nstart p.d, iter p.d.")
plot(df[,2], df[,3], col = mdl$cluster, main="df2-df3, k3, nstart p.d, iter p.d.")
plot(df[,2], df[,4], col = mdl$cluster, main="df2-df4, k3, nstart p.d, iter p.d.")
str(mdl)

#También podemos representar las agrupaciones de forma global utilizando la función fviz_cluster()


#fviz_cluster(x, data = NULL, stand = TRUE, 
#            geom = c("point", "text"),frame = TRUE, frame.type = "convex")

#que necesita

# x: Un objeto de clase partition, es decir la variable que retorna un algoritmo de clustering (PAM, CLARA o KMEANS)
#  data: Si el algoritmo es K-Means, datos de entrada al clustering. Innecesario en otro caso.
#  stand: Datos normalizados (TRUE) 
#  geom: Especificación de la geometría utilizada, “point”, “text” o c(“point”, “text”)
#  frame: Marco alrededor de clusters (TRUE)

#Lo aplicamos al objeto retornado por `kmeans()`

fviz_cluster(mdl, data=df)

#Otra función que se puede utilizar es la función `clusplot()`del paquete *cluster*. Esta función, como la anterior, representa el agrupamiento con respecto a sus 2 componente principales.


clusplot(df, mdl$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0)

#También podemos representar a que cluster 
#pertenece cada punto.
library(fpc)
plotcluster(df, mdl$cluster)

#Vamos ahora a estudiar el efecto del número de iteraciones. En los siguientes ejemplos vemos tres llamadas diferentes a *k-means* variando los parámetros: solo se realiza una reasignación aleatoria y el número de iteraciones máxima es 1 o 2 o 3.
#Se añade la visualización para comparar resultados.

mdl = kmeans(df, centers=3, nstart = 1, iter.max = 1) 
fviz_cluster(mdl, data=df)
mdl = kmeans(df, centers=3, nstart = 1, iter.max = 2) 
fviz_cluster(mdl, data=df)
mdl = kmeans(df, centers=3, nstart = 1, iter.max = 3) 
fviz_cluster(mdl, data=df)



#Veamos el efecto de más repeticiones de asignación inicial:
mdl = kmeans(df, centers=3, nstart = 10, iter.max = 1) 
fviz_cluster(mdl, data=df)
mdl = kmeans(df, centers=3, nstart = 10, iter.max = 2) 
fviz_cluster(mdl, data=df)
mdl = kmeans(df, centers=3, nstart = 10, iter.max = 10) 
fviz_cluster(mdl, data=df)



#Y ahora proponiendo más iteraciones...
mdl2 = kmeans(df, centers=3, nstart = 10, iter.max = 100)
fviz_cluster(mdl, data=df)

#Finalmente, escalaremos primero el dataset y luego aplicaremos el agrupamiento.
#ahora, escalemos a [0.0, 1.0] el dataset:
th = apply(df, 2, min)
span = apply(df, 2, max) - apply(df, 2, min)

df = scale(df, center = th, scale = span)

# aplicar a df el algoritmo kmeans y comparar.
mdl2 = kmeans(df, centers=3, nstart = 10, iter.max = 10)
fviz_cluster(mdl2, data=df)

str(mdl)


## Búsqueda del cluster óptimo
#El siguiente paso consiste en evaluar 
#los diferentes candidatos de tamaño de cluster. 

#Existen muchos métodos para intentar buscar el número cluster óptimo. Todos ellos se basan en aplicar el algoritmo de clustering con distintos valores de k.

#Una de las posibilidades es calcular la suma de distancias intra cluster (parámetro wss), representar esta cantidad según los valores de k y elegir como óptimo el k que corresponda con un punto de inflexión. Esto se conoce como regla del codo. Para ello utilizamos la función `fviz_nbclust`.
#fviz_nbclust(x, FUNcluster, method = c("silhouette", "wss"))

#que necesita

# x: matriz, data frame o vector
# FUNcluster: Nombre del algoritmo de particionado, kmeans, pam, etc.
# method: Método para determinar el número de clusters

#Vamos a probarlo con `kmeans` con k entre 1 y 10.

set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")


#También podemos usar la función `NbClust`, que computa un método de clustering con *k* clusters entre *min.nc* y *max.nc*, utilizando como distancia la especificada en distance. Su sintaxis es la siguiente:

# NbClust(data = NULL, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = NULL, index = "all")

#\item	data: matriz
#	diss: matriz de disimilitud. Por defecto NULL. Si no es NULL, entonces distance tiene que ser NULL
#  distance: distancia usada para calcular la matriz de disimilitud. Toma los siguientes valores: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski" o "NULL".  Por defecto, “euclidean”
# min.nc: Número mínimo de clusters
#\item  max.nc: Número máximo de clusters
#\item  method: Método de cluster usado: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans".
#\item  Index: Índice de validación usado: "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (todos menos GAP, Gamma, Gplus and Tau), "alllong" (todos).

library(NbClust)

res.nbclust <- NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
                       method = "complete", index ="all") 


#Vamos a realizar la agrupación con el número de clusters óptimo

set.seed(123)
km.res <- kmeans(df, 2, nstart = 25)
print(km.res)

#Calculamos la media de cada una de las variables en el cluster (los centroides reales)

aggregate(df, by=list(cluster=km.res$cluster), mean)


## Validación de clusters

#Para ellos usamos la función silueta.

Sil.km <- silhouette(km.res$cluster, dist(df))

plot(Sil.km,  col = 2:5) 

#En caso de que algún ejemplo esté en el cluster incorrecto, se puede buscar cual es el cluster vecino más próximo.

 # Calculamos la silueta
sil <- Sil.km[, 1:3]
# objetos con silueta negativa
#neg_sil_index <- which(sil[, 'sil_width'] < 0)
#sil[neg_sil_index, , drop = FALSE]


# Resumen de la silueta
si.sum <- summary(Sil.km)
# Average silhouette width of each cluster
si.sum$clus.avg.widths

# Media total 
si.sum$avg.width

# Tamaño de cada cluster
si.sum$clus.sizes


#Por otro lado, es posible comparar dos agrupaciones distintas. La función `cluster.stats()` del paquete *fpc* proporciona un mecanismo para comparar la similitud de dos soluciones de clúster usando una variedad de criterios de validación (coeficiente gamma de Hubert, el índice de Dunn y el índice de rand corregido)
cluster.stats(df, mdl$cluster, mdl2$cluster)

## Otros métodos de particionado: PAM y CLARA

#PAM se puede realizar SOLO con datos donde todas las variables sean continuas.
#La función standard de R que permite utilizar el método PAM (Particionado alrededor de la madiana) es `pam()` (en paquete cluster). Su sintaxis es:


# Métodos jerárquicos

## Métodos Aglomerativos:  hclust

#Este ejemplo está extraido de [R-manual](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html).

#Necesitamos los siguientes paquetes:
library(graphics)
library(corrgram)



#Veamos los gráficos de correlaciones de las variables dos a dos:
corrgram(USArrests)

#El método `hclust()` es un método de **Clustering Aglomerativo**. Su sintaxis es:

#`hclust(x, method=”complete”)`

#Necesita

# x:  Matriz de distancias

# method: Método aglomerativo. Posibilidades: “Ward.D”, “ward.D2”, “single”, “complete”, “average”, “mcquitty”, “median” o “centroid”.

#Retorna

# merge: Una matriz n-1 por 2. La fila i de fusión describe la fusión de los clústeres en el paso i del clúster. Si un elemento j en la fila es negativo, entonces la observación -j se fusionó en esta etapa. Si j es positivo, entonces la combinación fue con el grupo formado en la etapa j (anterior) del algoritmo. Así, las entradas negativas en la fusión indican aglomeraciones de singletons, y las entradas positivas indican aglomeraciones de no singletons.

# height: Un conjunto de valores reales n-1 (no decreciente para árboles ultramétricos). La altura de agrupación: es decir, el valor del criterio asociado con el método de agrupación para la aglomeración particular.
# order: Un vector que proporciona la permutación de las observaciones originales adecuadas para el trazado, en el sentido de que un diagrama de agrupación que utiliza este ordenamiento y combinación de matrices no tendrá cruces de las ramas.

# labels: Etiquetas para cada uno de los objetos agrupados.

# call: La llamada que produjo el resultado.

# method: El método de cluster que se ha utilizado.

#dist.method: la distancia que se ha utilizado para crear d (solo se devuelve si el objeto de distancia tiene un atributo de "método").

#A continuación se agruparán las variables usando el método average:

hc <- hclust(dist(USArrests), "ave")


#Ahora hay que examinar el agrupamiento, primero veamos la información de hclust:
help(hclust)

#Y ahora analizamos el agrupamiento:
str(hc)


#En este caso hay que examinar los resultados un poco más a fondo:
head(hc$merge)
head(hc$height)
head(hc$order)
hc$method
hc$call
hc$dist.method

#Busca el enlace entre Florida y North Carolina en merge. 
#Observa que es el ultimo cluster creado, por eso aparece a la izquierda
#Observe también el primer nodo creado: es el que aparece a la derecha.

#El siguiente paso es visualizar el dendograma: en un dendograma se observa el nivel de similitud entre las instancias, y como van agrupandose.
par(mfrow=c(1,1))
plot(hc)

#Para alinear los nombres de las hojas (filas):
plot(hc, hang = -1) 


#Vamos a cortar elárbol en 5 agrupaciones, con la función `cutree()`.
hc <- hclust(dist(USArrests), "cen") #euclidean distance al cuadrado, metodo centroid
memb <- cutree(hc, k = 5) #<-- poda del arbol para crear 5 grupos.

#En la variable **memb** se almacena el número de cluster asignado a cada ejemplo (en este caso un número entre 1 y 5). Si queremos conocer los elementos de un cluster:

rownames(USArrests)[memb == 3]


#A continuación se realizará usando el cuadrado de la distancia Euclídea, cortando los árboles en 10 agrupaciones y reconstruyendo la parte superior del árbol a partir de los centros de los clusters:
hc <- hclust(dist(USArrests)^2, "cen") #euclidean distance al cuadrado, metodo centroid
memb <- cutree(hc, k = 10) #<-- poda del arbol para crear 10 grupos.

cent <- NULL  #<--- ahora se obtienen los centroides de cada cluster
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
#volviendo a llamar a hclust con  la distancia entre centroides 
#y members contiene las muestras por cluster
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
#visualizacion de los dendogramas
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)

# Ahora representamos el clustering considerando las variables Murder y Assault
plot(USArrests$Murder,USArrests$Assault, col=memb)



### Probamos más métodos con UScitiesD y Chatterjee–Price Attitude Data

#Vamos a utilizar ahora otros métodos. Para ello consideramos otro conjunto de datos, UScitiesD, que contiene las distancias entre 10 ciudades de USA. Vamos a usar los resultados entre dos posibles algoritmos: "ward.D" y "ward.D2".
data(UScitiesD)
str(UScitiesD)

#Ahora se transformarán los datos en las dos direcciones principales:
#Classical Multidimensional Scaling o Principal Coordinates Analysis
#ver: https://www.sequentix.de/gelquest/help/principal_coordinates_analysis.htm
#   En este caso, se proyecta sobre dos ejes (k==2)
#   Observar el cambio de signo
mds2 <- -cmdscale(UScitiesD)

#Se visualiza el resultado de mds
#   observar el mapa de USA subyacente
#   una muy interesante transformacion
plot(mds2, type="n", axes=TRUE, ann=FALSE)
text(mds2, labels=rownames(mds2), xpd = NA)


#Ahora se usará el método hclust con dos métodos diferentes, ward.D y ward.D2.
#Se comparan los dos metodos de ward implementados
#   Por el tipo de agregacion, el ward.D propone Denver y Houston como mas similar a la
#   costa este
hcity.D  <- hclust(UScitiesD, "ward.D") # "wrong"
hcity.D2 <- hclust(UScitiesD, "ward.D2")
opar <- par(mfrow = c(1, 2))
plot(hcity.D,  hang=-1)
plot(hcity.D2, hang=-1)
par(opar)

#Finalmente, se puede probar con las distancias entre ciudades europeas:
#intente repetir esta experimentacion con el dataset eurodist
help(eurodist)



#Los datos del Chatterjee–Price Attitude Data son 30 muestras de 7 variables, la variable *rating* es la variable objetivo de una regresión. Este paquete viene con R.


#Cargando los datos y observando las correlaciones entre las variables:
str(attitude)
summary(attitude)
head(attitude)
corrgram(attitude)


#La distancia a utilizar es la distancia de Manhattan, obtenemos la matriz de distancias:
d <- dist(attitude, method= 'manhattan')


#Ahora vamos a comparar diferentes métodos para realizar el clusterizado jerárquico:
hc.ward <- hclust(d, method = 'ward.D')
hc.ward2 <- hclust(d, method = 'ward.D2')
hc.single <- hclust(d, method = 'sin') #single
hc.complete <- hclust(d, method = 'complete') #complete
hc.average <- hclust(d, method = 'av')  #average
hc.mcquitty <- hclust(d, method = 'mcq') #mcquitty
hc.median <- hclust(d, method = 'med') #median
hc.centroid <- hclust(d, method = 'cen')  #centroid

#Creamos una matriz de visualizaciones y ploteamos los diferentes dendogramas:
opar <- par(mfrow=c(2,4))
plot(hc.ward, hang=-1, ann=TRUE,main="ward",xlab='',ylab='')
plot(hc.ward2, hang=-1, ann=TRUE,main="ward.D2",xlab='',ylab='')
plot(hc.single, hang=-1, ann=TRUE,main="single",xlab='',ylab='')
plot(hc.complete, hang=-1, ann=TRUE,main="complete",xlab='',ylab='')
plot(hc.average, hang=-1, ann=TRUE,main="average",xlab='',ylab='')
plot(hc.mcquitty, hang=-1, ann=TRUE,main="mcquitty",xlab='',ylab='')
plot(hc.median, hang=-1, ann=TRUE,main="median",xlab='',ylab='')
plot(hc.centroid, hang=-1, ann=TRUE,main="centroid",xlab='',ylab='')
par(opar)


#Comparemos ahora los resultados al usar diferentes medidas de distancia, todos para el método single:
single.euclidean <- hclust(dist(attitude), method = 'single')
single.manhattan <- hc.single
single.maximum <- hclust(dist(attitude, method='maximum'), method = 'single')
single.canberra <- hclust(dist(attitude, method='canberra'), method = 'single')
single.minkowski <- hclust(dist(attitude, method = 'minkowski', p=3), method = 'single')


#Y la visualización posterior:
opar <- par(mfrow=c(3,2))
plot(single.euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(single.manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')
plot(single.maximum, hang=-1, ann=TRUE,main="maximum",xlab='',ylab='')
plot(single.canberra, hang=-1, ann=TRUE,main="canberra", xlab='',ylab='')
plot(single.minkowski, hang=-1, ann=TRUE,main="minkowski 3",xlab='',ylab='')
par(opar)



#Con los árboles de clusterizado se puede podar, lo que permite estudiar la mejor agrupación de los datos. Con cutree obtenemos para cada muestra a qué grupo pertenencen (lo tenemos hecho para cada distancia):
#podemos un poco los arboles: 
K = 10   #numero de clusters

memb.eu <- cutree(single.euclidean, k = K) #<-- poda del arbol para crear K grupos.
memb.mnh <- cutree(single.manhattan, k = K) #<-- poda del arbol para crear K grupos.
memb.max <- cutree(single.maximum, k = K) #<-- poda del arbol para crear K grupos.
memb.can <- cutree(single.canberra, k = K) #<-- poda del arbol para crear K grupos.
memb.min <- cutree(single.minkowski, k = K) #<-- poda del arbol para crear K grupos.

#Después, es posible obtener los centroides de cada grupo. Para ello, escogemos el promedio de todos los datos asignados a su grupo:
cent.eu <- NULL  #<--- ahora se obtienen los centroides de cada cluster
cent.mnh <- NULL  #<--- ahora se obtienen los centroides de cada cluster
cent.max <- NULL  #<--- ahora se obtienen los centroides de cada cluster
cent.can <- NULL  #<--- ahora se obtienen los centroides de cada cluster
cent.min <- NULL  #<--- ahora se obtienen los centroides de cada cluster
for(k in 1:K){
  cent.eu <- rbind(cent.eu, colMeans(attitude[memb.eu == k, , drop = FALSE]))
  cent.mnh <- rbind(cent.mnh, colMeans(attitude[memb.mnh == k, , drop = FALSE]))
  cent.max <- rbind(cent.max, colMeans(attitude[memb.max == k, , drop = FALSE]))
  cent.can <- rbind(cent.can, colMeans(attitude[memb.can == k, , drop = FALSE]))
  cent.min <- rbind(cent.min, colMeans(attitude[memb.min == k, , drop = FALSE]))
}


#Con las muestras asignadas a los grupos y las distancias entre entroides podemos obtener el clusterizado final:
#volviendo a llamar a hclust con  la distancia entre centroides y members contiene las muestras por cluster
cls.eu <- hclust(dist(cent.eu,method='euclidean'), method = "single", members = table(memb.eu))
cls.mnh <- hclust(dist(cent.mnh, method='manhattan'), method = "single", members = table(memb.mnh))
cls.max <- hclust(dist(cent.max,method='maximum'), method = "single", members = table(memb.max))
cls.can <- hclust(dist(cent.can,method='canberra'), method = "single", members = table(memb.can))
cls.min <- hclust(dist(cent.min,method='minkowski'), method = "single", members = table(memb.min))


#Finalmente, vamos a visualizar los resultados obtenidos con todas las muestras o con los centroides:
opar <- par(mfrow=c(3,2))
plot(single.euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(cls.eu, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
plot(single.manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')
plot(cls.mnh, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
plot(single.maximum, hang=-1, ann=TRUE,main="maximum",xlab='',ylab='')
plot(cls.max, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')

opar <- par(mfrow=c(3,2))
plot(single.euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(cls.eu, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
plot(single.manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')
plot(cls.mnh, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
plot(single.maximum, hang=-1, ann=TRUE,main="maximum",xlab='',ylab='')
plot(cls.max, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')

par(mfrow=c(2,2))
plot(single.canberra, hang=-1, ann=TRUE,main="canberra", xlab='',ylab='')
plot(cls.can, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
plot(single.minkowski, hang=-1, ann=TRUE,main="minkowski 3",xlab='',ylab='')
plot(cls.min, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
par(opar)



#Ahora representemos cómo se distribuyen las muestras en cada cluster para el gráfico de dispersión usando las variabels complaints y critical (o la mejor variable para cada caso):
par(mfrow=c(1,1))
plot(attitude$complaints, attitude$critical, col=memb.eu, main='euclidean', pch=17)
plot(attitude$complaints, attitude$privileges, col=memb.mnh, main='manhattan', pch=17)
plot(attitude$complaints, attitude$learning, col=memb.max, main='maximum', pch=17)
plot(attitude$complaints, attitude$learning, col=memb.can, main='canberra', pch=17)
plot(attitude$complaints, attitude$learning, col=memb.min, main='minkowski', pch=17)



## Comparación de dendogramas


library(dendextend)

# Carga de datos y generación de clusters con 2 métodos diferentes
data("USArrests")
df <- na.omit(USArrests)

#Construir matriz de distancias
	d <- dist(df, method = "euclidean") 

#Métodos de clustering
res.hc <- hclust(d, method = "ward.D2" )
res.agnes<-agnes(df, metric = "euclidean", stand = TRUE, method = "average")
res.diana<-diana(df, metric = "euclidean", stand = TRUE)

# Creamos una lista de dendogramas

den.hc<-as.dendrogram(res.hc)
den.diana<-as.dendrogram(res.diana)
den.agnes<-as.dendrogram(res.agnes)
dend_list <- dendlist(den.hc, den.diana)

# Uso de la función entanglement() para comparar dos dendogramas. 
#Cuanto más próximo a 0 más se parecen.
tanglegram(den.hc, den.diana ,main = paste("entanglement =", round(entanglement(dend_list), 2)))

#Diferencia relativa entre dendogramas con función all.equal(). 
#Si son iguales devuelve TRUE. En caso contrario retorna la distancia relativa.

all.equal(den.hc,den.diana)



# Métodos basados en densidad

require(cluster)

#Leemos el conjunto de datos multishapes del paquete factoextra 
# y nos quedamos con las dos primeras variables.
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

#Vamos a utilizar la función dbscan de los paquetes fpc y dbscan para realizar clustering. Observa el objeto que retornan. ¿Producen el mismo agrupamiento?. A continuación dibujamos  los resultados utilizando la función fviz_cluster (En el siguiente ejemplo db es el objeto que retorna dbscan y df un data.frame con los datos)


#La función `dbscan()` del paquete **fpc** es una implementación de DBSCAN (Density-based spatial clustering of applications with noise) que usa un tipo especial de árbol llamado *kd-tree*. Esta implementación es significativamente más rápido y puede trabajar con conjuntos de datos mucho mayores que  **dbscan** del paquete **dbscan**.

#La sintaxis de esta función (en ambos paquetes) es 

#`dbscan(x, eps, minPts = 5, weights = NULL, borderPoints = TRUE, ...)`

#donde 
#* x es una matriz de datos o un objeto distancia.

#* eps es el diámetro del entorno.

#* minPts: número de puntos en el entorno (5 por defecto)

#* weights pesos de los ejemplos en caso de que se realice clustering ponderado.

#* borderPoints: es un parámetro lógico que indica si los puntos frontera deben ser asignados (VERDADERO por defecto). En caso de FALSO estos puntos se consideran ruido.

#Retorna un objeto con el valor del parámetro *eps*, el de *minPts* y un parámetro llamado *cluster* que es un vector de enteros con la asignación al cluster. un 0 significa que el punto es ruido. 

library("fpc")
db1 <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

library("dbscan")
db2 <- dbscan::dbscan(df, eps = 0.15, minPts = 5)

library("factoextra")
par(mfrow=c(2,1))
fviz_cluster(db1, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point", palette = "jco", ggtheme = theme_classic())

fviz_cluster(db2, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

db3 <- fpc::dbscan(scale(df), eps = 0.15, MinPts = 5)
fviz_cluster(db3, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

#Ahora normaliza los datos con la función `scale()`, agrúpalos nuevamente con dbscan y observa los resultados. 
#*	¿Se produce el mismo agrupamiento?

#*	¿Cuál crees que es mejor? ¿Por qué?

#*	Existen 3 funciones en la librería dbscan para analizar las distancias de los k-vecinos más próximos. 
#  + kNN que retorna, para cada ejemplo, la distancia a sus k-vecinos más próximos (campo dist) y el identificador de esos k-vecinos (campo id).

#  + kNNdist, que calcula las distancias que se utilizan en kNNplot.

#  + kNNplot, que dibuja la distribución de distancias entre datos considerando el k-vecino más cercano. 
  
#Utilizando kNNplot, estima los valores de épsilon para k=5, 10,20,30,40. ¿Observas alguna tendencia, alguna dependencia entre épsilon y k? Elige para cada k valores de épsilon “no óptimos” (por ejemplo 0.5 y 0.4 para k=5) y observa las agrupaciones que se producen utilizando la función fviz_cluster (pon el parámetro ellipse a TRUE para que agrupe los clusters) 

#Comprueba que agrupaciones se producen para los valores de épsilon y MinPts estudiados. En el campo cluster del objeto que retorna la función dbscan tienes las asignaciones de cada ejemplo al cluster al que pertenece. Comprueba que ejemplos cambian de cluster o pasan de ser clasificados como ruido a pertenecer a un cluster.


d5<-dbscan::kNN(df,5)
d5d<-dbscan::kNNdist(df,5)
par(mfrow=c(1,1))
dbscan::kNNdistplot(df,5)
abline(h=0.5, lty=2)
abline(h=0.13, lty=4)

dbscan::kNNdistplot(df,10)
dbscan::kNNdistplot(df,20)
dbscan::kNNdistplot(df,30)
dbscan::kNNdistplot(df,40)

db0.5 <- fpc::dbscan(df, eps = 0.5, MinPts = 5)
db0.4 <- fpc::dbscan(df, eps = 0.4, MinPts = 5)
db0.1 <- fpc::dbscan(df, eps = 0.13, MinPts = 5)

resul<-list()
k=1
for (i in 0.10:0.17)
  for(j in 2:7)
  {
    {
        resul[[k]]<-fpc::dbscan(df, eps = i, MinPts = j)
        k=k+1
        
    }
}


fviz_cluster(resul[[3]], data = df, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())



fviz_cluster(db0.5, data = df, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
fviz_cluster(db0.4, data = df, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

fviz_cluster(db0.1, data = df, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
