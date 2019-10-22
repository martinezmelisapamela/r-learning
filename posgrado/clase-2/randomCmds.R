library(mlbench)
data("Ionosphere")
dummy<-Ionosphere[ , c(sample(ncol(Ionosphere)-2,15), ncol(Ionosphere)) ]
#datos <- Ionosphere[,c(sample(ncol(Ionosphere)-1,15),ncol(Ionosphere))]
head(dummy)

help("trainControl")

#separo datos en train y test
datos<- Ionosphere[,c(sample(ncol(Ionosphere)-2,14),ncol(Ionosphere))]
inTraining <- createDataPartition(datos$Class, p = .75, list = FALSE)
train <- datos[ inTraining,]
test  <- datos[-inTraining,]

#borro columnas en 0
training <- train[,apply(datos,2,function(x){all(x!=0)})]
testing <- test[,apply(datos,2,function(x){all(x!=0)})]

dim(train)
dim(datos)

###
###

help("train")
head(train)
###

data("iris")
TrainData <- iris[,1:4] 
TrainClasses <- iris[,5]
head(TrainData)
head(TrainClasses)


#para generar todos los graficos en una misma grafica 
#se intala la libreria grid extra
#se llama asi
#library(gridExtra)
#grid.arrange(plot1, plot2,...,plotn, ncol=3, nrow=3) --->ahi le paso de cuanto
#por cuanto quiero la grilla


help("par")
help("hclust")
help("train")

set.seed(123)
   # Seleccionamos 15 filas de las 50 que tiene este dataset
   

data("USArrests")
head(USArrests)
ss <- sample(1:50, 15)
df <- USArrests[ss, ]
df

#Si quiero ver la estructuta interna de un dataset lo que uso es el comando str = structure
str(data)
 