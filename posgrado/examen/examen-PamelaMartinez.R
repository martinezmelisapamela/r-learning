# Importo librerias
library(caret)
library(mlbench)
library(skimr)
library(rattle)
library(NeuralNetTools)
library(factoextra)
library(cluster)
library(NbClust)

#***************************#
#** Metodos supervisados  **#
#***************************# 


###########################
### Arboles de Decision ###
###########################

#cargo el dataset y seteo el valor de la semilla (DNI)
data("Ionosphere")
set.seed(30898558)
#selecciono 15 columnas del dataset
datos<- Ionosphere[,c(sample(ncol(Ionosphere)-2,14),ncol(Ionosphere))]
#la sentencia original era:
#datos <- Ionosphere[,c(sample(ncol(Ionosphere)-1,15),ncol(Ionosphere))]
#sin embargo cambie los valores originales ya que las entre las columnas
#que toma estan V1 y V11, lo cual en el momento de entrenar el algoritmo 
#me genera un error porque las interpreta como reptidas.

#separo datos en entrenamiento y prueba
inTraining <- createDataPartition(datos$Class, p = .75, list = FALSE)
train <- datos[ inTraining,]
test  <- datos[-inTraining,]
#borro columnas en 0
training <- train[,apply(datos,2,function(x){all(x!=0)})]
testing <- test[,apply(datos,2,function(x){all(x!=0)})]

#seteo trainControl con validacion cruzada
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE)
#entreno C4.5
c4.5Fit <- train(
  Class ~ .,
  data = training,
  method = "J48",
  trControl = ctrl
)
#hago la prediccion
pc4.5<-predict(c4.5Fit,testing)
#guardo resultados
cfm_pc4.5<-confusionMatrix(pc4.5,testing$Class)

#entreno C5.0
grid <- expand.grid(.winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
c5.0Fit <- train(
  Class ~ .,
  data = training,
  method = "C5.0",
  trControl = ctrl,
  tuneGrid = grid
)
#hago la prediccion
pC5.0<-predict(c5.0Fit, testing)
#guardo resultados
cfm_pc5.0<-confusionMatrix(pc4.5,testing$Class)

#entreno CART con rpart
rpartFit <- train(
  Class ~ .,
  data = training,
  method = "rpart",
  trControl = ctrl,
  tuneLength = 6
)
#hago la prediccion
prpart<-predict(rpartFit,testing)
#guardo resultados
cfm_rpart<-confusionMatrix(prpart, testing$Class)

#entreno CART con rpart2
rpart2Fit <- train(
  Class ~ .,
  data = training,
  method = "rpart2",
  trControl = ctrl,
  tuneLength = 6
)
#hago la prediccion
prpart2<-predict(rpart2Fit,testing)
#guardo resultados
cfm_prpart2<-confusionMatrix(prpart2,testing$Class)

#resumen de resultados
accu<-c(cfm_pc4.5$overall[1], cfm_pc5.0$overall[1], 
       cfm_rpart$overall[1],cfm_prpart2$overall[1])
kpp<-c(cfm_pc4.5$overall[2], cfm_pc5.0$overall[2], 
              cfm_rpart$overall[2],cfm_prpart2$overall[2])
resultados_df<-data.frame(accu, kpp)
names(resultados_df)<-c("Accuracy", "Kappa")
rownames(resultados_df) <-c("C4.5", "C5.0", "CART-Rpart", "CART-Rpart2")
resultados_df


#######
# KNN #
#######


#cargo el dataset y seteo el valor de la semilla (DNI)
data("Ionosphere")
set.seed(30898558)
#selecciono 15 columnas del dataset
datos<- Ionosphere[,c(sample(ncol(Ionosphere)-2,14),ncol(Ionosphere))]
#separo datos en entrenamiento y prueba
inTraining <- createDataPartition(datos$Class, p = .75, list = FALSE)
trainData <- datos[inTraining,1:14]
trainClasses<-datos[inTraining,15]
testData  <- datos[-inTraining,1:14]
testClasses  <- datos[-inTraining,15]

#seteo train control con validacion cruzada
trnCtrl = trainControl(method = 'cv', number = 10, search = 'grid', 
                       savePredictions = TRUE, classProbs = TRUE )

#posibles valores para los vecinos
kvalues = data.frame(k = c(12,14,16,18,20,22,24,26,28,30))

#entreno el algoritmo con accuracy como metrica
knn_1 <- train(trainData, trainClasses,
  method = "knn",
  preProcess= c("center","scale"),
  metric = "Accuracy",
  tuneGrid = kvalues,
  trControl = trnCtrl
)
#hago la prediccion
pkkn_1<-predict(knn_1,testData)

#guardo  resultados
pknn_1Res<-data.frame(obs=testClasses, pred=pkkn_1)
res_1<-multiClassSummary(pknn_1Res, lev=levels(testClasses))
KNNresultados<-res_1[1:3]
KNNresultados


####################
# Redes Neuronales #
####################

#cargo el dataset y seteo el valor de la semilla (DNI)
data("Ionosphere")
set.seed(30898558)
#selecciono 15 columnas del dataset
RNdatos<- Ionosphere[,c(sample(ncol(Ionosphere)-2,14),ncol(Ionosphere))]
#separo datos en entrenamiento y prueba
RNinTraining <- createDataPartition(RNdatos$Class, p = .75, list = FALSE)
RNtrain <- RNdatos[ RNinTraining,]
RNtest  <- RNdatos[-RNinTraining,]
#borro columnas en 0
RNtraining <- RNtrain[,apply(RNdatos,2,function(x){all(x!=0)})]
RNtesting <- RNtrain[,apply(RNdatos,2,function(x){all(x!=0)})]
#seteo train control
ctrl <- trainControl(method = "cv", number = 10)

#1° entrenamiento con 40 neuronas en 1 capa
mlp1 <- train(
  Class ~.,
  data = RNtraining,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = data.frame(size=40)
)

#2° entrenamiento con 15 capas ocultas
mlp2 <- train(
  Class ~.,
  data = RNtraining,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneLength=15
)

#3° entrenamiento con un valor de aprendizaje n=0.05
mlp3 <- train(
  Class ~ .,
  data = RNtraining,
  method = "mlpML",
  trControl = ctrl,
  tuneGrid = expand.grid(layer1=11,layer2=11,layer3=11),
  maxit = 1000,
  learnFuncParams = c(0.05,0)
)

#Resumen de resultados
MLP_res<-rbind(c(mlp1$results$Accuracy, mlp1$results$Kappa), c(mlp2$results$Accuracy[15],mlp2$results$Kappa[12]), c(mlp3$results$Accuracy,mlp3$results$Kappa))
MLPresultados_df<-data.frame(MLP_res)
names(MLPresultados_df)<-c("Accuracy", "Kappa")
rownames(MLPresultados_df) <-c("1° entrenamiento", "2° entrenamiento", "3° entrenamiento")  
MLPresultados_df



#*****************************#
#** Metodos No supervisados **#
#*****************************#

#cargo el dataset y seteo el valor de la semilla (DNI)
data("Ionosphere")
set.seed(30898558)
#selecciono 15 columnas del dataset
datos <- Ionosphere[,c(sample(ncol(Ionosphere)-1,15),ncol(Ionosphere))]
#borro columnas en 0
datos <- datos[,apply(datos,2,function(x){all(x!=0)})]
#cambio a numericas las vbles. categoricas, en el caso de las clases las cambio: good=2, bad =1
datos$Class = as.numeric(as.factor(datos$Class))

#estandarizo datos
datos.scaled <- scale(datos)

#busco un numero de cluster adecuado
fviz_nbclust(datos.scaled, kmeans, method = "wss")
nb<- NbClust(datos.scaled, distance = "euclidean", method = "kmeans")


##########################
### K-MEANS CLUSTERING ###
##########################

#En funcion del los resultados anteriores aplico kmeans utilizando 4 como cantidad de clusters
mdl_4c <- kmeans(datos.scaled, centers=4, nstart = 10 , iter.max = 50)

#Inspecciono el modelo resultante
mdl_4c$size
mdl_4c$withinss
mdl_4c$betweenss

#Visualizo clusters
fviz_cluster(mdl_4c, data=datos.scaled, geom = "point", ellipse.type = "norm")

##############################
### PAM Y CLARA CLUSTERING ###
##############################
require(cluster)
#En funcion del los resultados anteriores aplico PAM utilizando 4 como cantidad de clusters

#Metodo PAM
pam.res<-pam(datos.scaled, 4)
pam.res$clusinfo

#Visualizo clusters
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")

#Metodo CLARA
clara.res<-clara(datos.scaled, 4)
clara.res$clusinfo

#Visualizo clusters
fviz_cluster(clara.res, geom = "point", ellipse.type = "norm")

#############################
### CLUSTERING JERARQUICO ###
#############################
library(graphics)
library(corrgram)

#Linkage simple
#--------------

#1° iteracion de clusterizado sin arboles podados con linkage simple
hc.single_euclidean <-hclust(dist(datos.scaled,method='euclidean'), method = "single")
hc.single_manhattan <-hclust(dist(datos.scaled, method = "manhattan"), method = "single")

#Visualizo clusteres
plot(hc.single_euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(hc.single_manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')

#Podo arboles en 10 clusters
K=10
sgl.eu<-cutree(hc.single_euclidean, k=K)
sgl.mnh<-cutree(hc.single_manhattan, k=K)

#Obtengo centroides
cent.sgl_eu<-NULL
cent.sgl_mnh<-NULL
for(k in 1:K){
  cent.sgl_eu <- rbind(cent.sgl_eu, colMeans(datos.scaled[sgl.eu == k, , drop = FALSE]))
  cent.sgl_mnh <- rbind(cent.sgl_mnh, colMeans(datos.scaled[sgl.mnh == k, , drop = FALSE]))
}

#2° iteracion de clusterizado con arboles podados con linkage simple
hc1.single_euclidean <- hclust(dist(cent.sgl_eu,method='euclidean'), method = "single", members = table(sgl.eu))
hc1.single_manhattan <- hclust(dist(cent.sgl_mnh, method='manhattan'), method = "single", members = table(sgl.mnh))

#Muestro comparacion entre la 1° y 2° iteracion de clusterizado
plot(hc.single_euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(hc1.single_euclidean, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
plot(hc.single_manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')
plot(hc1.single_manhattan, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')


#Linkage Completo
#----------------

#1° iteracion de clusterizado sin arboles podados con lincage completo
hc.complete_euclidean <-hclust(dist(datos.scaled,method='euclidean'), method = "complete")
hc.complete_manhattan <-hclust(dist(datos.scaled, method = "manhattan"), method = "complete")

#Visualizo clusteres
plot(hc.complete_euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(hc.complete_manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')

#Podo arboles en 10 clusters
K=10
cmpl.eu<-cutree(hc.complete_euclidean, k=K)
cmpl.mnh<-cutree(hc.complete_manhattan, k=K)

#Obtengo centroides
cent.cmpl.eu<-NULL
cent.cmpl.mnh<-NULL
for(k in 1:K){
  cent.cmpl.eu <- rbind(cent.cmpl.eu, colMeans(datos.scaled[cmpl.eu == k, , drop = FALSE]))
  cent.cmpl.mnh <- rbind(cent.cmpl.mnh, colMeans(datos.scaled[cmpl.mnh == k, , drop = FALSE]))
}

#2° iteracion de clusterizado con arboles podados con linkage multiple
hc1.complete_euclidean <- hclust(dist(cent.cmpl.eu,method='euclidean'), method = "complete", members = table(cmpl.eu))
hc1.complete_manhattan <- hclust(dist(cent.cmpl.mnh, method='manhattan'), method = "complete", members = table(cmpl.mnh))

#Muestro comparacion entre la 1° y 2° iteracion de clusterizado
plot(hc.complete_euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(hc1.complete_euclidean, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')
plot(hc.complete_manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')
plot(hc1.complete_manhattan, hang=-1, ann=TRUE, main='por centros', xlab='', ylab='')


#Linkage Promedio
#------------------

#1° iteracion de clusterizado sin arboles podados con lincage completo
hc.avg_euclidean <-hclust(dist(datos.scaled,method='euclidean'), method = "average")
hc.avg_manhattan <-hclust(dist(datos.scaled, method = "manhattan"), method = "average")

#Visualizo clusteres
plot(hc.ward_euclidean, hang=-1, ann=TRUE,main="euclidean",xlab='',ylab='')
plot(hc.ward_manhattan, hang=-1, ann=TRUE,main="manhattan",xlab='',ylab='')


