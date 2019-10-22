
library(caret)


#Datos del problema: iris (nuestro amigo)
data(iris)
TrainData <- iris[,1:4]                  # Features de entrada o Predictores
TrainClasses <- iris[,5]                 # Etiquetas

#Lo primero: obtenga las caracteristicas de estos datos. 
#Y mas especificamente, cuantas etiquetas tenemos
summary(TrainData)
summary(TrainClasses)   

#Uso de train y trainControl para entrenar knn

ctrl <- trainControl(method = "none", classProbs = TRUE)

#entrenaremos un clasificador knn utilizando caret, sin validación cruzada
#sin ajuste de parametros, para k=3. 
#Usaremos los datos <TrainData,TrainClasses> para entrenar y para validar
#
knnk3 <- train(TrainData, TrainClasses,
               method = "knn",
               preProcess= c("center","scale"),
               tuneLength = 1,
               tuneGrid = data.frame(k=3),
               trControl = ctrl)

#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: 
#ya viene incluindo en knnk3
oknnk3 <- predict(knnk3, TrainData)
                  
#ahora se repetira lo mismo para k=5, almacenando los resultados y modelos en 
#las variables correspondientes.
# TO DO
knnk5 <- train(TrainData, TrainClasses,
               method = "knn",
               preProcess= c("center","scale"),
               tuneLength = 1,
               tuneGrid = data.frame(k=5),
               trControl = ctrl
               )
#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: 
#ya viene incluindo en knnk3

oknnk5 <- predict(knnk5, TrainData)


#Obtenga ahora los promedios de las medidas de prestaciones del clasificador knnk3
#utilice la funcion multiClassSummary

resKnn3<-confusionMatrix(oknnk3, TrainClasses)
datos3 <-data.frame(obs=TrainClasses, pred=oknnk3)
res3<-multiClassSummary(datos3, lev=levels(TrainClasses))


#Repita lo mismo para el clasificador knnk5
resKnn5<-confusionMatrix(oknnk5, TrainClasses)
datos5 <-data.frame(obs=TrainClasses, pred=oknnk5)
res5<-multiClassSummary(datos5, lev=levels(TrainClasses))


#Cual es mejor? 
#en ambos casos los resultados son lo mismo
#res3 y res5


#En el ejemplo anterior, existe un clasificador mucho mejor que otro.
#Pero, que pasaria en caso de enfrentarnos a datos no vistos anteriormente?
#Pues no se puede saber como trabajaran los clasificadores porque no tenemos 
#datos extra para realizar esta comprobacion.
#
#Por lo tanto, vamos entonces a dividir <TrainData,TrainClasses> en dos 
#conjuntos <trnD,trnC> para train y <tstD,tstC> para evaluar.
trnD = TrainData[c(1:40,51:90,101:140),] # Separar un conjunto para train 
trnC = TrainClasses[c(1:40,51:90,101:140)] # con sus etiquetas correspondientes
tstD = TrainData[c(41:50,91:100, 141:150),] # y otro para test
tstC = TrainClasses[c(41:50,91:100, 141:150)] #tambien con sus etiquetas

#entrenaremos un clasificador knn utilizando caret, sin validacion cruzada
#sin ajuste de parametros, para k=3. 
#Usaremos los datos <trnD,trnC> para entrenar y <tstD,tstC> para validar
#
knnk3b <- train(trnD, trnC,
                method = "knn",
                preProcess= c("center","scale"),
                tuneLength = 1,
                tuneGrid = data.frame(k=3),
                trControl = control
                )
#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: ya viene incluindo en knnk3
oknnk3b <- predict(knnk3b, tstD)

#ahora se repetira lo mismo para k=5, almacenando los resultados y modelos en 
#las variables correspondientes.
knnk5b <- train(trnD, trnC,
                method = "knn",
                preProcess= c("center","scale"),
                tuneLength = 1,
                tuneGrid = data.frame(k=5),
                trControl = control
                )
#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: ya viene incluindo en knnk3
oknnk5b <- predict(knnk5b, tstD)

#Obtenga ahora los promedios de las medidas de prestaciones del clasificador knnk3b
#utilice la funcion multiClassSummary
confusionMatrix(oknnk3b, tstC)
datos3b<-data.frame(obs=tstC, pred=oknnk3b)
res3b<-multiClassSummary(datos3b, lev=levels(TrainClasses))


#Repita lo mismo para el clasificador knnk5b
confusionMatrix(oknnk5b, tstC)
datos5b<-data.frame(obs=tstC, pred=oknnk5b)
res5b<-multiClassSummary(datos5b, lev=levels(TrainClasses))

#Cual es mejor? Hay diferencias con lo acontecido en la comparativa anterior?
# Tampoco hay diferencias


#Por casualidad, se comprobara ahora las prestaciones de knnk3b y knnk5b con todo
#el dataset
oknnk3bT = 
oknnk5bT = 

#Compare estos resultados con los obtenidos anteriormente.
#Existe algo sorprendente?


#############################################################################
#Lo que ha ocurrido es debido a:
# 1.- cuando entrenamos con todos los datos, no hay forma de evaluar el modelo,
#     al no disponer de datos extra para evaluacion.
# 2.- cuando entrenamos con un conjunto de datos para reservar otro conjunto
#     para utilizarlo en la evaluacion, estamos posiblemente obteniendo modelos
#     desviados. Si tenemos la mejor suerte del mundo, elegiremos un subconjunto
#     de train representativo de todos los casos, asi como un subconjunto de test
#     variado. ESTO ES CONFIAR DEMASIADO EN LA SUERTE.
# 3.- Como se pudo apreciar, el resultado tras evaluar sobre todo el conjunto de
#     entrenamiento y test (TrainData), los resultados son peores. Esto es debido
#     a lo comentado anteriormente sobre los subconjuntos de test, pero tambien 
#     es debido a que posiblemente no sean los mejores valores de k
#
#Que se puede hacer entonces?
# 1.- Introducir el ajuste de los parametros de los clasificadores
#Es decir, se fijaran combinaciones de valores de parametros que se consideren
#validos para entrenar el modelo. En el caso de knn, se podra dar valores impares
#del valor de k.
#
# 2.- Conjuntamente con validacion cruzada (y posiblemente, repeticiones).
#Con la validacion cruzada podremos entrenar/evaluar los modelos con diferentes
#pares de conjuntos de train/test. Luego, es posible obtener unos valores promedio
#de prestaciones de clasificador; este valor promedio sera mas independiente del
#remuestreo realizado.
#
#En definitiva, para cada configuracion de parametros se tendra el comportamiento
#medio, independiente del remuestreo, que permitira determinar cual o cuales son
#las mejores combiaciones de parametros.
#
#Todo esto se implementa usando la funcion trainControl y train con mas argumentos.

#especificar los valores posibles para los parametros
kvalues = data.frame(k = c(3,5,7, 9,11,13, 15,17,19, 21))
#
#cuando hay mas de un parametro, se debe tener un data.frame con tantas columnas como
#parametros del modelo, y en cada fila debe ir cada una de las combinaciones que se
#desea analizar.
#Se puede realizar a mano, pero existen metodos menos dolorosos.
#Por ejemplo, con expand.grid
#help(expand.grid)
#expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50), sex = c("Male","Female"))

#indicar que se desea validacion cruzada: en este caso, 10-fold cv
#ademas, que el metodo de ajuste de parametros sera el grid (porque se indicaran
#las posibles combinaciones de valores -en nuestro caso, kvalues-).
#a mayores se ha indicado retornar las probabilidad de las clases (knn no lo retorna),
#que almacene las predicciones finales del modelo (normalmente a FALSE), y que 
#la funcion de evaluacion sera la defaultSummary.
help("trainControl")
trnCtrl = trainControl(method = 'cv', number = 10, search = 'grid', 
                       savePredictions = TRUE, classProbs = TRUE )


#ahora es cuestion de llamar a la funcion train
help(train)
modelo = train(x=TrainData, y=TrainClasses,
               method = "knn",
               preProcess= c("center","scale"),
               metric = 'accuracy',
               tuneGrid = kvalues,
               trControl = trnCtrl
               )

#Observemos el modelo obtenido
print(modelo)
#Fijarse en las ultimas lineas impresas:
#   Accuracy was used to select the optimal model using the largest value.
#   The final value used for the model was k = 15.
#Previo a este mensaje, se puede ver los valores de Accuracy y Kappa para cada
#uno de los 10 modelos generados y evaluados con 10-fold cv.

#El modelo devuelto por train tiene varias variables miembro con nombre.
#teclee modelo$  y espere o tabule: verá todos las posibles variables 
# el mejor modelo
# la lista de parametros de control
#
# TO DO: compruebe los valores obtenidos y al mismo tiempo observe la ayuda
# de help(train) para entender el significado de cada una de las posibilidades


#Una vez obtenido el modelo, cada vez que se necesite clasificar se utilizara
#la funcion predict:
outModelo = predict(modelo, tstD)

multiClassSummary(data.frame(obs=tstC, pred=outModelo), lev = levels(tstC), model = 'knn')


#TO DO: evalue todos los modelos obtenidos hasta ahora con el dataset TrainData
#y compare los resultados. Es de verdad modelo el mejor de entre los encontrados?

#El restultado de este modelo hace un sobreajusta porque el test data es muy dirigidos
#Habria que reemplazar el conjunto de enteramiento por algo mas aleatorio.
# Por ejemplo:

#set.seed(998)
#inTraining <- createDataPartition(iris$Class, p = .75, list = FALSE)
#training <- iris[ inTraining,]
#testing  <- iris[-inTraining,]


#############################################################################
#Finalmente, indicar que existen alternativas a knn. Por ejemplo, el modelo
#knn3 permite acceder a los valores de probabilidad de cada etiqueta para cada
#ejemplo.
help(knn3)
#evaluar el modelo knn3 dados el conjunto de entrenamiento y las muestras a 
#clasificar, configurado para que retorne los valores de probabilidad
o3 = knn3Train()
#crear el data.frame con columnas obs, pred, setosa, versicolor y virginica
d3 = data.frame(obs=tstC, pred=factor(o3[1:30]), attr(o3,'prob'))

