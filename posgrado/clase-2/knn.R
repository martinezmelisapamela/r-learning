#Para ejemplarizar todo estos conceptos se utilizara un codigo basado
#en el mostrado en:
#     https://topepo.github.io/caret/measuring-performance.html#probs


library(caret)

#generacion de los datos que simulan la evaluacion de un clasificador
#Primero: se genera el factor con la etiqueta real que se asigna a cada muestra
#    Se recomienda averiguar lo que hace cada función para entender el código
#    perfectamente.
set.seed(1000)
true_class <- sort(factor(sample(paste0("Class", 1:2),
                                 size = 1000,
                                 prob = c(.2, .8), replace = TRUE
                                 )
                          )
                  )

probs_class1 <- c( rbeta(sum(true_class == "Class1"), 4, 1),
                   rbeta(sum(true_class == "Class2"), 1, 2.5)
                  )
probs_class2 <- 1 - probs_class1

test_set <- data.frame(obs = true_class,
                       Class1 = probs_class1,
                       Class2 = probs_class2
                       )
test_set$pred <- factor(ifelse(test_set$Class1 > .5, 
                               "Class1", 
                               ifelse(test_set$Class1 < 0.5, 
                                      "Class2", 
                                      sample(paste0("Class", 1:2), size = 1, prob=c(0.5, 0.5))
                                      )
                               )
                        )

#obtener informacion de lo creado hasta ahora
#   Observar las muestras que pertenecerian a cada clase, tanto real como predicha
summary(test_set)

#No es casualidad los nombres de las variable miembro obs y pred de test_set:
#   en algunas de las funciones que se indican a continuacion es necesario que esos sean
#   los nombres para que las funciones operen correctamente.
#Ademas, aquellos modelos que sean probabilísticos generaran las probabilidades de cada
#clase al predecir un ejemplo. Estas probabilidades se deben almacenar en un vector cuyo
#nombre sera la etiqueta. De aquí que existan las variables miembro Class1 y Class2.


#Veamos los histogramas de probabilidad para cada clase
library(ggplot2)
ggplot(test_set,
       aes(x = Class1)) + #creo el ggplot con la variable test_set y el eje corresponde a 
  # las probabilidades indicadas en test_set$Class1
  geom_histogram(binwidth = .05) + #agnado un histograma para cada clase, ancho de bin 0.05
  facet_wrap(~obs) + #indico sobre que variable hay que realizar la cuenta en cada bin
  xlab("Probability of Class #1") #agnado rotulo


#Primer paso: evaluar el resultado de clasificacion
# se asume como clase positiva Class1 al ser el primer level de los factores obs y pred
cl_results <- confusionMatrix(data = test_set$pred, reference = test_set$obs)
print(cl_results)

# pero podemos especificar el segundo nivel
lvls <- levels(test_set$obs)
cl_results2 <- confusionMatrix(data = test_set$pred, reference = test_set$obs,
                               positive = lvls[2])
print(cl_results2)


#Algunos de los valores de medidas estadísticas de prestaciones de clasificacion
#se pueden calcular directametente con funciones:
#  sensitivity
#  specificity
#  posPredValue
#  negPredValue
#En todas ellas se tienen los mismos parametros: data, reference, positive
sensitivity(data=test_set$pred, reference=test_set$obs, positive = levels(test_set$obs))
specificity(data=test_set$pred, reference=test_set$obs, positive = levels(test_set$obs))
posPredValue(data=test_set$pred, reference=test_set$obs, positive = levels(test_set$obs))
negPredValue(data=test_set$pred, reference=test_set$obs, positive = levels(test_set$obs))

#Para problemas de dos clases, la funcion twoClassSummary retorna el area debajo
#de la curva ROC, la sensitivity y specificity
#   en este caso, twoClassSummary utiliza las probabilidades de cada clase.
#   Luego, en la llamada a train hay que activar a TRUE el argumento classProbs
twoClassSummary(test_set, lev = levels(test_set$obs))


#Hay mas funciones para estos fines.
# defaultSummary(data, lev = NULL, model = NULL)      <<<--- es un wrapper de postResample
# postResample(pred, obs)                             <<<--- Accuracy y Kappa
# mnLogLoss(data, lev = NULL, model = NULL)           <<<--- logaritmo de probabilidad de distribución multinomial
#          -logLoss = (-1/n)  SUM_{i=1}^n SUM_{j=1}^C y_{ij} * log(p_{ij})
# multiClassSummary(data, lev = NULL, model = NULL)   <<<--- retorna los estadísticos promediados de entre los calculados para cada clase
# prSummary(data, lev = NULL, model = NULL)           <<<--- Precision y Recall
#
#Intente utilizar satisfactoriamente estas funciones. 
# TO DO


#############################################################################
#Las medidas de prestaciones de clasificacion se pueden obtener tambien para los datos
#de entrenamiento. Y para problemas multiclase.
#
#Vamos a empezar a aplicar los conceptos de ajuste de clasificador y de validación cruzada.
#


#Datos del problema: iris (nuestro amigo)
data(iris)
TrainData <- iris[,1:4]                  # Features de entrada o Predictores
TrainClasses <- iris[,5]                 # Etiquetas

#Lo primero: obtenga las caracteristicas de estos datos. 
#Y mas especificamente, cuantas etiquetas tenemos
# TO DO


#Uso de train y trainControl para entrenar knn
#
#entrenaremos un clasificador knn utilizando caret, sin validación cruzada
#sin ajuste de parametros, para k=3. 
#Usaremos los datos <TrainData,TrainClasses> para entrenar y para validar
#
knnk3 <- train(TrainData, TrainClasses,
               method = "knn",
               preProcess = c("center", "scale"),
               tuneLength = 1,
               tuneGrid = data.frame(k=3),
               trControl = trainControl(method = "none",classProbs = TRUE))
#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: 
#ya viene incluindo en knnk3
oknnk3 <- predict(knnk3,TrainData)

#ahora se repetira lo mismo para k=5, almacenando los resultados y modelos en 
#las variables correspondientes.
# TO DO
knnk5 <- train(TrainData, TrainClasses,
               method = "knn",
               preProcess = c("center", "scale"),
               tuneLength = 1,
               tuneGrid = data.frame(k=5),
               trControl = trainControl(method = "none",classProbs = TRUE))
#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: 
#ya viene incluindo en knnk3
oknnk5 <- predict(knnk5,TrainData)


#Obtenga ahora los promedios de las medidas de prestaciones del clasificador knnk3
#utilice la funcion multiClassSummary
# TO DO
resultados<-confusionMatrix(oknnk3, TrainClasses)
oknnk3 <- predict(knnk3,TrainData)
datos3<-data.frame(obs=TrainClasses, pred=oknnk3)
res3<-multiClassSummary(datos3, lev=levels(TrainClasses))

datos5<-data.frame(obs=TrainClasses, pred=oknnk5)
res5<-multiClassSummary(datos5, lev=levels(TrainClasses))


#Repita lo mismo para el clasificador knnk5
# TO DO


#Cual es mejor? 
# TO DO


#En el ejemplo anterior, existe un clasificador mucho mejor que otro.
#Pero, que pasaria en caso de enfrentarnos a datos no vistos anteriormente?
#Pues no se puede saber como trabajaran los clasificadores porque no tenemos 
#datos extra para realizar esta comprobación.
#
#Por lo tanto, vamos entonces a dividir <TrainData,TrainClasses> en dos 
#conjuntos <trnD,trnC> para train y <tstD,tstC> para evaluar.
trnD = TrainData[c(1:40,51:90,101:140),] # Separar un conjunto para train 
trnC = TrainClasses[c(1:40,51:90,101:140)] # con sus etiquetas correspondientes
tstD = TrainData[c(41:50,91:100, 141:150),] # y otro para test
tstC = TrainClasses[c(41:50,91:100, 141:150)] #tambien con sus etiquetas

#entrenaremos un clasificador knn utilizando caret, sin validación cruzada
#sin ajuste de parametros, para k=3. 
#Usaremos los datos <trnD,trnC> para entrenar y <tstD,tstC> para validar
#
knnk3b <- train(trnD, trnC,
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 1,
                tuneGrid = data.frame(k=3),
                trControl = trainControl(method = "none",classProbs = TRUE))
#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: ya viene incluindo en knnk3
oknnk3b <- predict(knnk3b,tstD)
datos<-data.frame(obs=tstC, pred=oknnk3b)
res3b<-multiClassSummary(datos, lev=levels(TrainClasses))

#ahora se repetira lo mismo para k=5, almacenando los resultados y modelos en 
#las variables correspondientes.
knnk5b <- train(trnD, trnC,
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 1,
                tuneGrid = data.frame(k=5),
                trControl = trainControl(method = "none",classProbs = TRUE))
#como hemos utilizado preProcess en la configuracion de train, entonces no es
#necesario preprocesar nuestos datos de test: ya viene incluindo en knnk3
oknnk5b <- predict(knnk5b,tstD)
datos<-data.frame(obs=tstC, pred=oknnk5b)
res5b<-multiClassSummary(datos, lev=levels(TrainClasses))

#Obtenga ahora los promedios de las medidas de prestaciones del clasificador knnk3b
#utilice la funcion multiClassSummary
# TO DO

#Repita lo mismo para el clasificador knnk5b
# TO DO

#Cual es mejor? Hay diferencias con lo acontecido en la comparativa anterior?
# TO DO


#Por casualidad, se comprobara ahora las prestaciones de knnk3b y knnk5b con todo
#el dataset
oknnk3bT = predict(knnk3b, TrainData)
oknnk5bT = predict(knnk5b, TrainData)

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
trnCtrl = trainControl(method = 'cv', number = 10, search = 'grid',savePredictions = TRUE,
                       classProbs = TRUE, summaryFunction = defaultSummary)


#ahora es cuestion de llamar a la funcion train
help(train)
modelo = train(x=TrainData, y=TrainClasses, method = 'knn',
               preProcess = c('center', 'scale'),
               metric = 'Accuracy',
               trControl = trnCtrl,
               tuneGrid = kvalues
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



#############################################################################
#Finalmente, indicar que existen alternativas a knn. Por ejemplo, el modelo
#knn3 permite acceder a los valores de probabilidad de cada etiqueta para cada
#ejemplo.
help(knn3)
#evaluar el modelo knn3 dados el conjunto de entrenamiento y las muestras a 
#clasificar, configurado para que retorne los valores de probabilidad
o3 = knn3Train(trnD, tstD, trnC, k = 15, l = 0, prob = TRUE, use.all = TRUE)
#crear el data.frame con columnas obs, pred, setosa, versicolor y virginica
d3 = data.frame(obs=tstC, pred=factor(o3[1:30]), attr(o3,'prob'))
#evaluar la verosimilitud
mnLogLoss(data=d3, lev=levels(d3$obs), model='knn3')

