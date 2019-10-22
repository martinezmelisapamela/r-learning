# Árboles de Decisión


#En estos ejercicios veremos como entrenar y evaluar los métodos de clasificación basados en Árboles de decisión. El paquete `caret` proporciona muchos métodos, pero vamos a utilizar los más estandar, que son C4.5, C5.0 y CART. Los dos primeros, como ya se ha explicado anteriormente, son básicamente el mismo en cuanto al núcleo de genración del árbol de decisión, pero el segundo incluye funcionalidades adicionales. 
#Empezaremos con los datos del conjunto Sonar que está incluido en el paquete `mlbench` y contiene el análisis de los datos de un Sonar para clasificar el material analizado como "Roca" o "Metal". Está formado por $60$ variables que toman valores entre $0$ y $1$, así que no será necesario escalar. Tampoco tiene valores faltantes. 


library(caret)
library(mlbench)
library(skimr)
library(corrgram)

library(rattle)

data(Sonar)
summary(Sonar) 

#Como podemos observar, no existen missing values. 

set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]
#Vamos a estudiar a continuación la distribución de los datos 
#mediante featurePlot
featurePlot(training[,c(1:6)], training[,61], "density")

#Como se puede observar, hay muchas variables 
#que tienen un comportamiento muy similar con respecto a la clase objetivo.
#Vamos a analizar los datos con un poco más de 
#detalle.

cort = cor(training[,-ncol(training)])
corrgram(training[,-ncol(training)], order=TRUE, lower.panel=panel.shade, upper.panel=NULL) 
print(sum(abs(cort[upper.tri(cort)]) > 0.8))

###Veamos que variables están altamente correladas

fc<-findCorrelation(cort, cutoff = 0.8, verbose = TRUE, names = TRUE)
coro = cor(training[, fc])

##Mostramos el correlograma solo de las variables correladas
corrgram(training[,fc], order=TRUE, lower.panel=panel.shade, upper.panel=panel.conf) 

###Como se puede observar, hay muchas variables correladas. 
##Eliminamos las variables correladas
###tanto en el conjunto de entrenamiento como en el conjunto de test.

training1<-training[,setdiff(names(training),fc)]
testing1<-testing[,setdiff(names(training),fc)]

###Buscamos variables con poca variabilidad
nearZeroVar(training1)
checkConditionalX(training1[,-ncol(training1)], training1[,ncol(training1)])

#Como podemos observar por la salida de estas dos funciones, 
#no existen variables con poca variabilidad.


#Vamos a experimentar con este conjunto de datos. En primer lugar establecemos los parámetros de experimentación. 
#Vamos a emplear el método de Validación cruzada básico, con 10 cajas, 
#y vamos a pedir que nos retorne la probabilidad de cada clase, con el parámetro `classProbs =TRUE`.

fitControl <- trainControl(method = "cv",
                           number = 10,
                           classProbs = TRUE)

#A continuación utilizamos la función `train` para construir un modelo por cada algoritmo. Como esta función va a probar con distintos valores de los parámetros, mostraremos los resultados para la mejor configuración de cada caso.

## C4.5 y C5.0

#El método asociado con C4.5 en train "J48". Por defecto se optimizan dos parámetros: `C`, que indica el nivel de confianza sobre nuestros datos (m??s alto, menos preocupación por el sobreajuste), y `M` que es el m??nimo número de ejemplos por nodo.

c4.5Fit <- train(
  Class ~ .,
  data = training1,
  method = "J48",
  trControl = fitControl
)

#El resultado del entrenamiento está almacenado en el objeto c4.5Fit. Podemos observar el entrenamiento realizado simplemente invocando al objeto. Tambi??n podemos observar que tal ha ido el entrenamiento utilizando la función `summary(c4.5Fit)`, que nos proporciona las medidas de evaluación y la matriz de confusión. Tambi??n se puede acceder a información parcial a trav??s del objeto. Si llamamos a la función `plot`, pasíndole como argumento el objeto que retorna la función `train`, representar?? el porcentaje de acierto obtenido seg??n los distintos parámetros de que se optimizan. En el caso de C4.5, se optimiza el número m??nimo de ejemplos por hoja.

c4.5Fit
summary(c4.5Fit)
c4.5Fit$results[rownames(c4.5Fit$bestTune),1:4]
plot(c4.5Fit)

plot(varImp(c4.5Fit), top=5)

###Podemos observar que el porcentaje de acierto se obtiene cuando el número de ejemplos 
#mínimo es 2, y el umbral de confianza es 0.010 
#(esto se puede ver tambi??n en la información contenida en c4.5Fit)

###Para ver el árbol obtenido
plot(c4.5Fit$finalModel)

#Vamos a repetir ahora el experimento, utilizando C5.0 en vez de C4.5


c5.0Fit <- train(
  Class ~ .,
  data = training1,
  method = "C5.0",
  trControl = fitControl
)

c5.0Fit
###Podemos observar que en C5.0 se optimizan parámetros distintos. 
#En concreto, por defecto se optimiza el tipo de salida (arbol o reglas), 
#si se realiza filtrado de variables o no (winnow) y 
#el número de iteraciones del Boosting (trials)
summary(c5.0Fit)
c5.0Fit$results[rownames(c5.0Fit$bestTune),1:5]
plot(c5.0Fit)

plot(c5.0Fit$finalModel) #Solo si el modelo óptimo es un árbol

grid <- expand.grid(.winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

c5.0Fitg <- train(
  Class ~ .,
  data = training1,
  method = "C5.0",
  trControl = fitControl,
  tuneGrid=grid
)

c5.0Fitg


## CART
#Con respecto a CART, en caret existen varias implementaciones, que básicamente difieren en en la forma de tratar el sobreajuste. Utilizaremos el método `rpart` que optimiza un parámetro `cp` cuyo valor determina si se introduce un nuevo nodo en el árbol, de forma que este nodo solo se introducir?? cuando permita que se reduzca el error en al menos el valor de `cp`. La opción "rpart2" simplemente controla la complejidad estableciendo una profundidad m??xima del árbol. 

set.seed(725)
rpartFit <- train(
  Class ~ .,
  data = training1,
  method = "rpart",
  trControl = fitControl,
  tuneLength = 6
)
rpartFit$results[rownames(rpartFit$bestTune),1:3]

###Visualizamos
plot(rpartFit)
library(rattle)
fancyRpartPlot(rpartFit$finalModel)

set.seed(725)
rpart2Fit <- train(
  Class ~ .,
  data = training1,
  method = "rpart2",
  trControl = fitControl,
  tuneLength = 6
)
rpart2Fit$results[rownames(rpart2Fit$bestTune),1:3]

###Visualizamos
plot(rpart2Fit)
fancyRpartPlot(rpart2Fit$finalModel)


#Vamos a resumir los resultados

rbind(c4.5Fit$results[rownames(c4.5Fit$bestTune),3:4],
      c5.0Fit$results[rownames(c5.0Fit$bestTune),4:5],
rpartFit$results[rownames(rpartFit$bestTune),2:3],
rpart2Fit$results[rownames(rpartFit$bestTune),2:3])


#Parece que el mejor método, a la vista de los resultados es C5.0. 
#Pero debemos ser cautos puesto que estos resultados se producen sobre ejemplos usados para entrenar. 
#Es deseable, como hemos visto, que probar la bondad de estos métodos sobre ejmplos no vistos, los que hemos guardado antes en la variable `testing1`


## Error de Generalización

#Como ya tenemos el entrenamiento realizado, lo que nos falta es comprobar como se comportan esos modelos sobre jemplos no vistos. Para ello, utilizamos la función `predict` para comprobar si el modelo seleccionado funciona igual de bien con datos no vistos.
# La función predict requiere dos argumentos, el objeto que devuelve la función train, y el conjunto de ejmplos no vistos
pc5.0<-predict(c5.0Fit,testing1)

#Esta función devuelve la clase de cada ejemplo de test. 

#La función confusionMatrix() necesita dos asignaciones de clase por cada ejemplo, 
#la dada por el método de clasificación, y la real, 
#y devuelve el número de discordancias.

confusionMatrix(pc5.0,testing1$Class)


confusionMatrix(predict(c4.5Fit,testing1),testing1$Class)$overall[1:2]
confusionMatrix(predict(c5.0Fit,testing1),testing1$Class)$overall[1:2]
confusionMatrix(predict(rpartFit,testing1),testing1$Class)$overall[1:2]
confusionMatrix(predict(rpart2Fit,testing1),testing1$Class)$overall[1:2]

## Metodos vagos

set.seed(725)
knnFit <- train(
  Class ~ .,
  data = training1,
  method = "knn",
  trControl = fitControl,
  tuneLength = 6
)
knnFit$results[rownames(knnFit$bestTune),1:3]
confusionMatrix(predict(knnFit,testing1),testing1$Class)$overall[1:4]

#Este modelo muestra mejores resultados que los anteriores basados en árboles de decisión. 
#Sin embargo hay que tener cuidado cuando hay muchas variables, aunque estas sean irrelevantes.


library(mlbench)
library(caret)
set.seed(564)
training <- as.data.frame(mlbench.circle(1000, d = 4))
set.seed(465)
testing <- as.data.frame(mlbench.circle(500, d = 4))

knnFitml<- train(
  classes ~ .,
  data = training,
  method = "knn",
  trControl = fitControl,
  tuneLength = 6
)
knnFitml$results[rownames(knnFitml$bestTune),1:3]

confusionMatrix(predict(knnFitml,testing),testing$classes)$overall[1:4]


#Ahora vamos a expandir la tabla de datos con más columnas compuestas por números aleatorios. 
#Como no se cambian los valores que determinan la variable objetivo parecer lógico esperar 
#que el resultado con las nuevas variables sea el mismo que antes, pero no es así. 
#Este paradigma usa todas la variables del conjunto de datos sin discriminar las que pueden ser más influyentes de las que no.

set.seed(725)
trainingLarge <- cbind(training,matrix(runif(20000,-1,1),nrow = 1000))
testingLarge <- cbind(testing,matrix(runif(10000,-1,1),nrow = 500))

knnFitTrainingLarge <- train(
  classes ~ .,
  data = trainingLarge,
  method = "knn",
  trControl = fitControl,
  tuneLength = 6
)
knnFitTrainingLarge$results[rownames(knnFitTrainingLarge$bestTune),1:3]
confusionMatrix(predict(knnFitTrainingLarge,testingLarge),testingLarge$classes)$overall[1:4]

#Además, si los valores de las variables no están normalizados y las variables que son irrelevantes tienen valores 
#mayores el problema se puede hacer incluso más grave.

set.seed(379)
#Las nuevas columnas tienen valores entre 0 y 100
trainingLarge <- cbind(training,matrix(runif(20000,0,100),nrow = 1000)) 
colnames(trainingLarge) <- make.names(colnames(trainingLarge),unique = T)
names(trainingLarge)[1:4]<-c("XX1","XX2","XX3","XX4")
testingLarge <- cbind(testing,matrix(runif(10000,0,100),nrow = 500))
names(testingLarge)[1:4]<-c("XX1","XX2","XX3","XX4")
colnames(testingLarge) <- make.names(colnames(testingLarge),unique = T)
levels(trainingLarge$classes)<-c("uno","dos")
levels(testingLarge$classes)<-c("uno","dos")

set.seed(725)
knnFitTrainingLarge <- train(
  classes ~ .,
  data = trainingLarge,
  method = "knn",
  trControl = fitControl,
  tuneLength = 6
)
knnFitTrainingLarge$results[rownames(knnFitTrainingLarge$bestTune),1:3]
confusionMatrix(predict(knnFitTrainingLarge,testingLarge),testingLarge$classes)$overall[1:4]

#¿Afectarían estos cambios a los modelos de árboles de decisión? Veámoslo.
set.seed(725)
rpartFitTrainingLarge <- train(
  classes ~ .,
  data = trainingLarge,
  method = "rpart",
  trControl = fitControl,
  tuneLength = 6
)
rpartFitTrainingLarge$results[rownames(rpartFitTrainingLarge$bestTune),1:3]
confusionMatrix(predict(rpartFitTrainingLarge,testingLarge),testingLarge$classes)$overall[1:4]

#La verdad es que no, los algoritmos para generar árboles de decisión solo añaden 
#variables que ayuden a predecir la clase. Comprueba que este último árbol no contiene 
#ninguna de las variables irrelevantes. Además, tampoco tiene efecto que haya distinto 
#rango de valores en la variables ya que los árboles solo comparan estos valores por cada variable de forma independiente.

#Estos problemas que presenta el paradigma de los vecinos más cercanos 
#se pueden solucionar preprocesando los datos antes de construir el modelo. 
#Por un lado se puede realizar una selección de variables para eliminar 
#las que parecen irrelevantes, por otro se pueden normalizar los valores. 
#Esto último lo puede hacer la función `train` directamente mediante el parámetro `preProcess`, 
#por ejemplo se puede indicar `preProcess = c("center","scale")` para estandarizar los datos. 
#Realiza este cambio y comprueba el resultado.

#Además del problema del ruido en los datos puede ocurrir que no se disponen de todas la variables necesarias
#para predecir la variable objetivo. Puedes probar a eliminar una de las cuatro varibles que determinan la clase 
#en los conjuntos de datos anteriores, con ruido o sin ruido, para ver como afecta esto a los modelos.

trainingSinX.4 <- training[, -4]
testingSinX.4 <- testing[, -4]
trainingLargeSinX.4 <- trainingLarge[, -4]
testingLargeSinX.4 <- testingLarge[, -4]


# Comparación de modelos
#La información completa de la función `confusionMatrix` ya proporciona el resultados de 
#algunos tests estadísticos, pero se realizan comparado el modelo en cuestión con la predicción 
#basada solamente en la clase mayoritaria. Esta información es relevante para valorar un modelo de 
#forma individual pero también nos interesa valorar si la diferencia entre dos modelos es significativa o no.

#Vamos a comparar primero los dos últimos resultados, 
#el porcentaje de acierto de los modelos knnFitTrainingLarge y rpartFitTrainingLarge sobre 
#los datos apartados de testeo.


binom.test(c(0.76,1-0.76)* # el porcentaje de acierto, y el de error
             nrow(testingLarge), # multiplicado por el número de instancias
           # así obtenemos el número total de aciertos y fallos
           p = 0.52) # el porcentaje de acierto del modelo con el que comparar

#El resultado nos dice que, como es de esperar, hay una diferencia significativa entre los modelos 
#`rpart` y `knn`.


#Por último, vamos a comparar dos modelos en base a sus resultados de validación cruzada con el test de la *t*.
t.test(knnFitTrainingLarge$resample$Accuracy,rpartFitTrainingLarge$resample$Accuracy,paired = T)



## Para comprobar igualdad
 t.test(c5.0Fit$resample$Accuracy,c4.5Fit$resample$Accuracy,paired = T)



## Ejercicio: Optimiza los anteriores métodos con el parámetro expand.grid y compara todos los métodos para seleccionar el mejor





