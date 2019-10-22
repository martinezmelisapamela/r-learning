library(MASS)
library(MLmetrics)
library(caret)
library(partykit)
library(corrplot)

data("Boston")
head(Boston)
#help("Boston")

datos<-Boston
pairs(datos)
#coeficiente de correlacion cor(datos), order hace un clustering jearquico
corrplot.mixed(cor(datos), number.cex=0.7, order="hclust")

# Distribucion de densidad de las variables cuantitativas del modelo
library(dplyr)
require(psych)
multi.hist(x = datos, dcol = c("blue", "red"), 
           dlty = c("dotted", "solid"), main = "" )

###Se puede observar que no todas las variables siguen una distribucion normal.

#vble a predecir: medv


set.seed(123)
trainIndex <- createDataPartition(datos$medv, p = 0.8, list=FALSE, times=1)
subTrain <- datos[trainIndex,]
subTest <- datos[-trainIndex,]

###Establecemos los parametros del experimento
fitControl <- trainControl(method = 'cv', number=10, summaryFunction=defaultSummary)

#Empezaremos por los metodos mas sencillos, problemso con una regresion lineal multiple

set.seed(123)



##Entrenamos (regresion lineal)

fit.lm <- train(medv~., data=subTrain, method = 'lm', 
                        trControl=fitControl,metric='RMSE')

#veo que tiene el objeto
#fit.lm
summary(fit.lm)


fit.lm$finalModel
residuos<-rstandard(fit.lm$finalModel)

#par can be used to set or query graphical parameters. 
#Parameters can be set by specifying them as arguments 
#to par in tag = value form, or by passing them as a list 
#of tagged values
par(mfrow=c(2,2)) #setea parametros (?)

plot(fit.lm$finalModel)
par(mfrow=c(1,1))

#predigo 
medv.lmCV<-predict(fit.lm, subTest)

#comparo resultado real vs resultado esperado
#head(cbind(subTest$medv, medv.lmCV))
#que es el mae?
c(RMSE(subTest$medv, medv.lmCV) ,MAE(subTest$medv, medv.lmCV))

###Veamos si hay correlacion entre la variable a predecir y la prediccion

cor(medv.lmCV, subTest$medv)

require(car)
influencePlot(fit.lm$finalModel)


#R-squared indica que porcentaje de la varianza consigue explicar el modelo. Como el p-valor es significativo, podemos decir que existe una relacion entre los predictores y la variable respuesta. 
#Las variables con *** tienen una relacion estadisticamente significativa con la variable a predecir.


#Vamos a estudiar distintos métodos de generar árboles de regresión en R. En primer lugar trabajaremos con la función `
#ctree2 a través del paquete caret.

#Los parámetros que se pueden optimizar son:
# maxdepth: Profundidad máxima del árbol. El parámetro por defecto es $0$, que significa que no hay restricciones sobre el tamaño.
# mincriterion: El valor que se debe superar para realizar una division de un nodo.

#Arbol de regresion - metodos de mineria de datos
### Mediante la funcion expand.grid elegimos los valores con los que vamos a optimizar el arbol
Grid <- expand.grid(maxdepth = seq(5,20,5), mincriterion=c(0.75, 0.85, 0.95)) #genero matriz de parametros

##Entrenamos el arbol de regresion
fit.ctree2CV <- train(medv~., data=subTrain, method = 'ctree2', 
                      preProcess=c("center","scale"), 
                      trControl=fitControl,tuneGrid=Grid, metric='RMSE')

fit.ctree2CV

### Ahora estudiamos el comportamiento de los parámetros de optimización del método.
plot(fit.ctree2CV)

dib<-fit.ctree2CV$finalModel
library(party)
### Dibujamos el árbol

plot(dib, type="simple",           # no terminal plots
     inner_panel=node_inner(dib,
                            pval = FALSE,                 # no p-values
                            id = FALSE),                  # no id of node
     terminal_panel=node_terminal(dib, 
                                  abbreviate = TRUE,
                                  digits = 1,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE)
)


medv.ctree2CV<-predict(fit.ctree2CV, subTest)

#medidas del error
c(RMSE(subTest$medv, medv.ctree2CV) ,MAE(subTest$medv, medv.ctree2CV))

###Veamos si hay correlación entre la variable a predecir y la predicción

cor(medv.ctree2CV, subTest$medv)

#CART=classification and regression treee
#Vamos a probar ahora con el metodo CART, que ya hemos visto en árboles de decisión.

## hacemos lo mismo con rpart, que también lo hemos usado para construir árboles de clasificación.

Grid <- expand.grid(cp = seq(0,5,0.25))

##Entrenamos
fit.rpartCV <- train(medv~., 
                     data=subTrain, 
                     method = 'rpart', 
                     trControl=fitControl, 
                     preProcess=c("center","scale"), 
                     tuneGrid=Grid, metric='RMSE')

### En cada nodo del árbol está representado el número de nodo, la división que se hace, el número de ejmplos que cumplen la condición

fit.rpartCV
#fit.rpartCV$finalModel


plot(fit.rpartCV)


library(rattle)

fancyRpartPlot(fit.rpartCV$finalModel)




medv.rpartCV<-predict(fit.rpartCV, subTest)

c(RMSE(subTest$medv, medv.rpartCV) ,MAE(subTest$medv, medv.rpartCV))

cor(medv.rpartCV, subTest$medv)
#baja la correlacion => es un poco peor que el anterior
#se puede explorar lo que tiene final model:
#fit.rpartCV$finalModel$cptable
#fit.rpartCV$finalModel$variable.importance


Grid <- expand.grid(maxdepth = seq(2,15,1))

##Entrenamos
fit.rpart2CV <- train(medv~., data=subTrain, method = 'rpart2', preProcess=c("center","scale"), trControl=fitControl,tuneGrid=Grid, metric='RMSE')

plot(fit.rpart2CV)
library(rattle)

fancyRpartPlot(fit.rpart2CV$finalModel)

#predigo
medv.rpart2CV<-predict(fit.rpart2CV, subTest)

#evaluacion del error
c(RMSE(subTest$medv, medv.rpart2CV) ,MAE(subTest$medv, medv.rpart2CV))

cor(medv.rpart2CV, subTest$medv)


#Vamos a generar ahora Modelos de regresion, utilizando el metodo `M5'
#del paquete `caret`. Lo parámetros que se pueden optimizar para este tipo de árboles son:
#pruned: Si se realiza poda o no
# smoothed: Si se suaviza la regresion
# rules: Si se generan reglas o no

#Modelos de regresion, sustitutyen la hoja por una ecuacion teoricamente son mejores

##Entrenamos
fit.M5CV <- train(medv~., data=subTrain, method = 'M5', preProcess=c("center","scale"),trControl=fitControl, metric='RMSE')
#nota: ese tira errores de un paquete de computacion paralela que tiene R
fit.M5CV

#predigo
medv.M5CV<-predict(fit.M5CV, subTest)

#Evaluo Resultados
c(RMSE(subTest$medv, medv.M5CV) ,MAE(subTest$medv, medv.M5CV))
cor(medv.M5CV, subTest$medv)

fit.M5CV$finalModel

plot(fit.M5CV)
plot(fit.M5CV$finalModel)

## vemos de nuevo que preprocesar los datos tiene mucha importancia.

fit.mlp <- train(medv~., data=subTrain,
                      method = "mlp",
                      trControl = fitControl,
                      preProcess=c("center","scale"),
                      tuneGrid = data.frame(size=15)
)

fit.mlp$results

medv.mlp<-predict(fit.mlp, subTest)

c(RMSE(subTest$medv, medv.mlp) ,MAE(subTest$medv, medv.mlp))

fit.mlp7 <- train(
  medv~., data=subTrain,
  method = "mlpML",
  preProcess=c("center","scale"),
  trControl = fitControl,
  tuneGrid = expand.grid(layer1=16,layer2=13,layer3=0)
)

fit.mlp7$results

medv.mlpML<-predict(fit.mlp7, subTest)

c(RMSE(subTest$medv, medv.mlpML) ,MAE(subTest$medv, medv.mlpML))



fit.knn <- train(
  medv~., data=subTrain,
  method = "knn",
  preProcess=c("center","scale"),
  trControl = fitControl
)

fit.knn$results

medv.knn<-predict(fit.knn, subTest)

c(RMSE(subTest$medv, medv.knn) ,MAE(subTest$medv, medv.knn))



rbind(c(RMSE(subTest$medv, medv.lmCV) ,MAE(subTest$medv, medv.lmCV)),
      c(RMSE(subTest$medv, medv.ctree2CV) ,MAE(subTest$medv, medv.ctree2CV)),
      c(RMSE(subTest$medv, medv.rpartCV) ,MAE(subTest$medv, medv.rpartCV)),
      c(RMSE(subTest$medv, medv.rpart2CV) ,MAE(subTest$medv, medv.rpart2CV)),
      c(RMSE(subTest$medv, medv.M5CV) ,MAE(subTest$medv, medv.M5CV)),
       c(RMSE(subTest$medv, medv.mlp) ,MAE(subTest$medv, medv.mlp)),
      c(RMSE(subTest$medv, medv.mlpML) ,MAE(subTest$medv, medv.mlpML)),
      c(RMSE(subTest$medv, medv.knn) ,MAE(subTest$medv, medv.knn)))


par(mfrow=c(4,2))

plot(varImp(fit.lm))
plot(varImp(fit.ctree2CV))
plot(varImp(fit.rpartCV))
plot(varImp(fit.rpart2CV))
plot(varImp(fit.M5CV))
plot(varImp(fit.mlp))
plot(varImp(fit.mlp7))
plot(varImp(fit.knn))



