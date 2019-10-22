

# El perceptron
#Vamos a construir la red neuronal más sencilla que hay, el Perceptrón lineal. Vamos a utilizar como datos de entrenamiento un subconjunto de los datos iris
# leemos el conjunto de datos
data(iris)
# Seleccionamos las longitudes del pétalo y del sépalo , y la clase
irissubdf <- iris[c(1:5,51:56), c(1, 3, 5)]
names(irissubdf) <- c("sepal", "petal", "species")
head(irissubdf)
summary(irissubdf)

# A�adimos una variable binaria como variable a predecir, 
#1 si es versicolor, -1 si es setosa
irissubdf[, 4] <- 1 #aca a�ade una columna de unos
irissubdf[irissubdf[, 3] == "setosa", 4] <- -1 #aca modifica esa col. para q ponga -1 cuando sea setosa

#separa las vbles explicativas de la vble respuesta NUMERICA
x <- irissubdf[, c(1, 2)]
y <- irissubdf[, 4]

#esto esta repetido
#3irissubdf[, 4] <- 1
#irissubdf[irissubdf[, 3] == "setosa", 4] <- -1
#x <- irissubdf[, c(1, 2)]
#y <- irissubdf[, 4]

## Vemos como estas dos clases son linealmente separables.

library(ggplot2)
Plotdatos<-ggplot(irissubdf, aes(x = sepal, y = petal)) +
  geom_point(aes(colour=species, shape=species), size = 3) +
  xlab("sepal length") +
  ylab("petal length") +
  ggtitle("Species vs longitud del sepalo y petalo")

Plotdatos

#La siguiente instruccion permite representar en cada iteracion el hiperplano de separacion que construye el perceptron.

#Como queremos ir viendo graficamente los pasos que hace el perceptron vamos 
#a definir una función que va a usar la grafica anterior como base y sobre ella va a representar 
#el hiperplano de cada paso y el punto que representa la instancia que se us� para actualizar 
#el vector $\mathbf{w}$ y por tanto el hiperplano.


dibuja.hiperplano<-function(x,b,w,punto=NA)
{
  
  plot(x, cex=0.2, xlim=c(-5,7), ylim=c(-2,5))
  points(subset(x,y==1), col="black", pch="+", cex=2)
  points(subset(x,y==-1), col="red", pch="*", cex=2)
  
  indep<- -b/w[2]
  pendiente<- -w[1]/w[2]
  abline(indep,pendiente, col="green")
  if (!is.na(punto)){points(punto, col="blue", pch="o", cex=2)}
}


#Vamos a utilizar como funcion de activacion la funcion de salto. 
#Si el producto de los pesos por cada ejemplo es $>0.5$, asignaremos el ejemplo
#a la clase positiva. En otro caso a la negativa.
#eta = tasa de aprendizaje con la que se actualizan los pesos

perceptron <- function(x, y, eta, niter) {
  
  # inicializacion vector de pesos
  pesos <- rep(0, ncol(x))
  b<-0
  errors <- rep(0, niter)
  
  
  # Bucle iteraciones
  for (jj in 1:niter) {
    
    # Bucle sobre el conjunto de entrenameinto
    for (ii in 1:length(y)) {
      
      # Prediccion de clase utilizando funcion salto
      z <- sum(pesos* as.numeric(x[ii, ])) + b
      if(z < 0) {
        ypred <- -1
      } else {
        ypred <- 1
      }
      
      # Modificacion de pesos
      delta.pesos <- eta * (y[ii] - ypred) * as.numeric(x[ii, ])
      pesos <- pesos + delta.pesos
      
      
      delta.b<- eta * (y[ii] - ypred) 
      b <- b + delta.b
      if (niter==1) {dibuja.hiperplano(x,b,pesos,x[ii,])}
      
      
      # Actualiza numero de errores
      if ((y[ii] - ypred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }
      
    }
    if (niter>1) {dibuja.hiperplano(x,b,pesos)}
  }
  
  res<-list(pesos=pesos,b=b,errors=errors)
  # muestra pesos
  
  return(res)
}

#Ahora vamos a ejecutar la funcion perceptron para comprender su funcionamiento. 
#Vamos a empezar fijando el numero de iteraciones a $1$. 
#Esto quiere decir que cada ejemplo de entrenamiento se presentara a la red una sola vez. 

#En cada iteracion del algoritmo la funcion `dibuja.hiperplano` representa el conjunto de datos, 
#el hiperplano de separacion que se construye en esa iteracion y el ejemplo que se esta presentando a la red, 
#como un circulo azul. 

#La salida se muestra en un pdf separado del resto. 

pdf("HiperplanoPerceptronIt1.pdf")
iter<-1
res <- perceptron(x, y, 1, iter)
dev.off()


#Como podemos observar, el algoritmo no consigue construir un hiperplano que separe las dos clases.
#Vamos a probar modificando el ratio de aprendizaje.


pdf("HiperplanoPerceptronIt1ap2.pdf")
iter<-1
res1 <- perceptron(x, y, 2, iter)
dev.off()


pdf("HiperplanoPerceptronIt1ap0-5.pdf")
iter<-1
res05 <- perceptron(x, y, 0.5, iter)
dev.off()


#Parece que algo no funciona bien, parece que el algoritmo no es capaz de encontrar el hiperplano.
#Vamos a probar ahora aumentando el numero de iteraciones, es decir, presentando mas de una vez cada ejemplo a la red. Vamos a probar con $5$ iteraciones. En este caso vamos a representar el hiperplano tras completar cada iteracion. Tambien vamos a representar el error tras cada iteracion, para estudiar su evolucion.

iter<-5
res5 <- perceptron(x, y, 1, iter)
plot(1:iter, res5$errors, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors", 
     main="Errores vs iteracion (5)")


iter<-10
res10 <- perceptron(x, y, 1, iter)
plot(1:iter, res10$errors, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors", 
     main="Errores vs iteracion (10) ")

#A la vista de la grafica de los errores, no parece que sean necesarias $10$ itercaiones, con $5$ tendriamos suficiente.
#Como ves, al final se consigue una linea, el hiperplano, que separa los puntos.  

# Redes neuronales

#Ahora que ya sabemos como funciona el perceptron, vamos a estudiar redes neuronales mas complejas. 
#Ahora utilizaremos el conjunto de datos denominado `Vehicle` que se puede encuentra en el paquete `mlbench`. Es un conjunto de 846 ejemplos de coches caracterizados por 18 variables y el tipo de coche, que es la variable a predecir. 
#Un vehiculo puede verse desde uno de muchos angulos diferentes. Las caracteristicas se extrajeron de las siluetas mediante la extension HIPS (Sistema de Procesamiento de Imagenes Jerarquicas) BINATTS, que extrae una combinacion de caracteristicas independientes de escala que utilizan tanto medidas clasicas basadas en momentos como la varianza escalada, la asimetria y la curtosis sobre la mayor / menor. Ejes y medidas heuristicas como huecos, circularidad, rectangularidad y compacidad.
#Se utilizaron cuatro vehiculos modelo "Corgie" para el experimento: un autobus de dos pisos, una camioneta Cheverolet, un Saab 9000 y un Opel Manta 400. 


library(caret)
library(mlbench)
library(NeuralNetTools)
data(Vehicle)


#Una vez hecho esto separamos una 
#porcion de los datos para la evaluacion final de los modelos y establecemos que el esquema de validacion va a ser bootstrap ponderado con 10 repeticiones. Como las instancias estan ordenadas por el indice de los participantes vamos a usar esta informacion para hacer la separacion entre entrenamiento y evaluacioln final.


ejTrain<-sample(1:nrow(Vehicle), 0.8*nrow(Vehicle),replace=FALSE) #hace la particion del conj de datos
training <- Vehicle[ejTrain,]
testing  <- Vehicle[-ejTrain,]




ctrl <- trainControl(
  method = "cv",
  number = 10
)

#Ahora pasamos a crear un primer modelo de red neuronal con una capa oculta. 
#Caret permite configurar el numero de neuronas en las capas ocultas de la red
#neuronal construida con el metodo `mlp` mediante el parametro `size`. 
#Para aprender esta red hay que ajustar $(18+1)*2 + (2+1)*4 =50$ pesos.

set.seed(1)
mlp1 <- caret:::train(Class ~., # llama asi porque puede suceder que tenga dos paquetes cargados con el mismo termino, al ponerle los puntitos lo que hago es especificar a que paquete me estoy refiriendo
                      data = training,
                      method = "mlp",
                      trControl = ctrl,
                      tuneGrid = data.frame(size=0)
)
mlp1$results


#Comprobamos los resultados ....el porcentaje de acierto es muy bajo. Vamos a probar otras configuraciones de la red, pero .... se nos ha olvidado que debemos normalizar los datos...., repetimos con un preprocesado para normalizar. Ahora, vamos a incluir el preprocesado dentro de la llamada a la función train.

set.seed(1)
mlp2 <- caret:::train(
  Class ~.,
  data = training,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = data.frame(size=0)
)
mlp2$results

#En la siguiente figura se muestra el error en cada iteración. El número de iteraciones por defecto es $100$. Se puede observar como el error desciende con el número de iteraciones.

ggplot() + geom_line(aes(x=1:length(mlp2$finalModel$IterativeFitError),
                         y=mlp2$finalModel$IterativeFitError)) +
  xlab("Iteraciones con 2 neuronas oculta. 1 capa") + ylab("Error")



#Simplemente unificando la escala de todas las variables, conseguimos duplicar el porcentaje de acierto. Incluimos el preprocesado en el resto de experimento que realicemos a partir de ahora. 

#Vamos a incrementar la complejidad de la red, 
#para ver si somos capaces de mejorar el aprendizaje.
#Si consideramos $10$ neuronas en una capa oculta, tendremos que ajustar 
#$(18+1)*10 + (10+1)*4 =234$ pesos,

set.seed(1)
mlp3 <- caret:::train(
  Class ~.,
  data = training,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  summaryFunction=multiClassSummary,
  tuneGrid = data.frame(size=10) #se consideran 10 neuronas
)

mlp3$results

ggplot() + geom_line(aes(x=1:length(mlp3$finalModel$IterativeFitError),
                         y=mlp3$finalModel$IterativeFitError)) +
  xlab("Iteraciones con 10 neuronas en 1 capa") + ylab("Error")

#Vemos que mejoran los resultados muuucho. 
#Por curiosidad, vamos a ver que ocurre cuando aumentamos la complejidad de la red, pero no escalamos los datos.

set.seed(1)
mlp4 <- caret:::train(
  Class ~.,
  data = training,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = data.frame(size=10)
)

mlp4$results

#Ahora comentar la linea del pre-processing y volver a ejecutar el train.
#Parece que los datos impiden el aprendizaje .....es muuuy importante preprocesar.


#Vamos ahora a añadir más complejidad, más neuronas, para ver si seguimos mejorando el porcentaje de acierto. Añadimos $50$ neuronas a la capa oculat, es decir, tenemos que ajustar $(18+1)*25 + (25+1)*4 =579$ pesos

set.seed(1)

mlp5 <- caret:::train(
  Class ~.,
  data = training,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = data.frame(size=25)
)

mlp5$results
summary(mlp5)

ggplot() + geom_line(aes(x=1:length(mlp5$finalModel$IterativeFitError),
                         y=mlp5$finalModel$IterativeFitError)) +
  xlab("Iteraciones con 25 neuronas en 1 capa") + ylab("Error")



#Parece que el porcentaje de acierto mejora cuando aumenta 
#la complejidad de la red, aunque se va estabilizando. 
#Vamos a optimizar el numero de capas ocultas de la red,
#utilizando la funcion `train`. 
#Primero utilizando el parametro `tuneLength` . 
#Mediante este parametro especificamos cuantos valores 
#distintos del numero de neuronas vamos a aprobar, 
#pero no cuales son esos valores. Los valores, los determina el metodo.

set.seed(1)
mlp6 <- caret:::train(
  Class ~.,
  data = training,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneLength=5
)


mlp6$results

#Como podemos observar este parametro empieza considerando prueba con 5 valores distintos, empezando por $1$, con paso $2$, pero ya hemos visto que con más neuronas en la capa oculta, mejoramos el porcentaje de acierto, asi que vamos a expecificarle nosotros el rango de valores que queremos comprobar.

set.seed(1)
mlp7 <- caret:::train(
  Class ~.,
  data = training,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = data.frame(size=seq(25,35,5)) #genera una secuencia entre 25 y 35 de 5 en 5
)
#muestro los resultados
mlp7$results

#Como se puede observar, no esta claro que aumentar la complejidad 
#repercuta en una mejora clara de los resultados, asi que vamos a 
#no superar las $25$ neuronas en la capa oculta, lo que, recuerda, 
#implicaba ajustar $579$ pesos

#Vamos a probar con redes mas profundas, 
#pero limitando la complejidad (numero de pesos a ajustar) 
#a un nivel similar a la red anterior. 
#Con esto queremos comprobar si a similar complejidad mayor 
#profundidad mejora los resultados. 
#Vamos a entrenar una red de $2$ capas y 
#una complejidad similar $(18+1)*16 + (16+1)*11 + (11+1)*4 = 581$.


set.seed(1)
mlp8 <- caret:::train(
  Class ~.,
  data = training,
  method = "mlpML", #mayor "anchura", permite a�adir capas ocultas
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = expand.grid(layer1=16,layer2=13,layer3=0) #aca le paso cuantas neuronas quiero por cada capa oculta (capa1, 2 y 3)
)


mlp8$results
summary(mlp8)

#La siguiente grafica muestra el comportamiento de la red cuando aumenta el numero de iteraciones.

ggplot() + geom_line(aes(x=1:length(mlp8$finalModel$IterativeFitError),
                         y=mlp8$finalModel$IterativeFitError)) +
  xlab("Iteraciones con 2 capas ocultas y similar número de pesos") + ylab("Error")



#Vamos a probar ahora con una red mas profunda. 

set.seed(1)
mlp9 <- caret:::train(
  Class ~ .,
  data = training,
  method = "mlpML",
  trControl = ctrl,
  preProcess=c("center","scale"),
  tuneGrid = expand.grid(layer1=seq(11,15,2),layer2=seq(9,11,2),layer3=seq(3,5,2)) #aca especificamos intervalos por capas que van de 2 en 2
)

mlp9$results
summary(mlp9)

### Mostramos la evolucion del error a lo largo del numero de iteraciones


ggplot() + geom_line(aes(x=1:length(mlp9$finalModel$IterativeFitError),
                         y=mlp9$finalModel$IterativeFitError)) +
  xlab("Iteraciones con 3 capas ocultas y similar número de pesos") + ylab("Error")


#Para una red de dos capas oculta se obtiene un resultado muy similar. Sin embargo, para una red con tres capas ocultas el resultado es más bajo, no mucho, pero quizá el incremento en complejidad no está justificado.
#Vamos a recapitular todos los resultados. No consideramos aquellos en los que no se han preprocesado los datos para realizar la comparación.

resultados<-rbind(mlp2$results[,c(2,3)], mlp3$results[,c(2,3)],mlp5$results[,c(2,3)],mlp7$results[3,c(2,3)],
                  c(mlp8$results$Accuracy,mlp8$results$Kappa),
                  c(mlp9$results$Accuracy[12],mlp9$results$Kappa[12]))


resultados

#A la vista de las Figuras anteriores, vemos que los errores cometidos son muy distintos cuando incluimos capas ocultas. Recuerda que lo que buscamos es un error lo más bajo posible,y que no haya resultados inesperados en la convergencia.

#Vamos ahora aumentar el numero de iteraciones, en lugar de las 100 por defecto, vamos a subirlo hasta $1000$. A�adimos el parametro `maxit`.

set.seed(1)
mlpit1<- caret:::train(
  Class ~ .,
  data = training,
  method = "mlp",
  trControl = ctrl,
  preProcess=c("center","scale"),
  tuneGrid = data.frame(size=25),
  maxit = 1000
)

mlpit1$results

set.seed(1)
mlpit2 <- caret:::train(
  Class ~ .,
  data = training,
  method = "mlpML",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = expand.grid(layer1=16,layer2=13,layer3=0),
  maxit = 1500
)

mlpit2$results

set.seed(1)
mlpit3 <- caret:::train(
  Class ~ .,
  data = training,
  method = "mlpML",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = expand.grid(layer1=14,layer2=10,layer3=5),
  maxit = 2000
)
resultadosit<-rbind(mlpit1$results[,c(2,3)], mlpit2$results[,c(4,5)],mlpit3$results[,c(4,5)])

#Ahora todos los modelos consiguen un mejor porcentaje de aciertos, pero el incremento es mayor en el 4 capas. Veamos de nuevo la curva de aprendizaje para cada modelo.

ggplot() + geom_line(aes(x=1:length(mlpit1$finalModel$IterativeFitError),
                         y=mlpit1$finalModel$IterativeFitError)) +
  xlab("1000 Iteraciones") + ylab("Error")

ggplot() + geom_line(aes(x=1:length(mlpit2$finalModel$IterativeFitError),
                         y=mlpit2$finalModel$IterativeFitError)) +
  xlab("1500 Iteraciones") + ylab("Error")

ggplot() + geom_line(aes(x=1:length(mlpit3$finalModel$IterativeFitError),
                         y=mlpit3$finalModel$IterativeFitError)) +
  xlab("2000 Iteraciones") + ylab("Error")

#Estas tres nuevas curvas que se muestran ya se parecen en un sentido, 
#hacia el final del numero de iteraciones la curva se mantiene casi horizontal. 
#Lo que es mas destacable, sin embargo, es que los modelos con mas capas muestran mucha variacion en el nivel de error. 
#Incluso, para la red de tres capas ocultas, en alguna iteracion se llega a doblar el error que se cometia en la iteracion anterior. �Puedes explicar estos fenomenos? �Se te ocurre alguna forma de eliminar la variacion tan grande del error entre iteraciones?
  
# Vamos a ver ahora como de buenos son estos modelos con datos nuevos.
cmmlp3<-caret:::confusionMatrix(predict(mlp3,testing),testing$Class)
cmmlp5<-caret:::confusionMatrix(predict(mlp5,testing),testing$Class)
cmmlp8<-caret:::confusionMatrix(predict(mlp8,testing),testing$Class)
cmmlp9<-caret:::confusionMatrix(predict(mlp9,testing),testing$Class)
cmmlpit1<-caret:::confusionMatrix(predict(mlpit1,testing),testing$Class)
cmmlpit2<-caret:::confusionMatrix(predict(mlpit2,testing),testing$Class)
cmmlpit3<-caret:::confusionMatrix(predict(mlpit3,testing),testing$Class)

Evaluacion<-function(x)
{
  return (apply(x,2,mean))
}

indices<-c(5,6,7,11)

perf<-rbind(Evaluacion(cmmlp3$byClass[,indices]),Evaluacion(cmmlp5$byClass[,indices]),
            Evaluacion(cmmlp8$byClass[,indices]),Evaluacion(cmmlp9$byClass[,indices]),
            Evaluacion(cmmlpit1$byClass[,indices]),Evaluacion(cmmlpit2$byClass[,indices]),
            Evaluacion(cmmlpit3$byClass[,indices]))
rownames(perf)<-c("1capa-10neuronas","1capa-25neuronas","2capas","3capas",
                  "1capa-25neuronas-it","2capas-it","3capas-it")


#Como se puede observar, las redes mejoran su rendimiento cuando se aumenta el numero de iteraciones, salvo las de una capa, que quiza requieren mas iteraciones.


# Representacion de redes neuronales

#Finalmente vamos a ver como pueden representarse las redes neuronales. Vamos a considerar una red peque�a, para visualizar mejor la representacion grafica.

#vamos a considerar un subconjunto del conjunto de datos inicial. Ademas usaremos el paquete `NeuralNetTools` para ayudarnos a realizar esta representacioN.


library(NeuralNetTools)
training <- Vehicle[ejTrain,c(1:8,19)]

set.seed(1)
mlpp <- caret:::train(
  Class ~ .,
  data = training,
  method = "mlp",
  preProcess=c("center","scale"),
  trControl = ctrl,
  tuneGrid = data.frame(size=3)
)

##Mostramos la arquitectura de la red



plotnet(mlpp$finalModel, x_names=names(training)[1:ncol(training)-1],y_names=levels(training$Class))


#Los pesos positivos entre capas se muestran como lineas negras y los pesos negativos como lineas grises. El grosor de la linea es proporcional a la magnitud absoluta de cada peso.

#Por otra parte, la funcion olden muestra la asociacion entre las variables de entrada y cada clase, estableciendo la importancia de cada variable en cada clase a partir de los pesos de la red. Como se puede observar, cada clase esta determinada por unas variables distintas.

par(mfrow=c(2,2))
olden(mlpp$finalModel, out_var='Output_bus')
olden(mlpp$finalModel, out_var='Output_opel')
olden(mlpp$finalModel, out_var='Output_saab')
olden(mlpp$finalModel, out_var='Output_van')
