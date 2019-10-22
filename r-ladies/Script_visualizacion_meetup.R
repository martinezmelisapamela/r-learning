getwd() #para saber donde se guardan los archivos
install.packages("plotly")     #Instalo el paquete plotly
library ("tidyverse","plotly")       #LLamo al paquete plotly
install.packages("ggplot2")     ##Instalo el paquete ggplot2
library ("ggplot2")                #LLamo al paquete ggplot2

#se puede guardar ahi el archivo propinas
propina<-read.csv('C:/Users/Pamela/Documents/r-ladies/scripts/DATOS/propina.csv')   #leo mi set de datos

#mi primer grafico con ggplot2
ggplot(data = propina, aes(x= total, y= propina)) + geom_point() + theme(aspect.ratio=1) #aes=atributos esteticos

ggplot(data = propina, aes(x= total, y= propina, colour= sexo )) + geom_point() + theme(aspect.ratio=1)

                                                                             
ggplot(data = propina, aes(x= total, y= propina, colour= sexo )) + geom_point(colour="blue") + theme(aspect.ratio=1) 

ggplot(data = propina, aes(x= total, y= propina, shape= sexo )) + geom_point() + theme(aspect.ratio=1)


ggplot(data = propina, aes(x= total, y= propina)) + geom_point(size=5, alpha=1/3) + theme(aspect.ratio=1) + labs(x= "Total de la cuenta", y="Propina dada por los clientes") #el alpha es la transparencia, el labs son los labels que se puede poner al grafico como referencia

ggplot(data = propina, aes(x= total, y= propina, colour= fuma )) + geom_point() + theme(aspect.ratio=1) 

ggplot() + layer(data = propina, mapping =aes(x= total, y= propina), geom = "point", stat="identity", position= "identity")

ggplot(data = propina, aes(x= total, y= propina, colour= fuma )) + geom_point() + theme(aspect.ratio=1) + facet_grid (sexo~fuma)  

# para graficos interactivos usamos la libreria plotly
library(plotly) #primero se llama a la libreria
#esta porcion de codigo muestra en el tooltip la propina y el sexo
pl <- ggplot(propina, aes(total, propina, colour = sexo)) + geom_point()
ggplotly(pl)

#esta porcion de codigo muestra en el tooltip solo el sexo
pl <- ggplot(propina, aes(total, propina, colour=sexo, text=paste('Sexo', sexo))) + geom_point()
ggplotly(pl, tooltip = "text")

