#4° meetup rladies
#instalo el dataset "desordenado"
install.packages("devtools")
devtools::install_github("rstudio/EDAWR")
#instalo tidyr
install.packages("tidyr")
#cargamos librerias
library(tidyr)
library(EDAWR)
#observamos los primeros casos
head(storms, 5) #la funcion head muestra los datasets
head(cases, 6)
head(pollution, 7)
#observamos que tipo de dato
str(storms)
str(cases)
str(pollution)
#funcion gather
gather (cases, year, n, 2:4)  
#funcion spread
spread (pollution, size, amount)  
#funcion separate
separate(storms, date, c("year", "month", "day"), sep = "-")
storms2<- separate(storms, date, c("year", "month", "day"), sep = "-")
#funcion unite
unite(storms2, date, year, month, day, sep = "-")

#instalo la libreria dplyr
install.packages("dplyr")
#instalo el dataset de los vuelos de NYC
install.packages("nycflights13")
#cargo las librerias dplyr
library(dplyr)
library(nycflights13)
#funcion select
select(storms2, storm, pressure)
select(storms, -storm)
select(storms, wind:date)
#funcion filter
filter(storms, wind>=50)
filter(storms, wind>=50, storm %in% c("Alberto", "Alex", "Allison"))
#mutate
mutate(storms, ratio=pressure/wind)
mutate(storms, ratio = pressure/wind, inverse = ratio^-1)
#summarise
pollution %>% summarise(mean = mean(amount), sum = sum(amount), n = n())
#arrange
arrange(storms, wind)
arrange(storms, wind, date)
#operador pipe
select(storms, storm, pressure)
storms %>% select(storm, pressure)
storms %>% filter(wind >=50)
#usando pipe
storms %>% 
  filter(wind >=50) %>%
  select(storm, pressure)

