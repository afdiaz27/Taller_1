##Cargue de paquetes 
library (pacman)
p_load(tidyverse, tidymodels, glmnet)

##Cargue de base de datos
setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller_1/stores")
load("database_18.Rda")
view(database_18.Rda)

##Creación de dataframe para iniciar limpieza de base de datos
df<- database_18

#Analizando la estructura de la base de datos
glimpse(df)
