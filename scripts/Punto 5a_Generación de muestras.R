#####Punto 5.a

###Objetivo: Separar la muestra en dos: la parte de entrenamiento (70%) y la parte de prueba (30%)

##Cargue de paquetes

library(pacman)
p_load(tidyverse, tidymodels, glmnet, ggplot2)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

##Cargue de la base de datos

load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/database_18_clean.Rda")

base_de_datos <- df_clean 

##Creación de variables adicionales

#Creación de interacciones

base_de_datos <- base_de_datos %>% mutate(female_age = female*age)
base_de_datos <- base_de_datos %>% mutate(female_age2 = female*age2)
base_de_datos <- base_de_datos %>% mutate(age3 = age^3)
base_de_datos <- base_de_datos %>% mutate(age4 = age^4)
base_de_datos <- base_de_datos %>% mutate(age5 = age^5)
base_de_datos <- base_de_datos %>% mutate(log_age = log(age))
base_de_datos <- base_de_datos %>% mutate(female_age3 = female*age3)

#Creación de las muestras de entrenamiento y parte de prueba (Enfoque de Validation Set)

set.seed(198) #Definición de semilla para reproducibilidad de los datos

division_data <- initial_split(base_de_datos,prop=0.7) #Training set = 70% & Test set = 30%

training_set<-training(division_data)
test_set<-testing(division_data)

save(training_set,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/Punto5a_training_set.Rda")
save(test_set,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/Punto5a_test_set.Rda")
save(base_de_datos,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/Punto5a_base_de_datos.Rda")
