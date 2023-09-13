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

##Se seleccionan las variables de interés

##age, clase, college, cuentapropia, dsi, estrato1, hoursWorkUsal, informal, ingtotob, maxEducLevel
##microEmpresa, ocu, oficio, p6210, p6620s1, relab, sex, sizeFirm, y_bonificacio~m, y_salarySec_m, y_ingLab_m_ha, 
##y_total_m, y_total_m_ha

df<- df %>%
  select(age, clase, college, cuentaPropia, dsi, estrato1, hoursWorkUsual, informal, ingtotob, maxEducLevel, microEmpresa, ocu, oficio, p6210, p6620s1, relab, sex, sizeFirm, y_bonificaciones_m, y_salarySec_m, y_ingLab_m_ha, y_total_m, y_total_m_ha)

show(df)

##Se verifican los NAs de la base



