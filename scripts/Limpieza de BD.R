########################## Limpieza de data #################################

##Cargue de paquetes
library (pacman)
p_load(tidyverse, tidymodels, glmnet)

##Cargue de base de datos y visualización
setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller_1/stores")
load("database_18.Rda")
view(database_18)

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

sapply(df, function(x) sum(is.na(x)))

##Se eliminan las variables y_bonificaciones_m, y_salarySec_m, y_ingLab_m_ha, p6620s1

df<- df %>%
  select(age, clase, college, cuentaPropia, dsi, estrato1, hoursWorkUsual, informal, ingtotob, maxEducLevel, microEmpresa, ocu, oficio, p6210, relab, sex, sizeFirm, y_total_m, y_total_m_ha)

NAN1_df <- data.frame(sapply(df, function(x) sum(is.na(x))))
colnames(NAN1_df)<- c("Numero de NAN")

##Se exporta la tabla de salida de Sapply
write.csv(NAN1_df, file = "sapply para NAN.csv", row.names=TRUE)

##Se eliminan las observaciones de NA para las variables categoricas

df_sin_nan<- df[!is.na(df$informal), ]

sapply(df_sin_nan, function(x) sum(is.na(x)))

df_sin_nan<- df_sin_nan[!is.na(df_sin_nan$y_total_m_ha), ]
sapply(df_sin_nan, function(x) sum(is.na(x)))

df_sin_nan<- df_sin_nan[!is.na(df_sin_nan$maxEducLevel), ]
sapply(df_sin_nan, function(x) sum(is.na(x)))

df_sin_nan<- df_sin_nan %>%
  select(age, clase, college, cuentaPropia, dsi, estrato1, hoursWorkUsual, informal, maxEducLevel, microEmpresa, ocu, oficio, relab, sex, sizeFirm, y_total_m, y_total_m_ha)

sapply(df_sin_nan, function(x) sum(is.na(x)))
NAN2_df <- data.frame(sapply(df_sin_nan, function(x) sum(is.na(x))))

colnames(NAN2_df)<- c("Numero de NAN")

##Se exporta la tabla de salida de Sapply
write.csv(NAN2_df, file = "sapply para NAN2.csv", row.names=TRUE)

##Verificación de ceros de la variable salario por hora y_total_m_ha

tabla_frecuencia_wage<- data.frame(table(df_sin_nan$y_total_m_ha))
print(tabla_frecuencia_wage)


boxplot(df_sin_nan$y_total_m_ha)
summary(df_sin_nan$y_total_m_ha)

install.packages("flextable")
library(flextable)

##Limpieza por outliers


std_wageh<- sd(df_sin_nan$y_total_m_ha)
print(std_wageh)
mean_wage<- mean(df_sin_nan$y_total_m_ha)
print(mean_wage)
tres_std_wageh<- std_wageh*3+mean_wage
print(tres_std_wageh)

df_clean <- df_sin_nan[df_sin_nan$y_total_m_ha <= 125000, ]
df_clean <- df_sin_nan[df_sin_nan$y_total_m_ha > 1006, ]

##Creación del logaritmo del salario

df_clean$log_wageh <- log(df_clean$y_total_m_ha)

#Creación de variable female, en la que hombre es la base

df_clean$female <- 0
df_clean$female <- ifelse(df_clean$sex==0, 1, df_clean$female)

##Verificando la variable female

veri_female <- df_clean[,c("sex","female")]
print(veri_female)

##Creación de edad al cuadrado

df_clean$age2 <- (df_clean$age)^2

##Se convierten en factores las variables categoricas

variables_categoricas <- c("estrato1", "maxEducLevel", "oficio", "relab", "sizeFirm")
df_clean<- df_clean %>%
  mutate_at(variables_categoricas, as.factor)

##Verificando el tipo de variable para asegurar que sea lo indicado (String, otro)

glimpse(df_clean)

##Cambiando female a fct

df_clean$female<- as.factor(df_clean$female)
str(df_clean)

##Guardando la base de datos limpia

save(df_clean, file = "d:/Javier/Desktop/UNIANDES/Big Data/Taller_1/stores/database_18_clean2.Rda")








