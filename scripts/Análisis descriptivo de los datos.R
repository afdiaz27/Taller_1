####### Punto 2 - Descripción de la base de datos

##Cargue de paquetes
library (pacman)
p_load(rio,tidyverse, tidymodels, glmnet,skimr)

##Cargue de base de datos y visualización
setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller_1/stores")
load("database_18_clean.Rda")
View(df_clean)

glimpse(df_clean)

df_continuas<- df_clean %>% 
  select(age, hoursWorkUsual, y_total_m_ha)

View(df_continuas)

summary(df_clean)

summary(df_continuas)

desc_continuas<- summary(df_continuas)
library(stargazer)
stargazer(desc_continuas, out="Variables continuas.tex")
writeLines(stargazer(desc_continuas, type="text"), "Variables continuas.tex")

#### Tablas de frecuencia para variables dicotomas
tabla_frec_clase<- table(df_clean$clase)
print(tabla_frec_clase)

tabla_frec_female<- table(df_clean$female)
print(tabla_frec_female)

tabla_frec_cuentapropia<- table(df_clean$cuentaPropia)
print(tabla_frec_cuentapropia)

#### Boxplot para variables continuas

boxplot(df_clean$age,ylab="Edad")
boxplot(df_clean$y_total_m_ha,ylab="Salario por hora")
boxplot(df_clean$log_wageh,ylab="Logartimo del salario por hora")

#### Histogramas

hist(df_clean$age, main="Histograma de la edad", xlab="Edad",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)
hist(df_clean$y_total_m_ha, main="Histograma del salario por hora",xlab="Salario por hora",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)
hist(df_clean$log_wageh, main="Histograma del logaritmo del salario por hora",xlab="Logaritmo del salario por hora",ylab="Densidad/Frecuencia",col="darkblue", border = ("grey"), breaks=100)


box_plot <- ggplot(data=df_clean , mapping = aes(estrato1 , y_total_m_ha ))+ geom_boxplot()
box_plot

box_plot2 <- ggplot(data=df_clean , mapping = aes(estrato1 , hoursWorkUsual )) + geom_boxplot() 
box_plot2












