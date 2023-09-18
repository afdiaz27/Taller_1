################################################
##Punto N3: Estimación del Perfil edad-salario##
##Cargue de paquetes
library (pacman)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

##Cargue de base de datos##
setwd("C:/users/USER/OneDrive - Pontificia Universidad Javeriana/Desktop/Taller_1/stores")
load("database_18_clean.Rda")

##Modelo linel##
#Seleccion de variable X
salario.por= c('age', 'age2')
X <-  df_clean %>% select(salario.por)

#Variable Y
y <- df_clean %>% 
  select(log_wageh)  %>%
  rename(y='log_wageh')

dat = cbind(X, y)

##Pre-estimacion
#instlacion paquete skim
install.packages("skimr")
library(skimr)
skim(dat)

##Estimacion modelo
install.packages("stargazer")
library(stargazer)
mod <- lm(y ~ ., data = dat, x = TRUE)
stargazer(mod, type="text")
library(stargazer)
stargazer(mod, out="Prueba1.tex")
writeLines(stargazer(mod, type="text"), "Prueba1.tex")

##Estimacion del logaritmo del salario predicho##
y_hat <- fitted(mod)
residuo <- resid(mod)
df_clean_p <- cbind(df_clean,residuo,y_hat)


##Graficar perfil de la edad contra el salario##
ggplot(df_clean_p , aes(y = y_hat, x = age)) +
  geom_point() + # add points
  stat_smooth(formula = 'y ~ x', method = lm, se = FALSE, 
              size = 1) +  #fit the linear model in the plot
  theme_bw() + #black and white theme
  labs(x = "edad",  
       y = "Logaritmo del Salario",
       title = "Perfil de edad contra salario") # labels

