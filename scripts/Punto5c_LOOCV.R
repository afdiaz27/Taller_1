###### Punto 5D - Estimación del LOOCV para los modelos con menor error en k-Fold Cross-Validation ####

# Se cargan los paquetes
library(pacman)
p_load(tidyverse, tidymodels, glmnet)

##Cargando la base de datos

##Cargue de base de datos y visualización
setwd("d:/Javier/Desktop/UNIANDES/Big Data/Taller_1/stores")
load("Punto5a_base_de_datos.Rda")

# Creación de las recetasde los mejores modelos - Modelo 7 y Modelo 10

recp_7<-recipe(log_wageh ~ female + age + age2 + informal + maxEducLevel + 
                 sizeFirm + relab , data=base_de_datos) %>% 
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies  

recp_10<-recipe(log_wageh ~ female + age + age2 + age3 + informal + maxEducLevel + 
                  sizeFirm + relab , data=base_de_datos) %>% 
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies 

#Creación de tipo de método de estimación

reglineal<-linear_reg()

#Creación de workflows

workf_7<-workflow() %>% add_recipe(recp_7) %>% add_model(reglineal)
workf_10<-workflow() %>% add_recipe(recp_10) %>% add_model(reglineal)

##Evaluación de modelos por LOOCV

#Primer modelo - Modelo 7

loocv_preds <- vector("numeric", length = nrow(base_de_datos))

for (i in seq_len(nrow(base_de_datos))) {
  loo_data <- base_de_datos[-i, ]
  loo_fit <- workf_7 %>% fit(data = loo_data)
  pred <- predict(loo_fit, new_data = slice(base_de_datos, i))$.pred
  loocv_preds[i] <- pred
}

temp <-bind_cols(base_de_datos$log_wageh, loocv_preds)

loocv_rmse1 <- rmse(temp, truth = ...1, estimate = ...2)

loocv_rmse1

#Segundo modelo - Modelo 10

loocv_preds <- vector("numeric", length = nrow(base_de_datos))

for (i in seq_len(nrow(base_de_datos))) {
  loo_data <- base_de_datos[-i, ]
  loo_fit <- workf_10 %>% fit(data = loo_data)
  pred <- predict(loo_fit, new_data = slice(base_de_datos, i))$.pred
  loocv_preds[i] <- pred
}

temp <-bind_cols(base_de_datos$log_wageh, loocv_preds)

loocv_rmse2 <- rmse(temp, truth = ...1, estimate = ...2)

loocv_rmse2



