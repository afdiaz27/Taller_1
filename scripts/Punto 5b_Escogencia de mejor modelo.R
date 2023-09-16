###Punto 5.b.

##Objetivo: Comparar el desempeño predictivo de los modelos de los puntos 3 y 4, así como cinco (5) adicionales, en términos de RMSE

##Cargue de paquetes

library(pacman)
p_load(tidyverse, tidymodels, glmnet, ggplot2)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

##Cargue de bases

load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/Punto5a_training_set.Rda")
load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/Punto5a_test_set.Rda")
load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/database_18_clean.Rda")
base_de_datos <- df_clean

#Creación de variables adicionales

base_de_datos<-base_de_datos %>% mutate(age3 = age^3)
base_de_datos<-base_de_datos %>% mutate(age4 = age^4)
base_de_datos<-base_de_datos %>% mutate(age5 = age^5)

#Creación de recipes

recp_1<-recipe(log_wageh ~ 1,data=base_de_datos)

recp_2<-recipe(log_wageh ~ age + age2,data=base_de_datos)

recp_3<-recipe(log_wageh ~ female, data=base_de_datos)

recp_4<-recipe(log_wageh ~ female + age + age2 + informal + maxEducLevel + 
                 sizeFirm + relab , data=base_de_datos) %>% 
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies  


recp_5<-recipe(log_wageh ~ female + age + age2 + informal + maxEducLevel + 
                 sizeFirm + relab , data=base_de_datos) %>% 
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies  


recp_6<-recipe(log_wageh ~ female + age + age2 + informal + maxEducLevel + 
                 sizeFirm + relab , data=base_de_datos) %>% 
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies  


recp_7<-recipe(log_wageh ~ female + age + age2 + informal + maxEducLevel + 
                 sizeFirm + relab , data=base_de_datos) %>% 
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies  


recp_8<-recipe(log_wageh ~ female + age + age2 + age3,  data=base_de_datos) %>% 
  
recp_9<-recipe(log_wageh ~ female + age + age2 + age3+age4 , data=base_de_datos) %>% 
  
recp_10<-recipe(log_wageh ~ female + age + age2 + informal + maxEducLevel + 
                  sizeFirm + relab , data=base_de_datos) %>% 
  step_dummy(all_nominal_predictors())  #Convertir todas las variables categóricas a dummies  


#Creación de tipo de método de estimación

reglineal<-linear_reg()

#Creación de workflows

workf_1<-workflow() %>% add_recipe(recp_1) %>% add_model(reglineal)

workf_2<-workflow() %>% add_recipe(recp_2) %>% add_model(reglineal)

workf_3<-workflow() %>% add_recipe(recp_3) %>% add_model(reglineal)

workf_4<-workflow() %>% add_recipe(recp_4) %>% add_model(reglineal)

workf_5<-workflow() %>% add_recipe(recp_5) %>% add_model(reglineal)

workf_6<-workflow() %>% add_recipe(recp_6) %>% add_model(reglineal)

workf_7<-workflow() %>% add_recipe(recp_7) %>% add_model(reglineal)

workf_8<-workflow() %>% add_recipe(recp_8) %>% add_model(reglineal)

workf_9<-workflow() %>% add_recipe(recp_9) %>% add_model(reglineal)

workf_10<-workflow() %>% add_recipe(recp_10) %>% add_model(reglineal)

#Predicción de los modelos

#Creación de dataframe de RMSE de los modelos

df_RMSE <- data.frame(Modelo = character(),RMSE = numeric())

#Modelo 1

fit_1 <- workf_1 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_1 <- predict(fit_1, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_1 <- rmse(yhat_1, truth = log_wageh, estimate = .pred)

df_RMSE[1,1]<-"Modelo 1"
df_RMSE[1,2]<-round(RMSE_test_1$.estimate,3)

#Modelo 2

fit_2 <- workf_2 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_2 <- predict(fit_2, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_2 <- rmse(yhat_2, truth = log_wageh, estimate = .pred)

df_RMSE[2,1]<-"Modelo 2"
df_RMSE[2,2]<-round(RMSE_test_2$.estimate,3)

#Modelo 3

fit_3 <- workf_3 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_3 <- predict(fit_3, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_3 <- rmse(yhat_3, truth = log_wageh, estimate = .pred)

df_RMSE[3,1]<-"Modelo 3"
df_RMSE[3,2]<-round(RMSE_test_3$.estimate,3)

#Modelo 4

fit_4 <- workf_4 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_4 <- predict(fit_4, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_4 <- rmse(yhat_4, truth = log_wageh, estimate = .pred)

df_RMSE[4,1]<-"Modelo 4"
df_RMSE[4,2]<-round(RMSE_test_4$.estimate,3)

#Modelo 5

fit_5 <- workf_5 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_5 <- predict(fit_5, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_5 <- rmse(yhat_5, truth = log_wageh, estimate = .pred)

df_RMSE[5,1]<-"Modelo 5"
df_RMSE[5,2]<-round(RMSE_test_5$.estimate,3)

#Modelo 6

fit_6 <- workf_6 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_6 <- predict(fit_6, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_6 <- rmse(yhat_6, truth = log_wageh, estimate = .pred)

df_RMSE[6,1]<-"Modelo 6"
df_RMSE[6,2]<-round(RMSE_test_6$.estimate,3)

#Modelo 7

fit_7 <- workf_7 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_7 <- predict(fit_7, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_7 <- rmse(yhat_7, truth = log_wageh, estimate = .pred)

df_RMSE[7,1]<-"Modelo 7"
df_RMSE[7,2]<-round(RMSE_test_7$.estimate,3)

#Modelo 8

fit_8 <- workf_8 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_8 <- predict(fit_8, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_8 <- rmse(yhat_8, truth = log_wageh, estimate = .pred)

df_RMSE[8,1]<-"Modelo 8"
df_RMSE[8,2]<-round(RMSE_test_8$.estimate,3)

#Modelo 9

fit_9 <- workf_9 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_9 <- predict(fit_9, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_9 <- rmse(yhat_9, truth = log_wageh, estimate = .pred)

df_RMSE[9,1]<-"Modelo 9"
df_RMSE[9,2]<-round(RMSE_test_9$.estimate,3)

#Modelo 10

fit_10 <- workf_10 %>% fit(data=training_set) #Ajuste del modelo en el training set

yhat_10 <- predict(fit_10, new_data = test_set) %>% bind_cols(test_set) #Predecir y_hat en el test set y crear el dataframe con la predicción

RMSE_test_10 <- rmse(yhat_10, truth = log_wageh, estimate = .pred)

df_RMSE[10,1]<-"Modelo 10"
df_RMSE[10,2]<-round(RMSE_test_10$.estimate,3)

df_RMSE

#Exportar RMSE

write.csv(df_RMSE, "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/view/Tabla_Punto5_ValidationSet_RMSE.csv", row.names = F)

##Mejor modelo

mejor_test<-get(paste0("yhat_",toString(which(df_RMSE[[2]]==min(df_RMSE[[2]]))[1])))

save(mejor_test,file = "C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/Punto5b_test_set_mejor_modelo.Rda")

