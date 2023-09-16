###Punto 5.b. - Gráficas y análisis

##Objetivo: Análisis del mejor modelo

##Cargue de paquetes

library(pacman)
p_load(tidyverse, tidymodels, glmnet, ggplot2)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra)

load("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/stores/Punto5b_test_set_mejor_modelo.Rda")

mejor_test<-mejor_test %>% mutate(error_mejor_test = .pred - log_wageh)

#Histograma del error

#ggplot(data = mejor_test, aes(x = error_mejor_test))+
#  geom_histogram()

png("C:/Users/afdia/OneDrive - Universidad de los Andes/Maestría en Economía Aplicada/Big Data y Machine Learning/Repositorios-GitHub/Taller_1/view/Punto5_Histograma_Error.png")
h_error<-hist(mejor_test$error_mejor_test,
              breaks = 200,
              col = "blue",
              main = "Histograma del error de predicción 
     Mejor modelo",
              xlab = "Error de predicción",
              ylab = "Frecuencia",
              labels = FALSE,
)
dev.off()

#Boxplot del error

ggplot(mejor_test, aes(y = error_mejor_test, x="")) +
  geom_boxplot(fill = "gray",
               coef = 3,
               outlier.colour = "red",
               outlier.size=1) + 
  labs(title = "Box Plot Error - Mejor Modelo", y = "Error de predicción", x="") + 
  theme_bw()

##Plot error vs log(w)

#Creación de rango para identificar outliers en log(w) observado
min_outlier <- mean(mejor_test$log_wageh)-3*sd(mejor_test$log_wageh)
max_outlier <-mean(mejor_test$log_wageh)+3*sd(mejor_test$log_wageh)

##Gráfica que identifica observaciones outliers y su valor de error (en rojo)
plot(mejor_test$log_wageh,mejor_test$error_mejor_test,
     xlab="Log(w) observado",
     ylab="Error de predicción",
     col = ifelse(mejor_test$log_wageh < min_outlier, "red","green"))
abline(v = min_outlier,col="gray",lwd=2,lty=2)
text(x=min_outlier-0.25, y=4.5, 'A')
abline(v = max_outlier,col="gray",lwd=2,lty=2)
text(x=max_outlier-0.25, y=4.5, 'B')

##Contribución de los outliers en el error

#Error al cuadrado para cada observación en el test set
mejor_test<-mejor_test %>% mutate(error_mejor_test2 = error_mejor_test^2)

#Porcentaje de cada observación en el error al cuadrado en el test set
mejor_test<-mejor_test %>% 
  mutate(porc_error_mejor_test2 = 
           error_mejor_test2/sum(error_mejor_test2))

#Identificar outliers en el test set
mejor_test<-mejor_test %>% 
  mutate(outlier = 
           ifelse(log_wageh<min_outlier | log_wageh>max_outlier,
                  "Outlier",
                  "No Outlier"))

#Calcular contribución de outlier
cont_outlier<-mejor_test %>% group_by(outlier) %>% 
  summarise(sum_contribución = round(sum(porc_error_mejor_test2),3),.groups='drop')