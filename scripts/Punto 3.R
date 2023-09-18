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

##Estimacion Invertalos de Confianza con Bootstrap##
#Carga de paquete para bootstrap#
p_load(boot)

id<-c(1:length(df_clean[[1]]))
df_clean<-cbind(id, df_clean)

set.seed(1000)
beta_fn <- function(formula,data,id){
  d <- data[id,]
  fit <- lm(formula,data=d)
  return(coef(fit))
}
reps <- boot(data=df_clean,statistic=beta_fn,R=1000,formula=y ~.)
reps
str(reps)

set.seed(1000)
Rep <- 1000
reg_age <- rep(0,Rep)
for(i in 1:Rep) {
  sample <- sample_frac(dat,size=1,replace=TRUE)
  f <- lm(y~.,sample)
  coefs <- f$coefficients
  b1 <- coefs[2]
  b2 <- coefs[3]
  reg_age[i] <- b1/(-2*b2)
}
hist(reg_age,xlab="Edad",ylab="Porcentaje",main="Edad pico del salario por hora",col="black")
abline(v=mean(reg_age),col="white",lwd=3)
summary(reg_age)
mean(reg_age)

prueba <- 2+2
print (prueba)