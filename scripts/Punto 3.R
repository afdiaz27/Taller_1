################################################
##Punto N3: Estimación del Perfil edad-salario##
##Cargue de paquetes
library (pacman)
p_load(tidyverse, skimr, stargazer, tidymodels, broom,knitr,kableExtra, ggplot2)

##Cargue de base de datos##
setwd("C:/users/de.sandoval10/Documents/GitHub/Taller_1/stores")
load("database_18_clean.Rda")

##Modelo linel##
#Seleccion de variable X
salario.por= c('age', 'age2')
X <-  df_clean %>% select(salario.por)

#Variable Y
y <- df_clean %>% 
  select(log_wageh) %>%
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
# Grafica el estimado de la función

ggplot(df_clean_p) +
  geom_point(aes(x=age,y=y_hat)) +
  ggtitle("Perfil edad-ingresos") +
  labs(x="Edad (años)", y="Predicción ln ingresos/hora")

##Estimacion Invertalos de Confianza con Bootstrap##
#Carga de paquete para bootstrap#


# FWL with Bootstrap - Conditional Wage Gap
p_load(boot)

set.seed(1000)
beta_fn <- function(formula,data,indices){
  d <- data[indices,]
  fit <- lm(formula,data=d)
  return(coef(fit))
}
reps <- boot(data=dat,statistic=beta_fn,R=1000,formula=y~.)
reps
str(reps)

set.seed(1000)
R <- 1000
reg_age <- rep(0,R)
for(i in 1:R) {
  sample <- sample_frac(dat,size=1,replace=TRUE)
  f <- lm(y~.,sample)
  coefs <- f$coefficients
  b1 <- coefs[2]
  b2 <- coefs[3]
  reg_age[i] <- b1/(-2*b2)
}

hist(reg_age,xlab="Edad",ylab="Porcentaje",main="Edad en la que se alcanza el pico de máximo salario",col="green")
abline(v=mean(reg_age),col="darkblue",lwd=3)
summary(reg_age)
mean(reg_age)

# Intervalo de confianza
ICb0 <- 1.96*6.009782e-02
ICb1 <- 1.96*3.220967e-03
ICb2 <- 1.96*3.962613e-05

ICb0
ICb1
ICb2

