#Maya Tovar
#Semana 8 
#25/09/2025

datos <- data.frame(
  trigo = c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40), 
  harina = c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
)

sumxi_yi <- sum(datos$trigo*datos$harina)
nxy <- length(datos$trigo) *mean(datos$trigo)*mean(datos$harina)
nxy
sumxi_yi-nxy

xi2 <- sum(datos$trigo^2)
nx2 <- length(datos$trigo)*mean(datos$trigo)^2

xi2 <- sum(datos$trigo^2)
xi2
nx2 <- length(datos$trigo)*mean(datos$trigo)^2
b1 <- (sumxi_yi - nxy)/(xi2-nx2)
b1

bo <- mean(datos$harina*datos$trigo)
bo

#en el modelo lineal va la variable dependiente

fit.lm <- lm(datos$harina ~ datos$trigo)
fit.lm$model
fit.lm$coefficients
fit.lm$residuals
mean(fit.lm$residuals)
datos$yprima <- 74.11-1.3536 * datos$trigo
datos$recta <- fit.lm$fitted.values

datos$prima <- bo+b1*datos$trigo
datos$residuales <- datos$harina-datos$recta
SSE <- sum(datos$residuales^2)
SSE/8
sqrt(SSE/8)


anova(fit.lm)

#instalar paquete lmtest
library(lmtest)

#modelo
m <- lm(datos$harina ~ datos$trigo)

#Breusch-Pagan 
bptest(m)

#
