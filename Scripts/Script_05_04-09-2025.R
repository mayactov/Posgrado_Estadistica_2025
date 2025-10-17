#Maya Tovar
#Semana 5
#04/09/2025

#Media movil para determinar el n ideal con respecto a la poblacion

data ("iris")
head(iris)

setosa <- subset(iris, Species == "setosa")

#Sumatoria acumulativa de la variable longitud
acum <- cumsum(setosa$Sepal.Length)
acum # sumatoria acumulada 
cont <- seq(1: length(setosa$Sepal.Length))
cont

#Crear un data frame con los datos creados

set_mov <- data.frame(cont, acum)
set_mov$movil <- round(acum/cont,2)

plot(cont, set_mov$movil, type = "l", 
     xlab = "Número de observaciones",
     ylab = "Media móvil")
abline(h=mean(setosa$Sepal.Length),
       col = "indianred")
