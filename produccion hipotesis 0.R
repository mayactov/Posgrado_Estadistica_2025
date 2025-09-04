#Maya Tovar
#Semana 4
#04/09/2025

#Prueba de T muestras dependientes

#Revisar la produccion del kg en semilla para el año 2013 y 2014

produccion <- read.csv("C:/Repositorio/produccion.csv", header= T)
produccion$Tiempo <- as.factor(produccion$Tiempo)

boxplot(produccion$Kgsem ~ produccion$Tiempo)
tapply(produccion$Kgsem, produccion$Tiempo, mean)

#H0= la media de produccion en el 2012 es menor a la del 2013

t.test(produccion$Kgsem ~ produccion$Tiempo, alternative = "greater",
       var.equal= T)
tapply(produccion$Kgsem,produccion$Tiempo, mean)

#H0= las medias de ambos años son igual a cero
#H1= las medias de ambos años son diferentes a cero
#Metodo alternativo two.sided
var.test(produccion$Kgsem ~ produccion$Tiempo )
t2012 <-subset(produccion$Kgsem,produccion$Tiempo =="T2012") 
t2013 <-subset(produccion$Kgsem,produccion$Tiempo =="T2013") 

t.test(t2012, t2013,
       alternative = "two.sided", 
       var.equal = T,
       paired =T)

t2012 <-subset(produccion$Kgsem,produccion$Tiempo =="T2012") 
t2013 <-subset(produccion$Kgsem,produccion$Tiempo =="T2013") 

#H0 la media del 2012 es menor que 2013
t.test(t2012, t2013,
       alternative = "less", 
       var.equal = T,
       paired =T)

