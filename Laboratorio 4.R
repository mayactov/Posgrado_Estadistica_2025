#Maya Tovar
#28/08/2025
#Semana 4

#Importar -------------------------------------------------------------------------------------------


calidad <- read.csv("C:/Repositorio/Posgrado_Estadistica_2025/calidad.csv")
View(calidad)
calidad$Tratamiento <- as.factor(calidad$tratamiento)
class(calidad$tratamiento)
summary(calidad)

mean(calidad$IE)

tapply(calidad$IE, calidad$Tratamiento, mean)
tapply(calidad$IE, calidad$Tratamiento,sd)
tapply(calidad$IE, calidad$Tratamiento, var)

colores <- c("navajowhite","skyblue")

boxplot(calidad$IE, calidad$Tratamiento)
boxplot(calidad$IE~ calidad$Tratamiento)

boxplot(calidad$IE~calidad$Tratamiento , col = c("navajowhite","skyblue"), main= "Vivero Forestal", xlab = "Tratamientos", ylab = "indice de calidad", ylim= c(0.4, 1.2))


#Aplicar un subconjunto para cada tratamiento

df_ctrl <- subset(calidad, Tratamiento == "Ctrl")
df_fert <- subset(calidad, Tratamiento == "Fert")

par(mfrow=c(1,2)) #para mostrar ambas graficas
qqnorm(df_ctrl$IE); qqline(df_ctrl$IE)
qqnorm(df_fert$IE); qqline(df_fert$IE)
par(mfrow=c(1,1)) #para cerrar la grafica

#Normalidad de los datos
shapiro.test(df_ctrl$IE)
shapiro.test(df_fert$IE)

#Revisa homogeneidad
var.test(calidad$IE ~ calidad$Tratamiento)

#codigo completo
t.test(calidad$IE ~ calidad$Tratamiento, alternative = "two.sided", var.equal = T)
t.test(calidad$IE ~ calidad$Tratamiento, alternative = "two.sided", var.equal = FALSE)
t.test(calidad$IE ~ calidad$Tratamiento, alternative = "greater", var.equal = T)

#Medir el efecto de tratamiento

cohens_efecto <- function(x, y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x);  s2 <- sd(y)
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  (mean (x) - mean(y)) / sp 
}

d_cal <- cohens_efecto(df_ctrl$IE, df_fert$IE)
