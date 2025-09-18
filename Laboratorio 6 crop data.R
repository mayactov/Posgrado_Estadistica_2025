#Maya Tovar 
#Semana 7.0

crop.data <- read.csv("C:/Repositorio/Posgrado_Estadistica_2025/crop.data.csv")
View(crop.data)

crop.data <- read.csv(url, header = T)
crop.data$density <- as.factor(crop.data$density)
crop.data$block <- as.factor(crop.data$block)
crop.data$fertilizer <- as.factor(crop.data$fertilizer)
summary(crop.data)

colores <- c("indianred","skyblue","navajowhite")
boxplot(crop.data$yield ~ crop.data$fertilizer,
        col= colores,
        xlab = "Fertilizantes",
        ylab = "Rendimiento (ton/ha)")

tapply(crop.data$yield, crop.data$fertilizer, mean)
tapply(crop.data$yield, crop.data$fertilizer, var)

#aplicar prueba de normalidad de datos 
#fertilizante combinando subset y shapiro 

shapiro.test(subset(crop.data$yield, crop.data$fertilizer == "1"))
shapiro.test(subset(crop.data$yield, crop.data$fertilizer == "2"))
shapiro.test(subset(crop.data$yield, crop.data$fertilizer == "3"))

#Prueba de Bartlett
bartlett.test(crop.data$yield ~ crop.data$fertilizer)

#Prueba de ANOVA
crop.data.aov <- aov(crop.data$yield ~ crop.data$fertilizer * crop.data$block)
summary(crop.data.aov)

#LSD determinar el valor
qt(0.975, 93)
sqrt((2*0.3859)/32) * qt(0.975, 93)
tapply(crop.data$yield, crop.data$fertilizer, mean)
#primer diferencia de medias F1 vs F2
176.757 - 176.9332

#primer diferencia de medias F2 vs F3
176.9332 - 177.3562

#primer diferencia de medias F3 vs F1
176.757 - 177.3562

#prueba de Tukey
sqrt((2*0.3859)/32) * qtukey(0.95, nmeans = 3, df = 93) 

TukeyHSD(crop.data.aov)
plot(TukeyHSD(crop.data.aov))
