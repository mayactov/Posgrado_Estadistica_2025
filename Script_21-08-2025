#Maya Tovar 
#21/08/2025
#Semana 3

#Importar datos

temperatura <- read.csv("C:/Repositorio/temperatura.csv")
View(temperatura)

head(temperatura) #Primeras 6 filas
dim(temperatura) #numero de filas y columnas
names(temperatura) #nombre de las columnas
str(temperatura) #estructura del data frame

summary(temperatura) #Resumen estadistico
names(temperatura) <- c("Anual","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
#corregir nombre de la columna

temperatura$media_anual <- rowMeans(temperatura[,2:13])
head(temperatura)
temp <- temperatura[,2:13]
temp10 <- temperatura[11:21, 2:13]
temperatura[2, 2]

colores <- c("skyblue", "skyblue", "navajowhite","salmon")

boxplot(temp, col= colores,
        main = "Comportamiento temperatura (2000 a 2020)",
        xlab = "Meses",
        ylab = "Temperatura (C)"
        )
