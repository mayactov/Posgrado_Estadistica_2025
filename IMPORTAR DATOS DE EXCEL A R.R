temperatura <- read.csv("C:/Repositorio/temperatura.csv")
View(temperatura)

head(temperatura) #Primeras 6 filas
dim(temperatura) #numero de filas y columnas
names(temperatura) #nombre de las columnas
str(temperatura) #estructura del data frame

summary(temperatura) #Resumen estadistico
names(temperatura) <- c("Anual","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
#corregir nombre de la columna

temperatura$media_anual <- rowMeans(temperatura[,2:13]) #seleccionar para visualizar lo escrito a partir de temperatura y donde termina el corchete
