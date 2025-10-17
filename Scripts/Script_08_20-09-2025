#Maya Tovar
#Semana 7
#20-09-2025

#Ejercicio:Comparación de concentraciones de estroncio en cuerpos de agua

# Datos originales
estroncio <- data.frame(
  Muestra = 1:6,
  Graysons_Pond = c(28.2, 33.2, 36.4, 34.6, 29.1, 31),
  Beaver_Lake = c(39.6, 40.8, 37.9, 37.1, 43.6, 42.4),
  Anglers_Cove = c(46.3, 42.1, 43.5, 48.8, 43.7, 40.1),
  Appletree_Lake = c(41.0, 44.1, 46.4, 40.2, 38.6, 36.3),
  Rock_River = c(56.3, 54.1, 59.4, 62.7, 60.0, 57.3)
)

#H0:Las medias de concentración de estroncio son iguales en todos los cuerpos de
#agua.
#H1: Al menos un cuerpo de agua tiene una media diferente.


# Transformar datos 
library(reshape2) 
estroncio_concentracion <- melt(estroncio, id.vars = "Muestra",
                                variable.name = "Sitio", value.name = "estroncio")
estroncio_concentracion$Sitio <- as.factor(estroncio_concentracion$Sitio)

# Resumen de datos
summary(estroncio_concentracion)

# Boxplot básico
colores <- c("indianred", "skyblue", "navajowhite", "lightgreen", "plum")
boxplot(estroncio ~ Sitio, data = estroncio_concentracion,
        col = colores,
        xlab = "Sitios",
        ylab = "Concentración de Estroncio",
        main = "Boxplot de Estroncio por Sitio")

# Promedios y varianzas por sitio
tapply(estroncio_concentracion$estroncio, estroncio_concentracion$Sitio, mean)
tapply(estroncio_concentracion$estroncio, estroncio_concentracion$Sitio, var)

# ANOVA
estroncio_aov <- aov(estroncio ~ Sitio, data = estroncio_concentracion)
summary(estroncio_aov)

#Se rechaza H0 
library(agricolae) 

#LSD 
lsd_resultados <- LSD.test(estroncio_aov, "Sitio", p.adj = "none") 
print(lsd_resultados) 

#Prueba Tukey
tukey_result <- TukeyHSD(estroncio_aov,  conf.level = 0.95) 
print(tukey_result) 
plot(tukey_result)

n <- 6 
glerror <- estroncio_aov$df.residual 
MSE <- summary(estroncio_aov)[[1]][["Mean Sq"]][2] 
k <- length(levels(estroncio_concentracion$Sitio)) 
qcrit <- qtukey(0.95, nmeans = k, df = glerror) 
MSD <- qcrit * sqrt(MSE/2 * (2/n)) 
MSD

# Gráfico con ggplot2 (violín + puntos)
library(ggplot2)
ggplot(estroncio_concentracion, aes(x = Sitio, y = estroncio, fill = Sitio)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, size = 1, color = "black") +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  theme_light() +
  labs(title = "Concentracion de Estroncio por Sitio",
       x = "Sitio",
       y = "Estroncio (mg/ml)") +
  scale_fill_manual(values = colores)

#Discusión 

#¿Qué cuerpo de agua presenta las concentraciones más altas?
#El sitio con mayores concentraciones de estroncio es Rock River, con un promedio
#de 58.3 mg/ml, encima de los demás cuerpos de agua.

#¿Qué sitios no difieren entre sí?
#Beaver Lake, Appletree Lake y Angler’s Cove forman un grupo sin diferencias 
#estadísticas significativas entre sí.
#Graysons Pond se mantiene como el sitio con concentraciones más bajas.
#Rock River destaca de manera aislada con valores mucho más altos.

#Desde el punto de vista ambiental, ¿qué implicaciones podrían tener estas 
#diferencias en la calidad del agua?

#Rock River Si bien el estroncio estable no presenta una toxicidad elevada, 
#esto puede ser señal de vulnerabilidad del sistema acuático a otros 
#contaminantes. Además, su acumulación puede constituir un indicador de riesgo
#potencial para la potabilidad del agua y para el equilibrio ecológico de los 
#ecosistemas acuáticos.(Malov & Laverov, 2023; Musgrove et al., 2020).
#En los sitios Beaver Lake, Angler’s Cove y Appletree Lake presentan valores 
#intermedios y similares, lo que sugiere una condición relativamente más
#estable y menos crítica.
#Graysons Pond, el de menor concentración, podría considerarse como referencia 
#de un nivel menos alterado. (Musgrove et al., 2020; McPherson et al., 2014).

#Referencias biliográficas
#Malov, A., & Laverov, N. (2023). Features of the Formation of Strontium 
#Pollution of Drinking Groundwater and Associated Health Risks in the North-West 
#of Russia. Water, 15(21), 3846. https://doi.org/10.3390/w15213846

#McPherson, C. A., Chapman, P. M., & McDonald, B. G. (2014). Development of a 
#strontium chronic effects benchmark for aquatic life in freshwater. 
#Environmental Toxicology and Chemistry, 33(11), 2472–2478. 
#https://doi.org/10.1002/etc.2696

#Musgrove, M. L., et al. (2020). The occurrence and distribution of strontium in 
#U.S. groundwater. Applied Geochemistry, 121, 104867.
#https://doi.org/10.1016/j.apgeochem.2020.104867

