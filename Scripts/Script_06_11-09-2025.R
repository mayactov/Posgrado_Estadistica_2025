#Maya Tovar
#Semana 6
#11/09/2025

data("iris")
boxplot(iris$Sepal.Length ~ iris$Species)

dat_iris <- subset(iris, Species %in% c("versicolor",
                                        "virginica"))
#Para eliminar un factor que se desecho de la base de datos original
dat_iris$Species <- droplevels(dat_iris$Species)
boxplot(dat_iris$Sepal.Length ~ dat_iris$Species)

#H0 = la media de sepal length de virginica es mayor que la media de versicolor

t.test(dat_iris$Sepal.Length ~ dat_iris$Species, alternative = "greater",
       var.equal = T)
tapply(dat_iris$Sepal.Length,dat_iris$Species, mean)
