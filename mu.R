#setosa

setosa <- subset(iris, Species == "setosa") #run y abre la tabla
mean(setosa$Sepal.Width)
#mu = 3.9
#H0= la diferencia entre las medias es cero o menor a cero = 3.9
#H1= la diferencia entre media teoretica y experimental es diferente a cero= diferente a 3.9 

t.test(setosa$Sepal.Width, mu = 3.9)

