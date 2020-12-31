#Otro an?lisis fundamental para los equipos de Marketing es comprender la salud de marca,
#es decir, comprender la percepci?n de los usuarios sobre los atributos principales de marca
#que han sido definidos por el propio marketer y que suelen medirse en 4 olas anuales.
#Es importante comprender que en estas mediciones se realizan muchas preguntas a los usuarios
#y que muchas veces los dataset resultantes son bastante complejos y es necesario "reducir" la
#data a nuevas dimensiones que sigan expresando la misma informaci?n, pero de forma condensada

#En el caso de la medici?n de percepci?n de marca se suele emplear el PCA (Principal Component Analysis)
#como t?cnica para normalizar las variables ya que busca capturar la mayor variabilidad de data 
#a trav?s de la identificaci?n de 

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Customer Analytics/PercepcionMarca")

library(tidyverse) #transformar y visualizar la data
library(psych) #normalizar los datos al estilo de la Psicolog√≠a. Recomienda explorarlo porque los psic. usan mucha estad.
library(corrplot) #graficar correlaciones de forma amigable

#cargamos los datos, asegurando que se generan 10 columnas con los valores
Encuesta_Marcas <- read.csv("PercepcionMarca_datos.csv", stringsAsFactors = F) %>%
  select(-1) # La primera columna es el index de cada fila y no es relevante, la descarta.

head(Encuesta_Marcas)
summary(Encuesta_Marcas)
table(Encuesta_Marcas$Brand)

#graficamos la correlaci?n entre los datos para ir explorando la relaci?n entre diferentes adjetivos
corrplot(cor(Encuesta_Marcas[ , 1:9]), order="hclust")

#Por ultimo para terminar la inspecci?n, creamos una matrix con la media de puntuaci?n
(Encuesta_MarcasMedia <- aggregate(. ~ Brand, Encuesta_Marcas, mean))
row.names(Encuesta_MarcasMedia) <- Encuesta_MarcasMedia$Brand
Encuesta_MarcasMedia$Brand <- NULL

#vemos r?pidamente que caracter?sticas est?n asociadas con cada marca con un heatmap
heatmap(as.matrix(Encuesta_MarcasMedia)) 
# de esta forma comprendemos los principales atributos asociados a cada una,
# lo cual es un aspecto fundamental para los equipos de marketing


##### PRINCIPAL COMPONENT ANALYSIS #####
# Aplicando prcomp podemos analizar cu?ntos componentes son necesarios
#para explicar la variabilidad en la data de la encuesta de percepci?n
EncuestaMarcas_PCA <- prcomp(Encuesta_Marcas[ , 1:9])
summary(EncuestaMarcas_PCA) #al ser una lista, con summary podemos entender qu? hay dentro
#en este caso se identiican los 9 adjetivos como componentes relevantes

plot(EncuestaMarcas_PCA, type="l") #type l indica gr?fico de linea

biplot(EncuestaMarcas_PCA)
biplot(EncuestaMarcas_PCA, choices = c(1,9), cex = c(0.3, 0.6))
biplot(EncuestaMarcas_PCA, choices = 3:4, cex = c(0.3, 0.6))

# using the aggregate, mean PRST brand ratings, plot the brands against the first two components. How do you interpret that? Now plot against the second and third components 
#Qu?How does that change your interpretation? 
#What does this tell you about PCA in general?
EncuestaMarcas_PCAMean <- prcomp(Encuesta_MarcasMedia)
biplot(EncuestaMarcas_PCAMean, choices = c(1,2), cex = c(0.3, 0.6))

#* No sÈ cÛmo interpretarlo, entiendo que las marcas est·n situadas en los puntos en los que tienen 
#* sus valores de las dos primeras componentes, pero no sÈ porque no hay dos flechas para cada 
#* habilidad en vez de una (visto de esta manera no sÈ a cu·l de las 4 marcas corresponde el valor al)
#* que apunta cada flecha. Quiz· las marcas que estÈn m·s cerca de las flechas tengan mayores valores
#* de esos par·metros que las que se encuentran m·s lejos, en cuyo caso la marca 1 serÌa m·s delightful
#* y exciting que la marca 2.

biplot(EncuestaMarcas_PCAMean, choices = c(2,3), cex = c(0.3, 0.6))

#* Siguiendo con la hipÛtesis anterior, la marca dos es "delightful"
#* e intuitiva, pero no generosa ni "de ayuda", por ejemplo.

#Utiliza summary para ver cu?ntos componentes existen

summary(EncuestaMarcas_PCAMean) # Hay 4 componentes, esto es, tantos como marcas.

#Realiza un biplot de los diferentes componentes
#Qu? relaciones se establecen? Similares al heatmap

biplot(EncuestaMarcas_PCAMean)
heatmap(as.matrix(EncuestaMarcas_PCAMean)) # No puedo pintarlo porque no es una matriz numÈrica, pero
                                           # por lo que veo al llamarla deberÌa serlo (DUDA).

# No creo que se refiera a que haga un biplot para cada pareja de componentes y solo puedo pintar dos
# al mismo tiempo, asÌ que no se me ocurre otra cosa que volver a pintar el biplot de las dos primeras, 
# por ejemplo. Pero es que este ya lo he comentado y no sÈ quÈ relaciones se establecen, el heatmap es 
# m·s claro.

##### DATA NORMALIZADA PARA PCA #####

#Utilizamos la funcion scale para "normalizar" los datos a traves de la media y la sd 
#por las dudas primero duplicamos el data frame para no perder la data inicial
Encuesta_MarcasScale <- Encuesta_Marcas
Encuesta_MarcasScale[ , 1:9] <- (scale(Encuesta_Marcas[ , 1:9]))

#utilizamos describe de librer?a psych que es para psicologia, pero muy util
#vemos ahora que mean y kurtosis es 0, lo que ha dejado la data bastante uniforme
describe(Encuesta_MarcasScale) 
corrplot(cor(Encuesta_MarcasScale[ , 1:9]), order="hclust") #pero la correlacion se mantiene igual

# Nos sigue diciendo que los primeros cuatro son los m√°s importantes.

Encuesta_MarcasScale_PCA <- prcomp(Encuesta_MarcasScale[ , 1:9])
summary(Encuesta_MarcasScale_PCA) #al ser una lista, con summary podemos entender qu? hay dentro
#en este caso se identiican los 9 adjetivos como componentes relevantes

plot(Encuesta_MarcasScale_PCA, type="l") #los primeros 4 componentes son los m?s relevantes

biplot(Encuesta_MarcasScale_PCA, cex = c(0.3, 0.6))
biplot(Encuesta_MarcasScale_PCA, choices = c(1,9), cex = c(0.3, 0.6))
biplot(Encuesta_MarcasScale_PCA, choices = 3:4, cex = c(0.3, 0.6))


#?Que sucede con la data de medias? ?Que analisis podemos obtener de su procesamiento?
Encuesta_MarcasMedia_PCA <- prcomp(Encuesta_MarcasMedia[ , 1:9], scale. = T)
summary(Encuesta_MarcasMedia_PCA) 
# Al hacer la media, las variables m·s importantes pasan a ser las 4 marcas.
biplot(Encuesta_MarcasMedia_PCA, cex = c(0.9, 0.6))

#* El gr·fico se me hace muy parecido al anterior, aunque ahora los tÌtulos de las marcas aparecen
#* m·s grandes (no sÈ si eso quiere decir algo) y la marca dos est· pr·cticamente centrada en el 
#* gr·fico.

#Grafiquemos de forma arm?nica los resultados de PCA
install.packages("factoextra")
library("factoextra")
get_pca_var(EncuestaMarcas_PCA) #me permite identificar el nombre de las variables

fviz_pca_var(EncuestaMarcas_PCA, col.var = "blue")

fviz_pca_var(EncuestaMarcas_PCA, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # evita overlaping del texto
             )


fviz_pca_var(Encuesta_MarcasMedia_PCA, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


fviz_pca_ind(Encuesta_MarcasMedia_PCA, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


