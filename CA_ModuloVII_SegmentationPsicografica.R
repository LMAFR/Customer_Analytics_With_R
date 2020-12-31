#La idea de este ejercicio es practicar con diferentes algoritmos y funciones que nos permiten
#trabajar con data categórica sin tener que transformarlos a variables numéricas como en el caso
#de Text Analytics

#Para ello primero es necesario instalar y cargar 2 librerías claves: cluster y Rtsne

install.packages("cluster")
install.packages("Rtsne")

library(tidyverse)
library(cluster) #utilizar la distancia de Gower
library(Rtsne) #realizar una visualización de los cluster

#1 Utilizaremos un dataset público que contiene datos ordinales y categóricos sobre
#clientes de bancos. Fuente: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

clientes_banca <- read_csv2("bank-full.csv", 
                            col_types = list("d","f","f","f","f","d","f","f","f","d",
                                             "f","d","d","d","d","f","f")) 
#usamos read_csv2 porque los datos estan separados por ";" y definimos los col_types para que 
#traiga las columnas como factores y no como texto plano


#inspeccionamos el dataset para entender sus variables
str(clientes_banca) #vemos que el dataset contiene 10 columnas de datos texto en formato factor
summary(clientes_banca) 
head(clientes_banca)

#Vamos a hacer un subset de datos para trabajar con los solteros y asi tener un dataset
#un poco más liviano para que no tarde tanto en calcular la distancia Gower
clientes_banca_single <- clientes_banca %>%
  filter(marital == "single")


#Vamos directo al asunto y calculamos la distancia Gower (porque tenemos datos categ�ricos)
banca_gower <- daisy(clientes_banca_single, metric = "gower")

#inspeccionamos el resultado
summary(banca_gower)


#lo convertimos a matrix para poder visualizar en más detalle los resultados y realizar operaciones
banca_gower_matrix <- as.matrix(banca_gower)


#Visualizamos aquellos clientes solteros que tienen mayor similitud entre si
#Para ello utilizamos los datos de la matrix
clientes_banca_single[which(banca_gower_matrix == min(banca_gower_matrix[banca_gower_matrix != min(banca_gower_matrix)]), arr.ind = TRUE)[1, ], ]
#vemos que son usuarios que tienen casi los mismos valores en factores excepto diferencias pequeñas
#a nivel de balance y duration


#Tambien podemos hacerlo con los clientes que menos se parecen entre si
clientes_banca_single[which(banca_gower_matrix == max(banca_gower_matrix[banca_gower_matrix != max(banca_gower_matrix)]), arr.ind = TRUE)[1, ], ]
#aqui ya vemos diferencias no solo en lo numerico sino tambien a nivel de los datos factoriales

#La matrix nos sirve para inspeccionar y hacer un check visual de que la distancia fue calculada
#correctamente, pero para la clusterizacion utilizamos el dataser origial de banca_gower

#2 
#Ahora pasamos a segmentar los usuarios, para ello definiremos cantidad de cluster aplicando
#un pequeño for loop a los datos de la matrix con la función pam
#TIP: si ven que la sesion explota o tarda demasiado en realizar los calculos, pueden seleccionar
#una muestra del total de las 12k observaciones para poder trabajar
n_clusters <- c(NA)

#voy a generar un for loop que tenga en cuenta un rango de entre 2 y 8 clusters y señale
#cuales son aquellos clusters que mantienen similitudes entre sus datos
for(i in 2:8){  
  pam_test <- pam(banca_gower_matrix, diss = TRUE, k = i)  #pam: partition around medoid.
  n_clusters[i] <- pam_test$silinfo$avg.width  
}
plot(1:8, n_clusters)
lines(1:8, n_clusters) #me indica que 3 sería lo ideal, también podría probar con 5


#3 Aplicamos k-medoids con PAM para poder clusterizar los datos con un k de 3
pam_banca_solteros <- pam(banca_gower, diss = T, k = 3) #diss T para que identifique como "matrix dissimilarity"

pam_banca_solteros_r <- clientes_banca_single %>%
  mutate(cluster = pam_banca_solteros$clustering) %>%
  group_by(cluster) %>%
  do(resumen = summary(.)) #acuerdense que el . significa que quiero seleccionar todo

pam_banca_solteros_r$resumen
#en el resumen vemos que se ha dado un gran peso de clusterizacion al tipo de trabajo
#dentro de todas las variales categoricas que le hemos dado de ingesta



#4
#Por ultimo visualizamos. Para aquellos que estuvieron intentando visualizar los segmentos de k-means
#con scatter, esta librería tiene la función Rtsne que nos permite graficar aquellos datos
#que estan cercanos entre si y poder difenciar claramente los clusters a nivel visual

#Para ello primero debemos generar el objeto con los datos correspondientes
#Aplicamos la función de Rtsne para que nos permite reducir el volumen de dimensiones de 
#forma similar a cuando vimos PCA en percepción de marca

tsne_banca <- Rtsne(banca_gower, is_distance = TRUE)
tsne_graf <- tsne_banca$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_banca_solteros$clustering))

#ahora si hacemos un scatter, 
ggplot(aes(x = X, y = Y), data = tsne_graf) +
  geom_point(aes(color = cluster))




