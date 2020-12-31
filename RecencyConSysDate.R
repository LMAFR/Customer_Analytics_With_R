# Autor: Alejandro Flordo Reyes.
# Calculo de Recency con la fecha del día en que se ejecuta el script menos las fechas del dataset.

#Practica Matricas de Satisfaccion del Consumidor - NPS
#NPS - Net Promoter Score - Score promedio de recomendacion
#En R existe una libreria llamada NPS para calcularlo automaticamente
#Deberán trabajar el calculo NPS sobre un dataset y analizar correlaciones con cluster de RFM


#PASO 1: Deben cargar las librerias tidyverse, lubridate, caret, corrplot y NPS

library(tidyverse)
library(lubridate)
library(caret)
library(corrplot)
library(NPS)

#PASO 2: Generen un dataset llamado ventas2020 cargando el archivo "NPS_T1.csv" eliminando primera columna
#Eliminen aquellos registros que figuren con un NPS NA utilizando filter
#Modifiquen la columna nps a columna numerica utilizando mutate y as.numeric
#Ayuda: al utilizar select si escriben select(-1) entonces se seleccionan todas las columnas excepto la primera

# setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Customer Analytics/ModuloIV_PracticaNPS")

# Primero guardamos el dataframe con los datos de la tabla, salvo la primera columna. Para ello,
# previamente he ojeado el archivo .csv con excel.

ventas2020 <- read.csv("NPS_T1.csv", header = T, sep =',') %>% 
  select(-1)
head(ventas2020) # Comprobamos que nos la ha eliminado.

#* Vemos lo que nos está mostrando y comprobamos que es lo que queremos (1 columna 
#* de fechas, 1 de caracteres y 3 de integers).
#* 
#* Comprobamos que los tipos asignados a cada columna son correctos. 

str(ventas2020)   

# A la columna de fechas le ha dado tipo character, vamos a arreglarlo por si nos pidieran 
# trabajar con ella.

ventas2020$fecha_compra <- as.Date(ventas2020$fecha_compra) # Asignamos el tipo Date a la columna de fechas.
str(ventas2020) # Ahora parece que todo está correcto (los tipos de columna son los que cabría esperar).

ventas2020 <- ventas2020 %>% 
  filter(!is.na(nps)) %>% 
  mutate(nps = as.numeric(nps))
View(ventas2020)
str(ventas2020)

#Calculen el NPS agrupado por cada tienda, utilizando la función nps del paquete NPS dentro de la funcion summarise
#¿cual es la tienda con mejor performance?

nps_tiendas <- ventas2020 %>%
  group_by(store) %>%
  summarise(NPS = nps(nps))
nps_tiendas # Resultados:

# store      NPS
# <chr>    <dbl>
# 1 tienda_1 0.143
# 2 tienda_2 0.135
# 3 tienda_3 0.182
# 4 tienda_4 0.192
# 5 tienda_5 0.183

#* Dado que 1 sería el mejor caso y -1 el peor, la tienda con mejor performance es la 4.

#Realizaremos un analisis entre los meses del año y el NPS para cada tienda
#para ello deben crear una columna mes en el dataframe de ventas2020 
#y agrupar tanto por mes como por store y calcular el nps en un nuevo data frame

ventas2020$mes <- month(ventas2020$fecha_compra) # También podría haberla creado con mutate.
View(ventas2020)

nps_mes_tienda <- ventas2020 %>%
  group_by(store,mes) %>% 
  summarise(NPS = nps(nps))
nps_mes_tienda # Solo ha llegado hasta el mes 7 (Julio) así que vamos a comprobar si no
# hay más meses incluidos en las fechas de las tablas.

max(ventas2020$mes) # En efecto, los datos solo contienen fechas hasta Julio.

#visualizamos la comparación de NPS de cada tienda para cada mes
#utilicen gráfico de scatter (geom_point) y den color a los puntos con
#columna store

ggplot(nps_mes_tienda) +
  geom_point(aes(x = mes, y = NPS, colour = store)) +
  labs(title = 'NPS de cada tienda en cada mes', x = 'Mes', y = 'NPS') +
  facet_wrap(~store, ncol = 5) +
  theme_bw() # Esto es lo que se nos pide con un poco de decoración (título, tema, etc.) y dividido por ventanas, que
# es algo que se me ha recomendado en el feedback de la práctica de CRM.

# Yo creo que quedaría más claro de esta otra forma: (esto lo escribí antes del feedback mencionado, pero creo que merece la pena dejarlo)

ggplot(nps_mes_tienda) +
  geom_point(aes(x = mes, y = NPS, colour = store)) +
  geom_line(aes(x = mes, y = NPS, colour = store)) +
  labs(title = 'NPS de cada tienda en cada mes', x = 'Mes', y = 'NPS') +
  facet_wrap(~store, ncol = 5) +
  theme_bw() # Yo creo que esto es algo más claro, ya que los colores no están todos en la misma gráfica y las líneas ayudan
# a tener una idea de la variación del NPS que ha tenido cada tienda, haciendo más clara la comparación entre 
# tiendas para cada mes. Yo hubiera añadido estas líneas al gráfico anterior también, solo por mostrar más clara-
# mente cada conjunto de valores de NPS (creo que el color no ayuda mucho en este caso), pero la gráfica anterior
# he preferido presentarla simplemente como se pedía para que supieran que sabía dibujarla.

#* Podemos observar que en el último mes la tienda 3 es la que ha obtenido mejores resultados, no la 3 (que era la que
#* tenía mejor NPS de las tiendas). Por otro lado las otras dos con mayor NPS son la 4 y la 5 y vemos que, en general, 
#* obtienen valores más altos que los de las tiendas 1 y 2, tanto en general (como era de esperar) como en ese último mes.


#Desarrollar el cálculo de RFM para cada comprador en otro dataframe 
#sin olvidar de modificar la columna de  fecha para que R la reconozca como tal utilizando as.Date
#Generen 5 clusters a traves de kmean para identificar segmentos de consumidores
#pueden utilizar de referencia el script visto en el modulo II

#* Ya he convertido la columna de fechas al tipo fecha antes (línea 41), así que vamos directos al cálculo 
#* del RFM:

calculo_RFM <- ventas2020 %>% 
  group_by(id_member) %>% 
  summarise(Recency = as.numeric(as.Date(Sys.Date())-max(fecha_compra)),
            Frequency=length(id_member),
            Monetary_value=sum(gasto))
head(calculo_RFM) # Visualicemos el resultado.

#* Ya tenemos el cálculo del RFM por comprador, ahora vamos a generar los 5 clusters para 
#* identificar segmentos de consumidores.

set.seed(4321) #* Plantamos una semilla para que el resultado obtenido sea reproducible (esto es,
               #* que cuando los profesores corrijáis esto os salga lo mismo que me salió a mí,
               #* por poner un ejemplo relevante).

segmentacionRFM <- kmeans(scale(calculo_RFM[,2:4]), 5, nstart = 1) # Creamos un vector con 5
# categorías o clusters y un número de elementos igual al número de filas del dataframe a partir
# del cual se ha generado.

str(segmentacionRFM)

calculo_RFM$ScoreRFM <- as.factor(segmentacionRFM$cluster) # Almacenamos el nuevo vector 
                                                           # generado dentro del dataframe y en forma de factor.

head(calculo_RFM) 

# Para que quede más bonito de cara a equipos de marketing y perfiles no
# técnicos en general, podemos cambiar los números de los clusters por 
# palabras.

calculo_RFM$Tipo_de_cliente <- NA
calculo_RFM$Tipo_de_cliente[calculo_RFM$ScoreRFM == 1] <- "inactivo"
calculo_RFM$Tipo_de_cliente[calculo_RFM$ScoreRFM == 2] <- "en riesgo"
calculo_RFM$Tipo_de_cliente[calculo_RFM$ScoreRFM == 3] <- "potenciales"
calculo_RFM$Tipo_de_cliente[calculo_RFM$ScoreRFM == 4] <- "leales"
calculo_RFM$Tipo_de_cliente[calculo_RFM$ScoreRFM == 5] <- "champions"

head(calculo_RFM)

#Calcular nps agrupando por segmento de consumidores en un nuevo data frame

#* En primer lugar, unimos el dataFrame calculo_RFM al original (ventas2020), de tal forma que
#* asignemos a cada miembro en dicho dataframe su correspondiente valor de clúster:
 
ventas2020 = ventas2020 %>%
  left_join(calculo_RFM, by = c("id_member" = "id_member")) # Sé que el by no es necesario en este caso, pero es para que se vea
                                                            # que sé usar la función y que sé lo que estoy haciendo.

View(ventas2020) # Comprobamos que se ha asignado cada Tipo de cliente del cluster a su correspondiente id_member 
                  # observando su valor en el dataframe para los 3 primeros id_members.

# Calculamos el nps:

nps_clusters = ventas2020 %>% 
  group_by(Tipo_de_cliente) %>% 
  summarise(NPS = nps(nps))

nps_clusters # Resultados:

# champions       0.156
# en riesgo       0.171
# inactivo        0.178
# leales          0.182
# potenciales     0.150

#* Observamos que los leales son los que tienen mayor NPS, seguidos de cerca por los inactivos.

#Ahora realicen una correlacion entre NPS y  los segmentos de consumidores de RFM
#Existen mayor correlacion con aquellos consumidores que gastan mas dinero o menos dinero?
lcorr = lm(nps_clusters$NPS ~ c(1:5)) # Como la variable categórica va del 1 al 5, la he representado con 
# un vector que contenga esos elementos para evitar problemas, ya que
# ambas variables deben ser cuantitativas para el análisis.
lcorr

a = lcorr$coefficients[2]
b = lcorr$coefficients[1]

# Pendiente: 0.0000505
# Ord. en el origen: 0.167

# Vemos que la pendiente es bastante pequeña con respecto a la ordenada en el origen.
# Por si acaso, hagámosle un test de correlación lineal:

cor.test(nps_clusters$NPS,c(1:5)) # Vemos que p_value = 0.9927 > 0.05, luego no hay correlación lineal (hipótesis nula).

# Sin embargo, en los resultados que habíamos obtenido, quitando los champions, parecía que el NPS aumentaba conforme
# los consumidores tenían un RFM mayor. Solo por si acaso, dibujemos los puntos y la recta de ajuste:

# plot(c(1:5),nps_clusters$NPS)
# abline(lm(nps_clusters$NPS ~ c(1:5))) # Esto sería un plot cutre.

ggplot(nps_clusters) +
  geom_point(aes(x=c(1:5), y = NPS)) +
  stat_function(fun=function(x) a*x+b) +
  xlim(0,5) +
  labs(title = 'NPS para cada Tipo de cliente', x = 'Tipo de cliente (nº)', y = 'NPS') +
  theme_bw()

# Recordemos que:

# 1 <- "inactivo"
# 2 <- "en riesgo"
# 3 <- "potenciales"
# 4 <- "leales"
# 5 <- "champions"

# Podemos ver que la recta no se corresponde con los puntos, ya que para los champions tenemos un valor
# anómalo (no se adapta para nada a la progresión que vemos para el resto de tipos de clientes, que si
# parece más lineal). Por lo tanto, hagamos un pequeño cálculo sin tener en cuenta los champions:

cor.test(nps_clusters$NPS[1:4],c(1:4)) # Ahora p_value = 0.035 < 0.05; por lo tanto, sí que hay correlación lineal.
# También podemos observar un coeficiente de correlación lineal bastante elevado 
# (de 0.96). 

# En consecuencia, concluimos que hay mayor correlación entre los consumidores con una menor RFM_Score que, a priori, son
# los que gastan menos, ya que se entiende que tienen valores de frecuencia y valor monetario (que están directamente ligados 
# al gasto, ya sea por hacer muchos pedidos o por la cantidad total gastada en las tiendas) más bajos que los de score 
# elevada. Estos últimos presentan una correlación más rara, ya que de potenciales a leales parece continuar la correlación 
# lineal, pero luego los champions rompen la correlación al comportarse de manera muy distinta a los otros segmentos.

lcorr2 = lm(nps_clusters$NPS[1:4] ~ c(1:4))

a2 = lcorr2$coefficients[2]
b2 = lcorr2$coefficients[1]

# plot(c(1:4),nps_clusters$NPS[1:4])
# abline(lm(nps_clusters$NPS[1:4] ~ c(1:4))) # Dejo aquí un dibujo esquemático para que se vea el ajuste sin los champions.

ggplot(nps_clusters[1:4,1:2]) +
  geom_point(aes(x=c(1:4), y = NPS)) +
  stat_function(fun=function(x) a2*x+b2) +
  xlim(0,4) +
  labs(title = 'NPS para cada Tipo de cliente', x = 'Tipo de cliente (nº)', y = 'NPS') +
  theme_bw()

#Que sucede si realizamos un promedio de NPS por cada segmentos para cada tienda?
#los segmentos puntúan muy diferente a cada tienda? Observamos algun patron?

nps_segm_tienda = ventas2020 %>% 
  group_by(store, Tipo_de_cliente) %>% 
  summarise(Mean_nps = mean(nps))

head(nps_segm_tienda)

# Para visualizar patrones y si puntúan más o menos diferente, lo mejor es hacer gráficas:

ggplot(nps_segm_tienda) +
  geom_point(aes(x = rep(c(1:5), 5), y = Mean_nps, colour = store)) +
  geom_line(aes(x = rep(c(1:5), 5), y = Mean_nps, colour = store)) +
  labs(title = 'Puntuación promedio de cada tipo de cliente para cada tienda', x = 'Tipo de cliente (nº)', y = 'Puntuación promedio') +
  theme_bw() # Gráficas juntas.

ggplot(nps_segm_tienda) +
  geom_point(aes(x = rep(c(1:5), 5), y = Mean_nps, colour = store)) +
  geom_line(aes(x = rep(c(1:5), 5), y = Mean_nps, colour = store)) +
  labs(title = 'Puntuación promedio de cada tipo de cliente para cada tienda', x = 'Tipo de cliente (nº)', y = 'Puntuación promedio') +
  facet_wrap(~store, ncol = 5) +
  theme_bw() # Gráficas por separado.

#* Nota. Ambas variables deben ser cuantitativas para llevar a cabo el gráfico de líneas, así que no he puesto el tipo de cliente
#* en forma categórica en el eje X, sino su equivalente numérico, que recordemos que es:
#* 
# 1 <- "inactivo"
# 2 <- "en riesgo"
# 3 <- "potenciales"
# 4 <- "leales"
# 5 <- "champions"

#* Vemos que todos los segmentos puntúan entre 7.5 y 8.05 aproximadamente para todas las tiendas, así que lo primero que hay que
#* decir es que no puntúan muy diferente en general. Ahora bien, si entramos en detalle, podemos observar casos individuales en 
#* los que sí se puntúa bastante diferente según la tienda; por ejemplo,  los potenciales (3) puntúan bastante más bajo en la
#* tienda 1 que en las otras.
#* 
#* Resumiéndolos, hay que destacar que la tienda 2 está obteniendo notas muy bajas para los clientes tipo champion, la 1 está 
#* obteniendo la peor puntuación promedio para los leales y los potenciales y que la 5 está obteniendo notas bastante altas 
#* para los champions.
#* 
#* En cuanto a patrones, en la gráfica con todas las curvas podemos observar cierta tendencia a mantener las puntuaciones entre
#* un 7.75 y un 7.95, que es el intervalo donde están contenidos la mayor parte de los valores. En cuanto a la representación
#* de gráficas separadas, lo único destacable es que en 4 de las 5 tiendas, los leales puntúan más alto que los potenciales, lo
#* que, aunque es un poco rebuscado, es el único patrón relativamente fiable que he podido extraer de esta segunda 
#* representación ("los leales tienden a puntuar más alto que los potenciales").

#Que sucede si correlacionamos frecuencia de compra de los 172 ids con el NPS? 
#Los consumidores que tienen mayor frecuencia de compra puntúan mas y mejor?

nps_score2 <- ventas2020 %>% 
  group_by(id_member) %>% 
  summarise(Frequency=length(id_member), NPS = nps(nps)); 

# Le añadimos el cálculo de la frecuencia, ya realizado antes en calculo_RFM para que el dataframe contenga ambas columnas:

View(nps_score2)

cor.test(nps_score2$Frequency, nps_score2$NPS) #* p_value = 0.2751 > 0.05. Se rechaza la hipótesis nula y, por lo tanto,
#* se observa cierta correlación lineal entre ambas variables. Sin embargo,
#* el coeficiente de correlación es pequeño (-0.084) y negativo (luego la 
#* pendiente es negativa y, en consecuencia, tenemos una relación de 
#* proporcionalidad inversa).

# Calculemos los parámetros de la recta de ajuste:

lcorr3 <- lm(nps_score2$NPS~nps_score2$Frequency) 
summary(lcorr3) #* Como era de esperar, r^2 es muy pequeño: 0.0070 (aproximadamente).

a3 = lcorr3$coefficients[2] # Pendiente: -0.00142  (aproximadamente).
b3 = lcorr3$coefficients[1] # Ordenada en el origen: 0.247 (aproximadamente). 

# plot(nps_score2$Frequency,nps_score2$NPS) 
# abline(lcorr3) 

ggplot(nps_score2) +
  geom_point(aes(x = Frequency, y = NPS)) + # Dibujamos el gráfico de dispersión asociado.
  stat_function(fun=function(x) a3*x+b3) + # Dibujamos la recta de regresión.
  xlim(min(nps_score2$Frequency),max(nps_score2$Frequency)) +
  labs(title = 'NPS en función de la Frecuencia de compra de cada cliente', x = 'Frecuencia', y = 'NPS') +
  theme_bw()

#* Obtenemos que, a mayor frecuencia, menor nps y, por lo tanto, a mayor frecuencia de compra,
#* los clientes puntúan peor. En cualquier caso, la correlación es MUY mala y, por lo tanto, las 
#* predicciones que nos proporciona la recta de regresión van a carecer de precisión (no son fiables).
#* Esto último se puede ver reflejado en la desviación típica de la pendiente, cuyo valor (obtenido con
#* la función summary) era de, aproximadamente, 0.0013 (enorme, teniendo en cuenta que la pendiente vale 
#* -0.00142, aproximadamente).
#* 
#* En conclusión: aparentemente, los consumidores con mayor frecuencia de compra puntúan peor a la tienda,
#* pero no podemos estar seguros de que este resultado sea fiable. Una posible solución sería estudiar una 
#* muestra mayor y ver si se obtienen resultados más fiables. 

#En líneas generales luego del análisis exploratorio, ¿podriamos identificar tiendas que sobresalen
#por una buena o una mala performance en terminos de NPS?

#* Al principio vimos que la tienda 4 era la que tenía mejor NPS si agrupamos por tienda y que 
#* la 3 y la 5 no se alejaban mucho de su valor, por lo que la 1 y la 2 parecían ser las peores. Después,
#* vimos en las gráficas que la tienda 3 ha sido la que mejor NPS ha obtenido el último mes del que
#* tenemos datos y que, en efecto, junto a la 4 y la 5, obtiene mejores valores de NPS, en general, 
#* que las tiendas 1 y 2. Por último, hemos visto que la tienda 2 obtiene una puntuación promedio especialmente mala 
#* de sus champions, que la tienda 1 no se queda atrás y tiene las peores notas en promedio para 
#* leales y potenciales, y que la tienda 5 está obteniendo notas especialmente altas para los champions.
#* 
#* Por lo tanto, hemos identificado a las tiendas 3, 4 y 5 como las mejores (destacando cada una en 
#* un aspecto relevante a nivel cuantitativo) y a las tiendas 1 y 2 como las peores (todo en términos 
#* de NPS).

#Pueden utilizar los dataset NPS_T2.csv y NPS_T3.csv para seguir practicando
#Y realizando cruces de informacion
