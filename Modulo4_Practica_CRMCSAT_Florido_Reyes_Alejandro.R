# Autor: Alejandro Florido Reyes

#MÓDULO IV - SATISFACCIÓN DEL CONSUMIDOR
#En este ejercicio estaremos trabajando con data típica que uno puede extraer del modulo de Support Service
#de un CRM sobre los problemas que se reportan con el producto o servicio
#Y contiene una encuesta de satisfaccion tanto para el producto como para el servicio brindado por el equipo de custome support

#Para completar el ejercicio deberan cargar las siguientes librerias: tidyverse, corrplot, psych

library(tidyverse)
library(corrplot)
library(psych)

#PISTA: Las librerias de corrplot y psych fueron utilizadas en el ejercicio de Percepcion de Marca. 
#Pueden revisar ese script como referencia para esta tarea

#PASO 1
#Cargamos el dataset "data_CRM.csv" eliminando columna 1 que es innecesaria
#Inspecciones en dataset con summary, describe y head para comprender la información que contiene, entender missing values en las columnas
#Realicen un plot para entender la distribución de las quejas a lo largo del período, 
#selecciondo el tipo de gráfico más óptimo para representar la data

crm <- read.csv("data_CRM.csv") %>% 
  select(-1); #View(crm) # Personalmente, he preferido usar View en primer lugar para tener
                         # una primera visión de la tabla, pero lo comento porque no se pide.

summary(crm) #* Este comando nos resume la información más relevante del df. 
             #* Date viene como character, eso lo vamos a corregir.
             #* Posibles variables categóricas que aparecen como character no 
             #* serán transformadas en factor por ahora, esperaré a que pidan 
             #* usarlas y a que sea necesaria dicha transformación (algunos
             #* ejemplos son pending_call y gender).

describe(crm) #* Nos devuelve los principales estadísticos y parámetros importantes para el 
              #* análisis habitual de las variables (máx, mín, media, etc.). 

head(crm) # Vemos las 6 primeras filas del df.

crm$create_date <- as.Date(crm$create_date) #* Ponemos la columna de fechas en tipo fecha (Date).
str(crm)

# Con respecto a valores perdidos (NA's), parece que solo los hay en las columnas de encuestas,
# comprobémoslo:

table(is.na(crm)) # Hay 8374 NAs (nótese que 8374/2 = 4187).
table(is.na(crm$ProdCSAT)) # Hay 4187 del total en ProdCSAT.
table(is.na(crm$ServCSAT)) # Los 4187 restantes están en ServCSAT.

# En efecto, solo los hay en esas columnas. ¿Qué significan? Supongo que un NA en estas columnas hace
# referencia a que el cliente no ha realizado la encuesta y, por lo tanto, la empresa no tiene datos 
# que puedan asociar a dichas columnas.

ggplot(crm) +
  geom_histogram(aes(x=create_date), color = 'cyan', fill = 'black', alpha =0.8) +
  labs(title = 'Distribución de quejas a lo largo del período', x = 'Fecha (mes)', y = 'Frecuencia') +
  theme_bw() 

#* El gráfico ideal para apreciar la distribución de una muestra es, generalmente, un
#* histograma. En este caso, el histograma divide los meses en 8 canales, de modo que cada
#* canal equivale a un intervalo de 4 días. Partiendo de eso hay que destacar que se observa
#* un patrón en la distribución que consiste en que desde el intervalo del 16 al 20 de Enero,
#* cada 20 días aproximadamente los últimos 4 consisten en una bajada en la cantidad de quejas
#* recibidas. Es curioso porque este patrón no se da en un momento concreto del mes (hay veces 
#* que se da al principio, otras a mitad y otras al final) y es prácticamente periódica.
#* 
#* Otro dato que llama la atención es la gran caída en el número de quejas justo en las últimas 
#* fechas.
#* 
#* No he mostrado todas los posibles motivos de queja en el histograma porque son demasiadas y 
#* el escalado de colores no permite diferenciarlas bien (además de sobrecargar la gráfica).

#PASO 2
#El dataset presenta todos los casos de los usuarios que han contactado con Customer Support en el período enero-abril 2020
#Pero nos interesa hacer el análisis sobre complaint_reason, por lo cual es necesario crear un nuevo dataset, 
#agrupar los datos por complaint_reason y realizar las siguientes operaciones para las columnas relevantes: 
#generar una columna que cuente la cantidad de llamadas para cada tipo de complaint_reason llamada "num_casos"
#generar una columna que cuente la cantidad de llamadas pendientes para cada tipo de complaint_reason contando la cantidad de "y" llamada pend_calls
#calcular el promedio de time_to_resolution_min para cada tipo de complaint_reason en una columna nueva llamada avg_time_to_resolution
#generar una columna que cuente la cantidad de need_replace para cada tipo de complaint_reason contando la cantidad de "TRUE" llamada n_replacements
#generar una nueva columna que calcule el Prod_CSAT para cada tipo de complaint_reason en una columna nueva llamada Prod_CSAT
#generar una nueva columna que calcule el Serv_CSAT para cada tipo de complaint_reason en una columna nueva llamada Serv_CSAT
#De esta forma el dataset nuevo debe contener las siguientes columnas: complaint_reason, num_casos, pend_calls, 
#avg_time_to_resolution, n_replacements, Prod_CSAT, Serv_CSAT

# Examinamos la frecuencia de los valores de ProdCSAT y ServCSAT con table():

table(crm$ProdCSAT) # Hay 2066+340 = 2406 cuatros y cincos en ProdCSAT.
table(crm$ServCSAT) # Hay 3812+1648 = 5460 cuatros y cincos en ServCSAT.

# Podemos obtener dichas sumas para hacer los cálculos del siguiente modo:
length(crm$ProdCSAT[((crm$ProdCSAT == 4) | (crm$ProdCSAT == 5)) & !is.na(crm$ProdCSAT)]) # 2406
length(crm$ServCSAT[((crm$ServCSAT == 4) | (crm$ServCSAT == 5)) & !is.na(crm$ServCSAT)]) # 5460

# El problema previo al feedback es que se contaban los na's, ya lo he solucionado. Análogamente ocurría en el denominador:

length(crm$ProdCSAT) # 12000 (ya vimos antes que este es el número de filas de ProdCSAT y ServCSAT, pero lo calculo aquí por hacer 
length(crm$ServCSAT) # el razonamiento más ameno).

length(crm$ProdCSAT[!is.na(crm$ProdCSAT)]) # 7813, que son 12000 menos los 4187 na's que dije previamente que había tanto para ProdCSAT como
length(crm$ServCSAT[!is.na(crm$ServCSAT)]) # para ServCSAT.

# Sustituyo los códigos anteriores en el cálculo de las columnas:

complaint <- crm %>% 
  group_by(complaint_reason) %>% 
  summarise(num_casos = length(pending_call), # El nº de casos incluye tanto las llamadas pendientes (por realizar) como las ya realizadas.
            pend_calls = length(pending_call[pending_call=="y"]), 
            avg_time_to_resolution = mean(time_to_resolution_min),
            n_replacements = length(need_replace[need_replace == TRUE]),
            Prod_CSAT = length(ProdCSAT[((ProdCSAT == 4) | (ProdCSAT == 5)) & !is.na(ProdCSAT)])/length(ProdCSAT[!is.na(ProdCSAT)])*100,
            Serv_CSAT = length(ServCSAT[((ServCSAT == 4) | (ServCSAT == 5)) & !is.na(ServCSAT)])/length(ServCSAT[!is.na(ServCSAT)])*100)

# He tenido en cuenta que hay que quitar crm$ de las expresiones en el summarise (ya que si no, no te hace las operaciones en función
# de la complaint_reason, te la haría para crm entero de una sola vez).

head(complaint) # Le echamos un vistazo rápido con head (6 filas).

View(complaint) #* Una vez que vemos que todo parece correcto, visualizamos 
                #* el dataframe completo (yo lo prefiero, personalmente).
#PASO 3
#Seleccionar un plot idoneo para poder realizar una comparativa de C-SATs para cada problema técnico
#Justificar la selección de dicho plot brevemente

#* Tras el feedback he decidido seguir el consejo de la profesora y realizar un gráfico de barras agrupadas con
#* la variable categórica (complaint_reason) en las abscisas y las variables cuantitativas (Prod_CSAT y Serv_CSAT)
#* en ordenadas:

library(reshape2) # Añado esta librería para hacer un melt, que es un método rápido para poder dibujar las barras agrupadas
                  # cuando las variables que quieres comparar están originalmente en columnas distintas de tu dataframe.

complaint[,-2:-5] # Nos quedamos solo con Prod_CSAT, Serv_CSAT y complaint_reason.
complaint2 <- melt(complaint[,-2:-5], id.vars='complaint_reason') # Hacemos el melt, las abcisas de la gráfica vendrán 
head(complaint2)                                                  # dadas por complaint_reason.

# Tras visualizar el resultado obtenido con un head(), hacemos la representación gráfica con ggplot:

ggplot(complaint2, aes(y = value, x = complaint_reason, fill = variable)) +
  geom_col(position='dodge') +
  labs(y='C-SAT (%)', x = 'Tipo de queja', title = 'Comparación de los valores de cada C-SAT para cada tipo de queja') +
  theme(axis.text.x = element_text(angle = 90))

#* Hay que mencionar que el máximo valor de CSAT se alcanza en ambos casos para el mismo tipo de queja: "Main switch does not on" y que
#* los valores de Serv_CSAT siempre son mayores a los de Prod_CSAT. Por último, mencionar que para "Fan swing not working" no tenemos
#* un valor Prod_CSAT.

#PASO 4
#Realizar una correlación entre las variables numéricas y analizar si existen correlaciones fuertes entre
#alguna de las variables presentadas. 
#las funciones de correlación poseen un argumento llamado use que permite excluir los NA para que el computo sea
#posible. Para ello incluyan como argumento use = "complete.obs" ya que por default es use = "everything" 
#¿La columna de Serv_CSAT muestra correlaciones con alguna otra columna?

View(cor(complaint[-1], use = "complete.obs")) #* Visualizamos la matriz con los coeficientes de
                                               #* correlación lineal o de Pearson (valor que, según la ayuda, 
                                               #* se calcula con cor por defecto).

#* Observamos correlaciones (lineales) fuertes entre las siguientes parejas: numeros de casos con llamadas 
#* pendientes (0.9983) y nº de reemplazamientos (0.7213), llamadas pendientes con número de 
#* reemplazamientos (0.7257), tiempo medio de resolución con ambas C-SATs (de producto,
#* 0.8134, y de servicio, 0.7457) y, por último, entre las notas de dichas encuestas (0.8294). Véase que
#* todas son relaciones de proporcionalidad directa, ya que los valores devueltos por cor no han sido 
#* elevados al cuadrado y, por lo tanto, su signo indica si la pendiente de la recta de ajuste es 
#* positiva o negativa.
#* 
#* Resumiendo la respuesta a la última pregunta del enunciado, la columna Serv_CSAT muestra correlaciones
#* con la Prod_CSAT y con la columna avg_time_to_resolution.
 
#summary(lm(complaint$Prod_CSAT ~ complaint$Serv_CSAT)) #* Esto lo he usado para asegurarme de que cor nos 
                                                        #* devolvía r y no r**2. El valor Multiple 
                                                        #* R-squared que devuelve es el cuadrado del
                                                        #* devuelto por cor para estas dos variables,
                                                        #* esto es, r^2 es el cuadrado del valor 
                                                        #* obtenido mediante cor (que, por lo tanto, 
                                                        #* es r). Lo dejo aquí a modo de curiosidad.

#Inspeccionen la funcion cor.test para entender su funcionamiento y apliquenla sobre aquellas correlaciones
#que ustedes opinaron anteriormente que tienen correlación con la columna de Serv_CSAT para verificar si su hipotesis es correcta
#IMPORTANTE: pueden explorar los diferentes métodos, pero el que utilizamos de forma genérica es pearson
##a su vez es importante que comprendan y utilicen el argumento exact con lógica FALSE

?cor.test #* Usamos la ayuda para examinarla. La función realiza un test con un intervalo de confianza
          #* del 95% por defecto (modificable con conf.level) en el que la hipótesis nula es que la 
          #* pendiente de la recta de ajuste de las dos
          #* variables que se introduzcan en su argumento sea nula. Por lo tanto, para un p-value mayor
          #* que 0.05, no hay correlación lineal (se mantiene la hipótesis y una recta con pendiente nula
          #* presenta un valor constante de la variable dependiente para todos los valores de la variable 
          #* independiente) y para uno menor, se rechaza la hipótesis y, por lo tanto, se aprecia cierta
          #* correlación lineal entre ambas variables (se pasa a la hipótesis alternativa, esto es,  
          #* pendiente no nula).
          #* Otros detalles son que el test también devuelve el valor de cor para esa pareja, el intervalo
          #* de confianza, los grados de libertad del test estadístico si sigue una distribución tipo T y 
          #* el estadístico empleado en el test (en este caso t).
          #* Por defecto, el método empleado es Pearson. Hay otros como el de Kendall o el de Spearman.
          #* El argumento exact, mencionado en el enunciado, acepta valores lógicos (incluyendo NULL, que es
          #* su valor por defecto) y se usa para ordenarle al programa que calcule el valor exacto de p-value
          #* o no. Por lo visto se suele usar (True) con la tau de Kendall y la ro de Spearman, pero no se 
          #* menciona para Pearson.
          #* Hay más argumentos, pero no los he considerado tan relevantes como para mencionarlos aquí.

cor.test(complaint$Serv_CSAT, complaint$Prod_CSAT, exact = F) # p-value = 9.957 * 10**-7.

cor.test(complaint$Serv_CSAT, complaint$avg_time_to_resolution, exact = F) # p-value = 4.431*10**-5

#* En efecto, para ambos test se obtiene un p-value < 0.05, luego se rechaza la hipótesis nula y,
#* por lo tanto, se mantiene la hipótesis alternativa, esto es, que la pendiente de la recta de 
#* ajuste no es nula y, por lo tanto, existe correlación lineal entre ambas parejas de variables.

#* Nota. He comprobado que obtengo los mismos valores independientemente del valor de exact (T,F 
#* o NULL), así que, como indica su definición, este argumento no influye en el método de Pearson.
#* Para comprobar qué hace exactamente, vamos a probarlo con otros métodos:

# --------------------------------- OTROS MÉTODOS - USO DE EXACT ---------------------------------------------------

#* Kendall.  

cor.test(complaint$Serv_CSAT, complaint$Prod_CSAT, exact = F, method = "kendall") # p-value = 0.4279, tau = 0.1190.
cor.test(complaint$Serv_CSAT, complaint$Prod_CSAT, exact = T, method = "kendall") # p-value = 0.4279, tau = 0.1190. Warning.
cor.test(complaint$Serv_CSAT, complaint$Prod_CSAT, exact = NULL, method = "kendall") # p-value = 0.4279, tau = 0.1190. Warning.

#* Vemos que el p-value no cambia si en lugar de poner exact = F, ponemos exact = T ó NULL y se nos devuelve un aviso de que 
#* no puede computar un valor exacto de p-value porque existen "empates" (ties). Dicho eso, en principio el valor que
#* obtenemos si ponemos T debe ser más preciso (según la definición de exact), pero en este caso no podemos comprobarlo
#* por lo que nos comenta el warning. Hay que comentar sobre el NULL que se obtiene lo mismo
#* que para True y eso es exactamente lo que dice en 'Details' (dentro de la ayuda) que debe ocurrir si se cumplen ciertas 
#* condiciones, pero no creo que merezca la pena indagar más aquí (simplemente nos quedamos con que es normal que se obtenga
#* lo mismo, aunque en este caso sea simplemente el mismo warning).

#* Spearman.
 
cor.test(complaint$Serv_CSAT, complaint$Prod_CSAT, exact = F, method = "spearman") # p-value = 0.4809, tau = 0.1547.
cor.test(complaint$Serv_CSAT, complaint$Prod_CSAT, exact = T, method = "spearman") # p-value = 0.4809, tau = 0.1547. Warning.
cor.test(complaint$Serv_CSAT, complaint$Prod_CSAT, exact = NULL, method = "spearman") # p-value = 0.4809, tau = 0.1547. Warning.

# Tenemos el mismo problema que con Kendall, no se puede calcular el valor exacto de p-value porque hay empates (véase el warning).

# En teoría, según pone en 'Details', si ponemos exact = T, p-value se calcula usando un algoritmo llamado AS 89 y, 
# si se pone F, se calcula mediante la "aproximación asintótica de t". Por lo tanto, usar un valor u otro determina el
# modo en que se calcula el p-value y, aunque ambos valores resultan ser parecidos, es natural especificar cuál queremos 
# aplicar cuando hacemos el cálculo, para saber qué ha ocurrido si a otra persona no le sale exactamente el mismo valor.
# En cuanto a NULL, no hay información en 'Details', habría que irse a la documentación completa para estudiar lo que 
# ocurre al usarlo.

# A modo de curiosidad, cabe mencionar que en estos dos ajustes sí que se puede mantener la hipótesis nula (ya que el
# p-value es mayor que 0.05), aunque en este caso no nos interesa saberlo, ya que no vamos a trabajar con estos métodos.

#----------------------------------------------------------------------------------------------------------------------

#Por último utilicen la función corrplot.mixed() para realizar el plot de todas las correlaciones juntas
#Intenten utilizar algunas de las opciones que presenta para embellecer el gráfico (colores, formas, tamaños, etc)
#La forma de aplicación sería corrplot.mixed(corr = (correlacion que quieren hacer con sus argumentos incluido use = "complete.obs")) 
#y el resto de argumentos que quieran incluir

corrplot.mixed(corr = cor(complaint[-1], use = "complete.obs"),bg = 'Black',
               tl.pos = "lt",tl.col = 'Black',upper = "shade")

#* He colocado la forma "shade" en lugar del círculo en la diagonal superior, que creo que muestra más claramente
#* donde están los valores de correlación más altos. Por otro lado, como los nombres de las variables son largos,
#* los he colocado fuera de la matriz en lugar de en la propia diagonal, ya que en dicha diagonal no eran 
#* legibles y tapaban datos. También he cambiado el color del fondo de las casillas de la diagonal principal, ya
#* que no estaba dibujando nada en ellas previamente y así se aprecia más claramente el contraste de colores con 
#* las casillas más claras de la diagonal superior. Por último, he cambiado los colores de los nombres de las
#* variables, que salen en rojo por defecto y me parece que es un color más molesto que útil a la hora de ser 
#* leído; he usado el color negro en su lugar.
#* 
#* Si, con "todas las correlaciones" se refirieran solo a las halladas en relación a Serv_CSAT, entonces quedaría:
#* 

Serv_Corr <- complaint %>% # Separamos las columnas que implican las correlaciones de las demás.
  select(4,6,7)
Serv_Corr # Comprobamos que hemos extraído las columnas correctas.

# Dibujamos la matriz con las correlaciones usando corrplot.mixed():

corrplot.mixed(corr = cor(Serv_Corr,use = "complete.obs"),
               bg = 'Black', tl.pos = "lt",tl.col = 'Black',upper = "ellipse")

#* Aquí he decidido usar la forma elíptica por cambiar un poco: cuanto más se aleje de la forma de un círculo 
#* la elipse dibujada, mayor es el valor del coeficiente de correlación (en valor absoluto). En este caso no 
#* afecta apenas, porque los valores son muy parecidos, pero puede ser interesante para valores muy dispares,
#* aunque a mí personalmente no me acaba de gustar porque ya tienes la escala de colores para saber si el 
#* coeficiente es mayor o menor (y hay que saber de memoria qué es lo que implica la elipse y qué es lo que 
#* implica el círculo si te enseñan un gráfico con estas formas sin habértelo explicado previamente, lo cual
#* no me parece precisamente ideal). En todo caso, lo he hecho así para probar alguna opción más.

#PASO 5
#Repetir el paso 4 pero enfocando el analisis en la columna Prod_CSAT en vez de Serv_CSAT: realicen hipotesis sobre correlaciones,
#apliquen cor.test para validarlas y corrplot.mixed() para representarlo.

#* Hemos dicho anteriormente que Prod_CSAT está fuertemente correlacionado con Serv_CSAT (esto ya ha sido 
#* analizado) y con el avg_time_to_resolution (que sería nuestra nueva hipótesis de correlación en este 
#* apartado). Como ya hicimos el test para la primera correlación, vamos a hacerlo ahora únicamente
#* para la segunda:

cor.test(complaint$Prod_CSAT, complaint$avg_time_to_resolution, exact = F) #p-value = 2.376*10**-6

#* Como era de esperar, p<0.05, luego se rechaza la hipótesis nula y se mantiene la alternativa, esto
#* es, la pendiente de la recta de ajuste no es nula y, por lo tanto, hay correlación lineal entre 
#* ambas variables.
#* 
#* De nuevo, vamos a dibujar el corrplot solo con las variables correlacionadas, ya que de todas formas 
#* he dibujado el corrplot con todas las correlaciones en el apartado anterior.
 
Prod_Corr <- Serv_Corr # Como las variables correlacionadas son las mismas, el dataframe con las tres 
                       # variables es el mismo que el que creé en el apartado anterior para Serv_CSAT.
                       # Creo una nueva variable con otro nombre solo por hacer el código más legible.

corrplot.mixed(corr = cor(Prod_Corr,use = "complete.obs"),
               bg = 'Black', tl.pos = "lt",tl.col = 'Black',upper = "ellipse")

#* Lógicamente, el resultado obtenido es el mismo que en el segundo corrplot que dibujé en el PASO 4.


