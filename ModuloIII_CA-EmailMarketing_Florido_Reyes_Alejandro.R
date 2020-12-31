# Autor: Alejandro Florido Reyes

#Análisis explotatorio de campañas de E-mail Marketing

# setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Customer Analytics/Practica_EmailMarketing")

#PASO 1: Deben cargar las librerias:
#tidyverse, lubridate, corrplot
library(tidyverse)
library(lubridate)
library(corrplot)

#PASO 2: Generar un dataset llamado email_analysis cargando el archivo "dataset_email-mkt.csv" eliminando 
#primera columna

email_analysis <- read.csv("dataset_email-mkt.csv", header = T, sep = ',') %>% 
  select(-1)

View(email_analysis) # Personalmente, prefiero tener una visión global de la tabla en una ventana aparte.

# También se podría haber usado summary, str o head/tail para obtener una visión resumida del dataframe.

#Modificar la columna de sendout_date para que sea fecha y no character
email_analysis$sendout_date <- as.Date(email_analysis$sendout_date, "%Y-%m-%d") # La sobreescribimos con tipo Date.
str(email_analysis) # Comprobamos que se ha sobreescrito correctamente.

#PASO 3: Generar un segundo dataset email_campaign filtrando la columna
#email_scope == "Campaign"

email_campaign <- email_analysis %>% 
  filter(email_scope == "Campaign")

View(email_campaign)

#Calculen los datos agregados de todas las columnas que comienzan con "total_"
#agrupando por journeys_id

agre <- email_campaign %>% 
  select(4:15,journeys_ids) %>%
  group_by(journeys_ids) %>% 
  summarize_all(sum, na.rm = T) # Le ordenamos que ignore los NAs al hacer la suma.

View(agre)

#Realicen un plot de la cantidad de envios de mails para cada journeys_ids

ggplot(agre) +
  geom_col(aes(x = journeys_ids, y =total_email_sent), color = 'cyan', fill = 'black', alpha = 0.8) +
  labs(title = 'Emails enviados para cada journey id', x = 'Journey id', y = 'Emails enviados') +
  theme_bw()

#* He decidido usar barras en lugar de puntos porque creo que el gráfico que se obtiene con barras es más visual y más 
#* entendible por un público de perfil no técnico, como podría ser un equipo de marketing.

#PASO 4: Realizar los cálculos de open_rate y ctor para cada journeys_id
#OR: el porcentaje de emails que fueron abiertos por los
#destinatarios sobre el total de emails enviados.
#Click to Open Rate (CTOR): El porcentaje de usuarios que
#recibieron el mail, lo abrieron y realizaron clic en el link deseado.

orctor <- agre %>% 
  summarise(journeys_ids, OR = total_email_open/total_email_delivered*100, CTOR = total_email_clicks/total_email_delivered*100)

# Tanto para OR como para CTOR tengo que tener en cuenta la entrega efectiva, por lo que uso los emails entregados como denominador.

View(orctor) # Añado la columna journeys_ids porque será útil cuando haya que deducir qué campaña ha dado mejores resultados.

#* Estoy observando que pierdo una fila del df y no sé por qué, voy a investigarlo...

#**********************************************************************************************
#* He observado en orctor que me falta la journey id 79. Veamos qué datos contiene:

email_analysis %>% 
  filter(email_analysis$journeys_ids == 79)

#* Podemos observar que contiene únicamente datos que no son de campañas (véase 
#* la columna e-mail_scope). Por lo tanto, esta columna no influye en los análisis de campañas.

#**********************************************************************************************

#Cual es el OR y CTOR promedio de todas las campañas realizadas?

meanorctor <- orctor %>% 
  summarise(OR_promedio = mean(OR),CTOR_promedio = mean(CTOR)) 

head(meanorctor) # Observamos que: mean(OR) = 95.5, mean(CTOR) = 12.3.

# Si quisiéramos el cálculo con más decimales, también podríamos usar View:

View(meanorctor) # Observamos que: mean(OR) = 95.50527, mean(CTOR) = 12.28233.

#Cuales son las campañas que mejor han performado?

# Serán aquellas con mayor OR o mayor CTOR (según los objetivos de la empresa):

which.max(orctor$OR) # Índice (fila) 85, esto es, journey id 86 en orctor, con OR del 3000%.
which.max(orctor$CTOR) # Índice (fila) 87, esto es, journey id 88 en orctor, con CTOR del 300%.

#* En caso de que a la empresa le interese simplemente tener visibilidad, podríamos decir que
#* la que tuvo mayor Open Rate fue la mejor. En este caso esa sería la de journeys_ids = 86,
#* cuyo índice en la tabla es 85 (ya que la 79 se queda fuera del análisis, como comprobamos 
#* antes). La tasa de OR en este caso es del 3000%. Sin embargo, este dato es muy extremo, por
#* lo que puede ser interesante analizar otras campañas que hayan tenido un buen desempeño, por
#* si acaso este valor anómalo no se pudiera tener en cuenta realmente (un mismo email enviado
#* ha sido leído 30 veces y en el dataframe agre no hay otros precedentes similares a este). 
#* 
#* Según hemos visto en la teoría, los valores típicos de la OR están entre el 12% y el 60%, pero su 
#* promedio es del 24%, así que vamos a suponer que una buena campaña sería aquella que está por
#* encima del promedio y a calcular qué campañas cumplen esta condición:
 
GoodOR <- orctor %>% 
  select(journeys_ids, OR) %>% 
  filter(OR > 24 & OR <3000) # Evitamos el valor extremo con el 3000%. 
                             # Observamos que no es el único por encima del 100%.
GoodOR

#* Como son bastantes campañas, podemos ser más restrictivos para quedarnos solo con las mejores
#* a la vez que evitamos tasas por encima del 100%, que podemos considerar anómalas:
#* 

VeryGoodOR <- orctor %>% 
  select(journeys_ids, OR) %>% 
  filter(OR > 80 & OR <= 100)

View(VeryGoodOR)

#* Vemos que las 4 mejores campañas con un valor de OR "normal" (por debajo del 100%) son las de
#* journeys_id 35, 68, 84 y 88. 
#* 
#* Salvo el 35 (que tampoco tiene un valor muy alto), podemos observar
#* en "agre" que, en general, las que han tenido mejor performance están entre aquellas campañas
#* que menos emails habían enviados, lo que es lógico, ya que cuanto menor sea el número de usuarios
#* al que le envías emails, mayor es la probabilidad de que todos abran el email (es más fácil que lo
#* habrán 5 personas que 500).
 
#* Por otro lado, si a la empresa tiene por objetivo que el cliente la conozca lo más a fondo
#* posible e incluso tenga mayores posibilidades de convertirse en comprador (que es lo típico),
#* entonces lo que más le interesará será el CTOR y la campaña que mejor ha cumplido su 
#* función ha sido aquella cuyo journeys_ids = 88 (índice 87 en esta tabla por el mismo motivo
#* que comenté en el párrafo anterior). La tasa de CTOR es en este caso del 300%. Sin embargo,
#* de nuevo se trata de un valor anómalo y, por lo tanto, no fiable.
#* 
#* Según la teoría, la CTOR oscila normalmente entre  el 6 y el 32%, teniendo su valor promedio
#* en un 15%. Filtremos aquellas campañas que superen este valor promedio para saber qué campañas
#* han tenido un buen CTOR:
#* 

GoodCTOR <- orctor %>% 
  select(journeys_ids, CTOR) %>% 
  filter(CTOR > 15 & CTOR <= 100)

View(GoodCTOR) # Hay 12 resultados, que son bastantes, vamos a quedarnos con las mejores.

VeryGoodCTOR <- orctor %>% 
  select(journeys_ids, CTOR) %>% 
  filter(CTOR > 50 & CTOR <= 100)

View(VeryGoodCTOR) 

#* Vemos que las 3 mejores, si no tenemos en cuenta los valores anómalos, son las 
#* de journeys_ids 55, 76 y 86. Además, se repite el hecho de que las tres mejores
#* han sido campañas con nº de emails entregados muy pequeños (15, 9 y 1 en este 
#* caso), por lo que podemos decir que tiene sentido que lo sean.

#Las campañas que peor performan son aquellas donde más "flag_unsubscribe"
#con valor TRUE existen?

# Comprobémoslo:

sum(is.na(email_analysis$flag_unsubscribe)) # Comprobamos que la columna no contiene NAs, por si acaso.

flag <- email_campaign %>% 
  select(flag_unsubscribe, journeys_ids) %>% 
  group_by(journeys_ids) %>% 
  summarise(Numero_de_Trues_en_flag_unsubscribe = sum(flag_unsubscribe)) 

#* False = 0 y True = 1, luego si la nueva columna tiene un valor distinto de cero, para esa fila había 
#* True's (y dicho valor indica el número de True's que había).

View(flag)

#* Como son muchos datos, voy a juntar los datos de OR y CTOR en la misma tabla que los Flag y 
#* voy a dibujar un diagrama de dispersión para ver si es cierto lo que se pregunta.

flag$OR <- orctor$OR
flag$CTOR <- orctor$CTOR
View(flag)

ggplot(flag) +
  geom_point(aes(x = Numero_de_Trues_en_flag_unsubscribe, y = OR), color = 'blue', position = 'jitter') +
  geom_point(aes(x = Numero_de_Trues_en_flag_unsubscribe, y = CTOR), color = 'red', position = 'jitter',
             shape = 'º', size = 2.5) +
  ylim(0,310) +
  labs(title = "Valores de OR (azul) y CTOR (rojo) registrados según el número de True's en flag_unsubscribe", x = "Número de True's",
       y = '%') +
  theme_bw()

#* Para empezar, tengo que comentar que he utilizado la opción jitter para espaciar los puntos y
#* que se vea más clara la cantidad de puntos que hay para cada valor (los valores del eje X en realidad son enteros).
#* También he omitido el valor de OR = 3000 para que se vieran mejor las distancias entre puntos,
#* ya que el valor de 3000 hacía que la gráfica se alejara demasiado y no se reflejaran claramente dichas
#* distancias.
#*  
#* Dicho eso, observamos que la mayor parte de valores tienen flag_unsubscribe = False y dentro
#* de estos valores se han obtenido tasas de todo tipo, desde nulas (sobre todo para el CTOR) 
#* hasta las más grandes.
#* 
#* Por lo tanto, nunca se han obtenido las mejores tasas con algún
#* True en flag_unsubscribe. Por otro lado, para el caso de la OR no se puede decir que se obtengan siempre los
#* peores cuando hay True's, sino más bien que, como mucho, se obtienen valores medios de las OR que se pueden 
#* obtener si no hay ningún True.
#* 
#* Sin embargo, para el caso de la CTOR está más claro que se van a obtener los peores resultados
#* si hay algún True, ya que, aunque también se pueden obtener si no los hay, en caso de haber 
#* algún True, los resultados obtenidos siempre son muy cercanos a un 0%. Sin True's hay varios
#* casos de buenas CTOR.
#* 
#* Conclusión: no se puede decir que si hay True's el resultado va a ser necesariamente peor que
#* si no los hay, ni mucho menos que cuantos más True's hay, peor es el resultado (ya que, además,
#* hay pocos valores para agregados de True's altos, así que no estaría bien afirmar que existe 
#* dicha relación). Así que no, no es cierto que las campañas que han obtenido peores resultados 
#* sean aquellas donde existen más True's. 

#PASO 5: Realizar análisis de los usuarios según su género, realizando un nuevo
#dataset que agregue los datos según género

# REV CAMBIADO A EMAIL_CAMPAIGN.

gene <- email_campaign %>% 
  select(4:15,gender) %>% 
  group_by(gender) %>% 
  summarize_all(sum,na.rm=T)

View(gene)

#* Como es lógico, he obviado los campos sobre los que no se podía sumar (y también los booleanos),
#* al igual que hicimos la primera vez que agregamos (agrupando por journeys_ids).

#Calcular métricas de OR y CTOR para cada género e identificar si se perciben
#diferencias de comportamiento en relación a la tasa de apertura y clics

(orctor2 <- gene %>% 
  summarise(Genero = gender,OR = total_email_open/total_email_sent*100, CTOR = total_email_clicks/total_email_sent*100))

#* f OR = 44.5%, CTOR = 3.40%
#* m OR = 50.7%, CTOR = 4.05%
#* u OR = 31.3%, CTOR = 2.93%

#* Podemos observar que las tasas de apertura y clics son inferiores cuando no se conoce el 
#* género, sobre todo en el caso de la tasa de apertura (Open Rate). En cuanto a hombres y 
#* mujeres, ambas tasas son algo menores para el caso de las mujeres.

#Qué sucede con la cantidad promedio de páginas vistas por género?

#* La cantidad promedio de páginas vistas por género viene dada en la siguiente tabla:

(gene2 <- email_campaign %>% 
  select(total_pageviews,gender) %>% 
  group_by(gender) %>% 
  summarize_all(mean,na.rm=T)
)

#* f mean(total_pageviews) = 4.36
#* m mean(total_pageviews) = 3.26
#* u mean(total_pageviews) = 3.25

#* NOTA. En este caso habría que tener en cuenta que el número de páginas en total es mucho menor 
#* que en el caso de los e-mails, de tal forma que el número total de páginas vistas por género no 
#* llega a los 100 para ningún género, lo que demuestra que la muestra es pequeña con respecto a la 
#* total y, en consecuencia, que las conclusiones que se extraigan de la misma no tienen por qué ser
#* muy fiables. El total de páginas vistas viene dado por:
 
gene$total_pageviews

#* Y es de 48 para mujeres (f), 88 para hombres (m) y 39 si no se conoce el género (u). Véase que 
#* el número es muy inferior al número de filas del dataframe original:

nrow(email_campaign) # Que es de 14988.

#* Dicho eso, intentemos sacar conclusiones de los datos, a pesar de que tengamos una cantidad pequeña de los mismos.
#* Podemos observar que las mujeres ven, en promedio, más páginas que los hombres y que cuando
#* no se conoce el sexo, el promedio de páginas vistas es prácticamente igual al de los hombres.
#* Estos datos no dejan de ser curiosos si consideramos los datos del dataframe donde hemos sumado
#* los valores de esta columna agrupando por género (gene), ya que ahí podemos 
#* observar que son los hombres los que han visto más páginas en total, lo que significa que en
#* la base de datos hay más hombres que mujeres (o que hay números similares, pero muchos NAs en
#* el caso de las mujeres). Veamos qué está pasando realmente:

table(email_analysis$gender) #* Vemos que, como se planteó en el párrafo anterior, lo que ocurre 
                             #* es que hay muchos más hombres que mujeres.
#*   f   m    u
#* 2909 5629 6462
#* 
#* De esta tabla también se puede deducir que hay un gran % de aquellos cuyo sexo no 
#* es conocido de los que directamente no hay información de cuántas páginas han visto (NAs).

#Los hombres o las mujeres exhiben un comportamiento diferencial?

#* Sí. Por una parte, hemos visto que los hombres tienen mayor tendencia que las mujeres a abrir
#* correos y hacer click en ellos. Por otra parte, hemos visto que las mujeres miran, en promedio,
#* un mayor número de páginas. Si bien los valores obtenidos no presentan diferencias extremas
#* entre ambos géneros, sí que podrían servir como punto de partida para dar un enfoque 
#* personalizado a los clientes de esta empresa; por ejemplo, intentando hacer su página más 
#* atractiva para las mujeres (de modo que tiendan a visitarla más aún) y mandando más 
#* información en forma de correos a los hombres.





