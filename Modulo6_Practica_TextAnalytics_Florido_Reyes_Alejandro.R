# Autor: Alejandro Florido Reyes

# Hay que entregar este script y el RData con el environment en un zip (o por separado, da igual).

# MÓDULO VI - TEXT ANALYTICS

#En este ejercicio realizamos un análisis de la conversación en Twitter alrededor de la marca Zara.
#Realizaremos un análisis exploratorio utilizando las técnicas vistas en clase. Finalmente,
#aplicaremos un análisis de sentimiento y modelado de tópicos, que nos permitan profundizar
#en los documentos (tweets) extraídos y en la conversación alrededor de la marca.

# setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Customer Analytics/Modulo6_CustomerAnalytics_PracticaR/Practica_Text_Analytics_Alejandro_Florido_Reyes")

#Para completar el ejercicio deberan cargar las siguientes librerías:
# tidyverse, stringr, rtweet, readtext, tidytext, udpipe, quanteda, syuzhet, topicmodels

library(tidyverse)
library(stringr)
library(rtweet)
library(readtext)
library(tidytext)
library(udpipe)
library(quanteda)
library(syuzhet)
library(topicmodels)

#PISTA: Las librerias fueron utilizadas en los ejercicios prácticos del módulo de Text Analytics 
#Pueden revisar esos script como referencia para esta tarea


#PASO 1
#Realizamos una búsqueda en Twitter utilizando la query de búsqueda "Zara". Fijamos los parámetros:
# n= 18000 (límite máximo permitido de descarga de registros, es posible que se crearan menos tweets en el intervalo temporal seleccionado)
# include_rts= FALSE
# lang= "es"
# since = "2020-12-09"
# until = "2020-12-16" 

#PISTA: consulta la ayuda de la función search_tweets

# help("search_tweets")
# tweets_Zara <- search_tweets(q="Zara", n = 18000, include_rts = FALSE, lang= "es") 

  #* Guillermo me ha dicho que descargue el máximo número de datos posible, así que no he indicado los argumentos since y until 
  #* (porque si no los indicas te baja los datos de los últimos 10 días, que es el máximo de días que podemos examinar con
  #* la versión gratuita).
 
#Inspecciona el dataframe

View(tweets_Zara)
summary(tweets_Zara) # Mucha información, convendría resumir el dataframe.
str(tweets_Zara)     # De nuevo, demasiada información a priori.

  #* Vemos que, en efecto, nos ha descargado tweets que incluyen la palabra "Zara" del 16/12 al 07/12 (los últimos 10 días 
  #* desde el día que empecé a hacer esta práctica). 

# El dataset descargado contiene 90 columnas, pero no nos interesan todas. Selecciona las columnas:
# created_at, screen_name, text, favorite_count, retweet_count, lang, status_url, name, location, 
# description, followers_count, friends_count, statuses_count

resumen_Zara <- tweets_Zara %>% 
  select(created_at, screen_name, text, favorite_count, retweet_count, lang, status_url, name, location, 
         description, followers_count, friends_count, statuses_count)

head(resumen_Zara)  

#* LIMPIEZA

#Convierte el texto en minúsculas. PISTA: utiliza la librería stringr

resumen_Zara <- resumen_Zara %>% 
  mutate(text = str_to_lower(text))

#Convierte la fecha de creación en Date

resumen_Zara <- resumen_Zara %>% 
  mutate(created_at = as.Date(created_at))

str(resumen_Zara) # Comprobamos que, en efecto, ahora la columna created_at es tipo Date.

#Sustituye las letras acentuadas por letras sin acentuar. PISTA: utiliza la librería stringr 

resumen_Zara$text <- str_replace_all(resumen_Zara$text, 'á', 'a') # Cambiamos las letras acentuadas por su versión sin acentuar.
resumen_Zara$text <- str_replace_all(resumen_Zara$text, 'é', 'e')
resumen_Zara$text <- str_replace_all(resumen_Zara$text, 'í', 'i')
resumen_Zara$text <- str_replace_all(resumen_Zara$text, 'ó', 'o')
resumen_Zara$text <- str_replace_all(resumen_Zara$text, 'ú', 'u')

#Inspecciona de nuevo el dataset con summary, str y head.

summary(resumen_Zara) # Podemos observar que están las columnas que hemos seleccionado y ninguna más. También las fechas
                      # en las que se publicaron los tweets que vamos a analizar (entre el 7/12/2020 y el 16/12/2020).
str(resumen_Zara)  # Nada que añadir, ya revisé que created_at era del tipo Date.
head(resumen_Zara) # Esto nos da una primera idea de que los cambios sobre mayúsculas y letras acentuadas se han llevado a cabo
                   # (al compararlo con la columna text del dataframe original).

#Verifica que el texto se transformó en minúsculas y que las letras con acento se sustituyeron por letras sin acentuar
#¿Cuántos registros (tweets) contiene el dataset?

str_detect('Test', "[:upper:]") # Detecta las mayúsculas y devuelve True si encuentra alguna y False si no.
sum(str_detect(resumen_Zara$text, "[:upper:]")) # Lo hacemos sobre toda la columna text y sumamos los valores (False vale 0 y True vale 1).

# Vemos que hay dos textos en los que aún hay mayúsculas, veamos cuáles son:

resumen_Zara$text[str_detect(resumen_Zara$text, "[:upper:]") == T]

# A raíz del último código vemos que las mayúsculas están en unas extrañas trazas que no sé de dónde provienen a priori, pero que no
# parece que vayan a aportar nada al análisis. De cualquier forma, como también contienen urls y algo de texto en una de ellas, no las
# voy a eliminar (ya que dichos elementos sí siguen en minúsculas y pueden ser interesantes para el análisis).

# Comprobamos que se han efectuado los cambios sobre las letras acentuadas:

str_detect('Emáro', "á[:alpha:]") # Devuelve true si hay una á y False si no.
str_detect('Emaro', "á[:alpha:]")

# Usando la misma lógica que con las minúsculas:

sum(str_detect(resumen_Zara$text, "á[:alpha:]"))
sum(str_detect(resumen_Zara$text, "é[:alpha:]"))
sum(str_detect(resumen_Zara$text, "í[:alpha:]"))
sum(str_detect(resumen_Zara$text, "ó[:alpha:]"))
sum(str_detect(resumen_Zara$text, "ú[:alpha:]"))

# Como todas las sumas dan 0, hemos eliminado efectivamente estos 5 caracteres acentuados del texto.

# Nº de registros del dataset:

nrow(resumen_Zara) # 7331 filas (registros).

# No hemos eliminado filas, así que debería ser el mismo número de registros que el dataset original, comprobémoslo:

nrow(tweets_Zara) # 7331, en efecto.

#Añade una nueva columna al dataset que unifique el volumen total de interacciones
#La columna se debe llamar "interacciones" y se calcula como la suma de favorite_count y retweet_count para cada registro

resumen_Zara <- resumen_Zara %>% 
  select_all %>% 
  mutate(interacciones = favorite_count + retweet_count)

View(resumen_Zara) # Comprobamos que la operación se ha realizado correctamente y actualizamos la vista del dataset con este 
                   # View.

# PASO 2 
#Analizamos los datos extraídos y respondemos a preguntas sobre los datos

#Visualiza el número de tweets por día. ¿En qué día se crearon más tweets?

tw_dia <- resumen_Zara %>%
  select(created_at) %>% 
  group_by(created_at) %>%
  summarise(n_tweets = n())

tw_dia

# Dado que tenemos solo 10 días, creo que un gráfico de columnas (barras) puede ser bastante visual:

ggplot(tw_dia) + 
  geom_col(aes(x=created_at, y = n_tweets), color = 'cyan', fill = 'black', alpha = 0.8) +
  labs(title = 'Nº de Tweets por dia', x = 'Fecha (dia)', y = 'Nº de Tweets') +
  theme_bw()

# Como se puede apreciar en el gráfico, el día que se crearon más tweets fue el 15 de diciembre.
# También podemos hacer el cálculo de forma analítica:

max_tw <- max(tw_dia$n_tweets) # Máximo número de tweets en un mismo día. 
tw_dia$created_at[tw_dia$n_tweets == max_tw] # Día en que se obtiene dicho máximo: "2020-12-15".

#Calcula el número total (suma) de interacciones por día. Represéntalo gráficamente
#¿En qué día hubo más interacciones?

int_dia <- resumen_Zara %>% 
  group_by(created_at) %>% 
  summarise(int_dia = sum(interacciones))

int_dia

# Por el mismo motivo que antes, usamos un gráfico de barras para hacer la representación:

ggplot(int_dia) + 
  geom_col(aes(x=created_at, y = int_dia), color = 'cyan', fill = 'black', alpha = 0.8) +
  labs(title = 'Nº de interacciones por dia', x = 'Fecha (dia)', y = 'Interacciones') +
  theme_bw()

# A la vista del gráfico, el día que más interacciones hubo fue el 15 de diciembre.
# Calculémoslo analíticamente también:

max_int <- max(int_dia$int_dia)
int_dia$created_at[int_dia$int_dia == max_int] # En efecto, el resultado es: "2020-12-15".

# Tiene sentido que el día que hubo más interacciones fue el día en que se twitteó más (si bien una cosa
# no implica necesariamente la otra).

#¿Qué cuentas (screen_name) tienen mayor (max) número de followers? Pista, necesito utilizar la columna followers_count

followers <- resumen_Zara %>%
  distinct(screen_name, followers_count) %>% # No me interesa que me devuelva una misma cuenta varias veces: evito duplicados.
  top_n(5, followers_count) %>% # Voy a obtener las 5 cuentas con mayor número de followers directamente, no todas.
  arrange(desc(followers_count)) # Como quiero que me devuelva las de mayor número de followers, ordeno el nº de followers en orden descendente.
View(followers)

# Según la tabla anterior, las 5 cuentas con mayor número de followers son las siguientes:
#
# screen_name followers_count
#
# ElUniversal     5079474
# RevistaSemana   4568365
# biobio          3271446
# Reforma         3209315
# VogueSpain      2707969

          
#¿Cuál fue el tuit con más retweets? Pista, necesito utilizar la columna retweet_count

max_rt <- resumen_Zara %>% 
  select(created_at, screen_name, text, retweet_count) %>% 
  filter(retweet_count == max(retweet_count))

View(max_rt) # Aunque aparezca el texto sin terminar, si ponemos el ratón sobre dicho texto nos aparece el texto completo.
             # Tuvo 120 retweets.

#* He añadido algunas columnas aparte de las del texto y el nº de retweets porque quería comprobar si se había publicado en una fecha concreta
#* (9/12/2020, una fecha bastante normal en cuanto a número de interacciones según la gráfica dibujada antes) y si el autor era una cuenta
#* conocida. Podemos decir que esto último ha ocurrido porque si buscamos en Internet a este usuario de twitter, podemos ver que le siguen
#* casi 17000 usuarios, de modo que podemos decir que esta cuenta tiene cierta influencia y es normal que su tweet haya tenido tanta difusión
#* (retweets). Dejo aquí un enlace para ver el twitter del usuario en cuestión: https://twitter.com/sosabuelos1/?lang=es

#¿Cuál fue el tuit con más likes? Pista, necesito utilizar la columna favorite_count

max_likes <- resumen_Zara %>% 
  select(created_at, screen_name, text, favorite_count) %>% 
  filter(favorite_count == max(favorite_count))

View(max_likes) # Tuvo 1257 likes. Curiosamente, se publicó el día que más interacciones hubo (15-12-2020) y, como las interacciones
                # ese día no llegaron a 5000 (véase la gráfica), podemos decir que contribuyó con más de un 25% de las interacciones
                # (1250 es un 25% de 5000, esto es, una cuarta parte de 5000). Además, he añadido el screen_name para buscar a la 
                # persona que publicó el tweet y comprobar si es algún tipo de influencer (a priori, no lo parece, pero dejo aquí
                # la url por si os surge curiosidad: https://twitter.com/brunelahaddad?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor)

# Dicho todo eso, tiene sentido que el mayor número de likes se produzca el día que más interacciones hay (ya que si hay más interacciones
# lo primero que cabe pensar es que haya más gente reaccionando en la red y, puesto que el tweet no lo ha publicado un influencer, es poco
# probable que la recepción que tuvo por parte de los usuarios se debiera meramente a la influencia del usuario que lo publicó).

#* Otro detalle que cabe destacar es que podríamos pensar que la gente da más likes que retweets en base a los valores máximos de cada
#* variable. Comprobemos si esto es cierto:

m_like <- mean(resumen_Zara$favorite_count) # 2.980357
m_rt <- mean(resumen_Zara$retweet_count) # 0.3215114
m_like/m_rt # 9.269835

#* De modo que la gente da, en promedio, 9 veces más likes que retweets y, por lo tanto, los likes tienen mucho más peso que los retweets
#* en la gráfica de interacciones. Además, esto también se puede interpretar como que un retweet tiene más valor que un like (ya que son
#* más difíciles de conseguir).

#PASO 3
#Tokenizamos el texto, separándolo en palabras y contando el número de palabras.
#Filtramos menciones y visualizamos hashtags

#Utiliza la función unnest_tokens() de la librería tidytext para tokenizar el texto
#Cuenta el número de palabras y ordenálas en orden descendente según su frecuencia
#PISTA: utiliza el parámetro token= "tweets". Consulta la ayuda de la función unnest_tokens()

Tweets_Zara_Token <- resumen_Zara %>% 
  unnest_tokens(input = text, output = "palabra", token = "tweets", drop = FALSE) %>% 
  count(palabra) %>%
  arrange(desc(n))

View(Tweets_Zara_Token) # Como era de esperar, dentro de las palabras que más se repiten hay preposiciones y otras palabras que suelen
                        # carecer de interés de cara al análisis. Sin ir más lejos, la palabra más repetida es la preposición "de".

nrow(Tweets_Zara_Token) # Vemos que se han encontrado 22890 palabras distintas en los tweets, cada una repetida una cierta cantidad de veces.

#Excluye menciones del dataframe tokenizado Tweets_Zara_Token.
#PISTA: utiliza filter() junto con str_detect() con pattern = "@[:alnum:]", consulta el script 2_Libreria_RTWEET

Tweets_Zara_Token <- Tweets_Zara_Token %>%
  filter(!str_detect(palabra, pattern = "@[:alnum:]"))

View(Tweets_Zara_Token) # Comprobamos que ha quitado los @ buscando alguno de los que aparecían antes. Efectivamente, los
                        # ha filtrado.
sum(str_detect(Tweets_Zara_Token$palabra, pattern = "@[:alnum:]")) # Para una comprobación más rigurosa, podemos seguir el mismo método
                                                                   # que para las letras acentuadas o la minúsculas.

# Vemos que la línea de código anterior nos devuelve 0, luego ya no hay palabras con @ en Tweets_Zara_Token ni, por lo tanto, menciones.

#Crea un dataframe que contenga los hashtags. PISTA: consulta el script 2_Libreria_RTWEET

Hashtags_Zara <- Tweets_Zara_Token %>%
  filter(str_detect(palabra, pattern = "#[:alnum:]"))

View(Hashtags_Zara)

#Representamos los hashtags como un wordcloud utilizando la librería wordcloud, que no fue introducida en las sesiones prácticas
#Puedes hacer pruebas y variar los parámetros max.words, min.freq, scale, etc para observar como varía el resultado
#install.packages("wordcloud")
library(wordcloud)
wordcloud( # Hace una representación gráfica de la frecuencia de las palabras.
  words = Hashtags_Zara$palabra, 
  freq = Hashtags_Zara$n, 
  max.words = 50, # Cantidad de palabras que quiero que aparezcan, para que sea legible.
  min.freq = 8, # Podemos filtrar la mínima frecuencia que deben tener las palabras que representemos.
  scale =c(2.5,1.5),    # He observado que tanto la escala como la frecuencia mínima determinan si realmente
  random.order = F, # se alcanza el máximo de palabras específicado e incluso si se filtra alguna de ellas porque no quepan. 
  rot.per = 0.5, # Determina qué porcentaje (en tanto por 1) de las palabras se representan en vertical.
  random.color = F, # Si le pongo random.color = F, todas menos una o dos se me quedan de un mismo color.
  color = brewer.pal(4, "BrBG"))

  # Con random.order = F, las que tienen color distinto con random.color = F son aquellas que tienen mayor frecuencia.
  # Creo que lo que hace es que divide los resultados en intervalos de frecuencia y los pinta de un color u otro según
  # el intervalo de frecuencia que les corresponda (lógicamente hay más palabras en intervalos de bajas frecuencias que
  # en los de alta frecuencia).

  # Otro detalle es que cada vez que lo dibujas, dibuja la nube de forma diferente, pero creo que solo reestructura 
  # la información presentada, esto es, siempre presenta la misma información. *Nota: si la escala es ajustada, existe
  # la posibilidad de que un valor a veces entre en el wordcloud y otras veces se quede fuera (en cuyo caso el comando
  # devuelve un warning) y es en este caso excepcional en el que puede que ejecutar el comando dos veces no devuelva
  # exactamente la misma información.

  # Vemos que #zara es el hashtag más repetido (como podía esperarse por la búsqueda que hemos hecho), seguida por
  # #jor15 y #news. Esto es más visual que mirar las palabras en la tabla y lo sería aún más si mostráramos más de
  # 3 hashtags con alta frecuencia (aunque ese no es el caso en este análisis, pero creo que está bien quedarse con
  # esa idea), aunque tiene el problema de que puede filtrar algunos resultados si no usamos unos valores de scale
  # adecuados, como comentaba antes.

#PASO 4 #* (Análisis de sentimiento)
#Realizamos un análisis de sentimiento utilizando la librería SYUZHET
#A diferencia del script 6_Libreria_SYUZHET, donde aplicamos un análisis de sentimiento por palabra (token),
#en este caso apliqueremos la función get_nrc_sentiment a cada tweet (documento)

#Como el dataset es relativamente grande, en esta sección trabajaremos con una muestra.
#Seleccionamos una muestra de 500 tweets de forma aleatoria utilizando la función sample.

set.seed(1234) # Como vamos a coger una muestra aleatoria, voy a poner una seed para que los profesores veáis el mismo resultado que yo.
Dataset_Zara_subset <- resumen_Zara[sample(nrow(resumen_Zara), size=500), ]

#La función get_nrc_sentiment de la librería Syuzhet permite visualizar las emociones y sentimiento
#Analiza 8 "emociones": anger, anticipation, disgust, fear, joy, sadness, surprise y trust
#Así como la polaridad positivo o negativo.

#Utilizamos la función get_nrc_sentiment() con el parámetro language= "spanish"
Analisis_NRC <- get_nrc_sentiment(char_v = Dataset_Zara_subset$text, language = "spanish")

#Inspecciona el resultado utilizando View()

View(Analisis_NRC) # Vemos que tenemos los sentimientos para cada tweet, así como su polaridad, pero no vemos a qué
                   # tweet va asociado cada análisis en este dataframe, así que lo suyo sería unirlo al utilizado como
                   # base del análisis (o, al menos, a la columna text del mismo).

#Unificamos el resultado y el dataframe de partida Dataset_Zara_subset, utilizando la función cbind()
Analisis_NRC_df <- cbind(Dataset_Zara_subset, Analisis_NRC)

View(Analisis_NRC_df[, c(3,15:24)]) # Creamos una ventana aparte para poder visualizarlo cuando nos haga falta con View().

# De momento nos quedamos solo con el texto y el análisis de sentimiento, que es lo que parece más importante relacionar.
# Además, así es más fácil comparar ambas columnas (si no habría muchas columnas de por medio).

# Inspecciona de nuevo el resultado utilizando summary
# Observa los valores mínimo, máximo y medio para cada una de las 8 emociones y para las columnas negative/positive

summary(Analisis_NRC_df)

#    anger       anticipation      disgust           fear            joy       
#   Min.   :0.00   Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
#   Mean   :0.226  Mean   :0.296   Mean   :0.164   Mean   :0.234   Mean   :0.27  
#   Max.   :6.00   Max.   :7.000   Max.   :6.000   Max.   :5.000   Max.   :6.000  
# sadness         surprise         trust         negative        positive    
# Min.   :0.000   Min.   :0.000   Min.   :0.00   Min.   : 0.00   Min.   :0.000  
# Mean   :0.272   Mean   :0.112   Mean   :0.382  Mean   : 0.516  Mean   :0.66  
# Max.   :5.000   Max.   :3.000   Max.   :6.00   Max.   : 9.00   Max.   :8.000 

# Vemos que el valor mínimo siempre es 0, esto es, hay tweets que no implican una o varias emociones según el algoritmo.
# Me parece lógico, no es normal expresar 8 emociones tan diferentes algunas de otrass en un tweet.

# Vemos que, en el caso de los sentimientos, los valores máximos están en torno a 6 y 7, siendo el máximo de 7 para 
# anticipación y especialmente bajo para la sorpresa (que tiene el valor mínimo con un 3).
# En cuanto a la polaridad, ambos máximos son cercanos, siendo de un 9 para la negatividad y de un 8 para la positividad.

# Por último, las medias de los sentimientos están por debajo de 1 (probablemente porque hay muchos tweets para los que
# el valor de la mayoría de los sentimientos es 0) y las de las polaridades también (también hay muchos ceros en estas
# columnas, lo que disminuye mucho el valor de la media). El sentimiento con mayor media es trust (destacando sobre el 
# resto), mientras que tanto surprise como disgust destacan por tener un valor medio notablemente más bajo que el resto 
# de sentimientos. La polaridad con mayor media es la positiva (a pesar de no tener el mayor máximo), lo cual es 
# perfectamente posible, dada la gran cantidad de datos que estamos examinando en este análisis de sentimientos, que hace
# que un valor máximo tan similar al de la polaridad negativa no influya apenas en los resultados de las medias.

# 1) Calcula la suma total de la columna positive
# 2) Calcula la suma total de la coluna negative.
# ¿La polaridad de la conversación es positiva o negativa? PISTA: resta el total negativo al total positivo

(Negativo_total = sum(Analisis_NRC_df$negative)) # 258 para negative.
(Positivo_total = sum(Analisis_NRC_df$positive)) # 330 para positive.

Positivo_total-Negativo_total # 72 (> 0 y, por lo tanto, positivo).

# Positivo_total > Negativo_total: la polaridad de la conversación es positiva, esto es, los tweets son, en balance, 
# más positivos que negativos.

#Finalmente podemos analizar el porcentaje de cada emoción en la conversación
#Solución: utilizamos la función prop.table y colSums para obtener el porcentaje de cada emoción
# La función prop.table divide el valor de cada celda entre la suma total de filas y columnas (% de la celda)
# La función colSums() suma el valor de todas las celdas de cada columna (% de la columna)
Analisis_NRC_emotions <- colSums(prop.table(Analisis_NRC_df[c("anger", "anticipation", "disgust", "fear",
                                                              "joy", "sadness", "surprise", "trust")]))
sort(Analisis_NRC_emotions*100, decreasing= TRUE)

# Observamos que los mayores porcentajes se obtienen para anticipation (~15.13%) y trust (~19.53%). Los menores son los de surprise (~5.73%)
# y disgust (8.38%). Estos sentimientos parecen cuadrar muy adecuadamente con fechas tan señaladas como son las navidades, donde los 
# familiares esperan (trust) reencontrarse y pasar una serie de días tranquilos y felices con sus familias (lo que podría entrar dentro
# del sentimiento de anticipación). 

# Cabe mencionar que los sentimientos de tristeza (sadness) y felicidad/júbilo (joy) están muy a la par y los de miedo e ira también, lo que
# podría denotar confusión ante el panorama actual en el que algunas familias no podrán reunirse o tienen miedo de un rebrote frente al COVID.
# Aunque el COVID no es un tema propio de Zara, hemos aprendido durante los últimos meses que es un tema transversal que está afectando a
# todas las facetas de la sociedad, así como ocurre con Navidad.

#Inspeccionamos ejemplos
angry_items <- which(Analisis_NRC_df$anger > 0) # Crea un vector con los índices para los que anger > 0.
Analisis_NRC_df[angry_items, "text"] # Obtiene el texto para las filas asociadas a esos índices, esto es, las asociadas
                                     # con un sentimiento de ira/enfado.

# Hay resultados que no son muy acertados, como el [60] (quizás el algoritmo intenta detectar sentimientos detrás de la
# ironía) y el [43] (es una frase muy objetiva, quizá es que el enlace llevaba a una broma o algo así, pero a mí no me muestra nada),
# pero en general me parece que el algoritmo acierta bastante.

joy_items <- which(Analisis_NRC_df$joy > 0) # Hace lo mismo para el sentimiento "joy".
Analisis_NRC_df[joy_items, "text"]

# A primera vista, los resultados obtenidos podrían estar asociados con el sentimiento de alegría (joy) aunque, de nuevo, los hay
# más claros y menos claros. Por ejemplo, el [79] parece más una afirmación que una frase que manifieste felicidad, aunque normalmente
# el comprador considerará esa frase como algo bueno y, por lo tanto, quizá podría asociarse a dicho sentimiento desde ese punto de vista.

#Nota: este ejercicio es un ejemplo de cómo trabajar con la librería Syuzhet y realizar un análisis de sentimiento
# En un caso real, se debe analizar y limpiar en profundidad el conjunto de documentos (tuits en este caso),
# por ejemplo eliminando menciones, urls y documentos (tuits) no relevantes del análisis de sentimiento.

  # De acuerdo.

#PASO 5
#Analizamos el dataset utilizando la libería udpipe.

#Descargamos y cargamos el modelo para español. 
#ud_model <- udpipe_download_model(language = "spanish") # Lo comento porque yo ya descargué una versión concreta y no quiero cambiarla.
#ud_model <- udpipe_load_model(ud_model$file) #Esta línea no se ejecuta correctamente si existe más de un modelo en el directorio de nuestro ordenador
ud_model <- udpipe_load_model(file= "spanish-gsd-ud-2.5-191206.udpipe") #Al especificar el nombre del modelo a cargar, aseguramos que sí cargue el modelo correctamente

# He especificado el nombre del modelo copiando y pegando desde mi carpeta de trabajo. Incluiré el archivo utilizado en el 
# zip cuando entregue el ejercicio (por eso no he usado directamente ud_model$file a pesar de tener solo un modelo, para
# que sepáis cuál he usado).

#Lo aplicamos sobre la columna del texto de tuits, generando 14 variables
Dataset_Zara_ud <- udpipe_annotate(ud_model,
                                   x = resumen_Zara$text,
                                   parallel.cores = 2)

#Convertimos en data frame. Inspecciona el resultado, revisa las variables generadas por la función udpipe_annotate()
Dataset_Zara_ud <- as.data.frame(Dataset_Zara_ud)
View(Dataset_Zara_ud)    # Se nos crea una columna con cada palabra de cada tweet (incluyendo signos de puntuación
                         # no deseados y palabras poco relevantes como preposiciones o conjunciones, ya que no habíamos
                         # limpiado a fondo). También tenemos columnas que nos indican en qué frase o "párrafo" del tweet 
                         # estamos, así como a qué tipo de palabra corresponde cada palabra del tweet, su raíz,
                         # su género y si es un signo de puntuación, una palabra, etc.

summary(Dataset_Zara_ud) # La mayor parte son columnas tipo caracter, pero sentence_id nos muestra que los tweets tenían
                         # un número de frases medio es de 1.378, lo cual es lógico porque los tweets suelen tener una 
                         # extensión corta (es para lo que están pensados, no lo digo porque lo haya examinado directamente
                         # en el dataframe). Vemos también que el máximo número de párrafos que asigna a un tweet es 6,
                         # lo que puede tener sentido, ya que no sé cómo clasifica exactamente por párrafos pero, como los
                         # tweets no pueden ser muy largos (como decía antes y por el número de caracteres), 6 es un número
                         # que puede llegar a entenderse (de todas formas el nº medio de párrafos es 1.06, más lógico aún).

#Observa que los signos de puntuación no han sido eliminados
#Utilizando la columna "token", elimina los signos de puntuación y las menciones
#PISTA: para eliminar signos de puntuación utiliza el patrón "[:punct:]". Revisa la cheatsheet de stringr vista en clase.

# En efecto, observo por ejemplo signos de interrogación (?).

Dataset_Zara_ud <- Dataset_Zara_ud %>% 
  filter(!str_detect(token, pattern = '[:punct:]'))

sum(str_detect(Dataset_Zara_ud$token, pattern = '[:punct:]')) # La suma devuelve 0, así que se han eliminado correctamente.

View(Dataset_Zara_ud) # Visualizamos el dataset actualizado.

#Analicemos los adjetivos
Adjetivos_Zara <- Dataset_Zara_ud %>%
  filter(upos == "ADJ") %>%
  count(token) %>%
  arrange(desc(n))

wordcloud(
  words = Adjetivos_Zara$token, 
  freq = Adjetivos_Zara$n, 
  max.words = 80,
  min.freq = 5,
  scale =c(8,0.4), # He cambiado la escala de 4.8 a 8 para que se vean mejor los adjetivos reales. Esto puede dejar fuera
  random.order = T, # a "zara" en algunos casos, pero como no es realmente un adjetivo, no nos importa (además, aparece
  rot.per = 0.3, random.color = T, # un warning indicándonos que ese valor no ha entrado en el gráfico, así que sabemos
  color = brewer.pal(4, "BrBG"))   # que en principio se habría dibujado. No se nos pasa por alto).

# Vemos que hay varias palabras como "hola" o "zara" que se están considerando como adjetivos a pesar de no serlo.
# También hay muchos adverbios.

# El color no determina la frecuencia (random.color = T), por lo que está relacionada con el tamaño de la palabra.
# Los adjetivos (reales) más frecuentes parecen ser: privado, nuevo o nueva. Cotejémoslo, por si acaso, 
# visualizando el dataset:

View(Adjetivos_Zara)

# Confirmamos que la proporción es la mencionada antes, aunque también que he pasado por alto alguna palabra importante
# ("bueno" y "tercer", por ejemplo) al intentar obtener conclusiones directamente a partir del gráfico.

#Analiza los verbos y representa un wordcloud como hemos hecho en el caso de los adjetivos
#PISTA: utiliza la condición de filtrado upos == "VERB"

Dataset_Zara_ud_verbs <- Dataset_Zara_ud %>% 
  filter(!str_detect(token, pattern = "#[:alnum:]")) %>%  # Aprovechamos para quitar los @ y los #, porque se dijo en clase
  filter(!str_detect(token, pattern = "@[:alnum:]"))      # que los hashtags y las menciones suelen ser clasificadas como 
View(Dataset_Zara_ud_verbs)                               # verbos y eso no nos interesa.

Verbos_Zara <- Dataset_Zara_ud_verbs %>%
  filter(upos == "VERB") %>%
  count(token) %>%
  arrange(desc(n))

wordcloud(
  words = Verbos_Zara$token, 
  freq = Verbos_Zara$n, 
  max.words = 80,
  min.freq = 5,
  scale =c(6,0.4),  # Es posible que, de nuevo, "zara" no entre en la gráfica, pero como no es un verbo no es importante
  random.order = T, # (que "zara" aparezca como verbo es una errata cometida por el algoritmo y por no haber limpiado más).
  rot.per = 0.3, random.color = T, 
  color = brewer.pal(4, "BrBG"))   

#* Parece que los verbos (reales) que más se repiten son: tengo, tiene, hacer, comprar, hay y quiero. Cotejésmolo:
#* 
View(Verbos_Zara) # En este caso hay bastantes verbos al mismo nivel de frecuencia como para acertarlos todos con un primer vistazo
                  #  (me he dejado "es" o "ver", por ejemplo), pero los que he comentado son bastante frecuentes.

#Nota: observa que "Zara" ha sido incorrectamente clasificado como Adjetivo y como Verbo.
#De la misma forma, otros tokens no fueron clasificados correctamente.
#En un caso real, sería necesario corregir estos defectos en la anotación del dataframe.

#* De acuerdo, yo lo había comentado antes de todas formas (no leí esto hasta que terminé de analizar adjetivos y verbos).

#PASO 6
#Realizamos un modelado de tópicos utilizando la librería topicmodels
#El objetivo es identificar temas en la conversación en Twitter sobre la marca Zara

#Para ello realizamos los siguientes pasos:
# - Seleccionamos nombres y adjetivos
# - Excluímos palabras muy frecuentes en los documentos pero sin significado relevante,
#   como el término de búsqueda de tuits "Zara" o palabras como "gracias", "por favor", etc.
# - Trabajamos con el id de documento (doc_id) y el lema (token lematizado)

#Nota: la libería topicmodels está construida utilizando objetos del paquete tm. Para poder ejecutar funciones
#de este paquete, debemos transformar en Document Term Matrix (dtm) utilizando la función cast_dtm()
Modelo_Zara <- Dataset_Zara_ud %>% 
  filter(upos %in% c("NOUN", "ADJ")) %>% 
  filter(!token %in% c("zara")) %>%
  select(id = doc_id, word = lemma) %>%
  mutate(id = str_replace_all(id, "doc", "")) %>% 
  count(word, id) %>% 
  cast_dtm(id, word, n) # Se está utilizando un topic_model.

#Generamos varios modelos, variando el número de temas definido en cada modelo (parámetro k)
#Utilizamos la función LDA() del paquete topicmodels
set.seed(1234)
Modelo_Zara_LDA <- LDA(Modelo_Zara, k = 3, control = list(seed = 1234)) # Si el corpus es grande, hasta un k = 8 puede ser bueno.
Modelo_Zara_LDA2 <- LDA(Modelo_Zara, k = 5, control = list(seed = 1234))
Modelo_Zara_LDA3 <- LDA(Modelo_Zara, k = 8, control = list(seed = 1234))

#Transformamos en formato tidy (tibble data.frame) utilizando la función tidy()
Zara_topics <- tidy(Modelo_Zara_LDA, matrix = "beta") 
Zara_topics2 <- tidy(Modelo_Zara_LDA2, matrix = "beta")
Zara_topics3 <- tidy(Modelo_Zara_LDA3, matrix = "beta")

#Inspecciona los dataframes. Puedes realizar una primera inspección ordenando de forma descendente utilizando la columa beta

Zara_topics_ord <- Zara_topics %>% 
  arrange(desc(beta))
View(Zara_topics_ord)

Zara_topics2_ord <- Zara_topics2 %>% 
  arrange(desc(beta))
head(Zara_topics2_ord)

Zara_topics3_ord <- Zara_topics3 %>% 
  arrange(desc(beta))
head(Zara_topics3_ord)

# Podemos observar las palabras con mayor beta de entre todos los tópicos al ordenar por beta descendente.

# *Nota. También podría haber clickado dos veces sobre la columna beta en las Views de cada dataframe sin
# hacer el arrange(), pero supongo que es preferible que muestre como se ordenaría mediante código en lugar
# de ordenar las columnas de esa manera ("a mano").

# Quizá sería más interesante mostrar solo las palabras de un tópico en cada visualización:

Zara_topics_ord <- Zara_topics %>%
  arrange(desc(beta)) %>% # Primero ordeno, para que los resultados se agrupen estando ya ordenados.
  group_by(topic) %>%     # Luego agrupo.
  summarise(term, beta) %>% 
  top_n(10, beta)         # He observado que hay muchos registros. Limito el número de registros por grupo con esta función.
View(Zara_topics_ord) # Observamos que hay demasiados registros en el dataframe como para poder inspeccionar las mayores
                      # betas de cada topico simplemente haciendo scrolling por el dataframe, así que le hago un top 10
                      # para que solo me muestre 10 de cada una. Ahora podemos visualizar más claramente las mayores betas
                      # para cada tópico.

# Repito la operación para k=5 y k=8:

Zara_topics2_ord <- Zara_topics2 %>%
  arrange(desc(beta)) %>% 
  group_by(topic) %>%     
  summarise(term, beta) %>% 
  top_n(10, beta)         
View(Zara_topics2_ord) 

Zara_topics3_ord <- Zara_topics3 %>%
  arrange(desc(beta)) %>% 
  group_by(topic) %>%     
  summarise(term, beta) %>% 
  top_n(10, beta)         
View(Zara_topics3_ord)     

#Seleccionamos los top terms de cada modelo y los visualizamos
## Modelo k=3
Zara_top_terms <- Zara_topics %>%  
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zara_top_terms_facet <- Zara_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

View(Zara_top_terms) # Esto es básicamente lo que he hecho yo antes para inspeccionar los dataframes.
                     # Vemos que el ggplot ha dibujado 3 gráficas (una para cada tópico), con las palabras en el eje
                     # de ordenadas y el valor de beta en el de abscisas. Esto nos da una representación visual de las 
                     # palabras con mayor beta.

Zara_top_terms_facet

## Modelo k=5
Zara_top_terms2 <- Zara_topics2 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zara_top_terms_facet2 <- Zara_top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

head(Zara_top_terms2)

head(Zara_top_terms_facet2) # Aquí he probado a ver la gráfica con head() ya que en realidad el comando implica pipes y un 
                            # mutate, así que pensé que lo mismo devolvía algo interesante, pero no es gran cosa.

View(Zara_top_terms_facet2) # Sin embargo, curiosamente, si le hacemos un View() observamos que aparece con formato de tabla
                            # desplegable (dejo esto aquí a modo de curiosidad, no creo que pueda sacar más información de
                            # ello, ya que no había visto este tipo de tablas desplegables antes).
Zara_top_terms_facet2

## Modelo k=8
Zara_top_terms3 <- Zara_topics3 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zara_top_terms_facet3 <- Zara_top_terms3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  scale_x_reordered() #reorder_within y scale_x_reordered permiten ordenar en cada facet (tema)

head(Zara_top_terms3)
Zara_top_terms_facet3 

#Seleccionamos el modelo k=5
#Intenta describir cada uno de los 5 temas identificados, revisando las palabras con mayor probabilidad
#(beta) de pertenecer a cada tema
#PISTA: ejecuta el objeto Zara_top_terms_facet2 para realizar un análisis visual

Zara_top_terms_facet2

# Tópico 1: parece tratar sobre ropa (vestido, bota, looks) y podría asociarse a la compra de ropa en Zara para las fiestas de Navidad (aunque
# el COVID perjudica a estos eventos seguro que hay gente que va a querer hacer lo que pueda durante estas fiestas). Con respecto al tema
# de fiestas se mencionan palabras como amigo, gente y fiesta.

# *Nota. No olvidemos que hemos filtrado Zara en los tópicos, es decir, damos por hecho que los tópicos están relacionados con Zara.

# Tópico 2: de nuevo, podemos intuir que el tema está relacionado con la ropa (berskha, jersey, colección, ...), aunque en este caso
# parece que preocupa el precio de la misma (euro, precio, Berskha, ...) y que se busca algo concreto (modelo, colección, ...). La palabra
# casa se repite mucho, pero es ambigua y podría referirse tanto a comprar desde casa como a comprar ropa para estar por casa.

# En conclusión yo diría que el tópico 2 está relacionado con comprar ropa de un determinado tipo y con un rango de precios propio de
# tiendas como Zara o Berskha (nótese que aquí se establece una relación entre ambas tiendas).

# Tópico 3: la palabra gracia es la más frecuente, lo que no nos dice mucho, pero sí que hay muchísimas palabras relacionadas
# con las compras online: pedido, compra, "enviano" (enviado), numero, servicio, mensaje, correo, detalle... Por lo que yo 
# diría que este tópico se refiere a las compras online en Zara, que son cada año más frecuentes en navidad y las semanas previas a 
# esta (a las que pertenecen los tweets analizados). 

# Tópico 4: este tópico parece relacionado con los regalos de navidad en general, ya que se mencionan temas muy dispares (trabajo,
# hijo, bolso, amiga, ...), pero todos ellos están asociados a probabilidades (beta) relativamente pequeñas, mientras que los de
# probabilidades más grandes son generales y hacen referencia a los regalos de navidad y año nuevo (dia, navidad, año, regalo, paquete, ...).
# Además, el tema se refuerza al hablar de Zara, ya que es una tienda que vende ropa para todas las edades, a un precio módico (adecuado
# para hacer regalos a amigos si no estás muy bien económicamente hablando) y a personas de ambos géneros.

# Tópico 5: este tópico es especialmente interesante si no conoces a Zara como empresa, porque hay que investigar en las noticias para 
# identificarlo. Resumiendo, el 8 de diciembre la exdirectora de Zara Home (que se puede asociar con la palabra "homir") fue contratada por
# el grupo Sargadelos (que también parece destacar por la elaboración de objetos para el hogar). En base a esto y a las fotos de las 
# noticias que podemos encontrar en Internet, donde dicha mujer aparece con un traje negro, podemos asociar la mayor parte de las palabras
# del tópico a dicha noticia: tienda (ambas empresas se basan en la venta de productos en tienda), verdad, empresa, "trave" (traje),
# negro, mujer, fin, sargadelo, trabajador, amancio (por Amancio Ortega), ... 

# Conclusión: el tópico 5 está asociado a la contratación de la exdirectora de Zara Home por parte del grupo Sargadelos.

# Por supuesto, todo esto son mis interpretaciones y no tienen por qué ser correctas. De hecho, dado que no hemos hecho una limpieza profunda
# de los datos, probablemente las interpretaciones realizadas puedan variar mucho con respecto al significado real de cada tópico.

#Finalmente, utilizando un wordcloud visualizamos las palabras más relevantes por tópico.
#Por ejemplo, para el tópico número 2
Zara_wordcloud <- Zara_topics2 %>%
  filter(topic == "2") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zara_wordcloud$term, 
  freq = Zara_wordcloud$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))

# Podemos utilizar este wordcloud para determinar las palabras que más se repiten en los 5 tópicos y hacer una limpieza más a fondo.
# Por ejemplo, a partir del gráfico, podemos ver que las palabras 2ªb, dio o tipo se repiten mucho en el tópico 2, a pesar de
# que en principio no son palabras que aporten mucho significado. 

# En un momento dado, otras palabras que podemos filtrar serían aquellas que aparecen en las gráficas de todos o casi todos los tópicos 
# (digamos que 3 de los 5 como mínimo) y se pueden asociar a la fecha o el contexto, por ejemplo, navidad o año. Esto sería viable porque
# en una fecha tan señalada como es navidad es normal que la mayor parte de los mensajes estén referidas a ella y porque, de hecho,
# vemos que el tema siempre esta entre las palabras más frecuentes (ya sea como navidad o como año y nuevo).

# Por último, también podríamos filtrar palabras que no se entienden, por ejemplo "gracia" en el tercer tópico.

# Veamos qué ocurre si filtramos algunas palabras con los criterios mencionados:

Modelo_Zara <- Dataset_Zara_ud %>% 
  filter(upos %in% c("NOUN", "ADJ")) %>% 
  filter(!token %in% c("zara", "gracia", "gracias","año", "2ªb", "navidad","hola", "mismo", # gracias lo filtro porque aparece si quito "gracia"
                       "talla", "vez", "dio","nivel","contar", "favor", "vio","vida")) %>%  # pero no "gracias".
  select(id = doc_id, word = lemma) %>%
  mutate(id = str_replace_all(id, "doc", "")) %>% 
  count(word, id) %>% 
  cast_dtm(id, word, n) # Se está utilizando un topic_model.

#Generamos varios modelos, variando el número de temas definido en cada modelo (parámetro k)
#Utilizamos la función LDA() del paquete topicmodels

set.seed(1234) # Reiniciamos la semilla, llamándola de nuevo.
Modelo_Zara_LDA2 <- LDA(Modelo_Zara, k = 5, control = list(seed = 1234))

#Transformamos en formato tidy (tibble data.frame) utilizando la función tidy()
Zara_topics2 <- tidy(Modelo_Zara_LDA2, matrix = "beta")

Zara_top_terms2 <- Zara_topics2 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zara_top_terms_facet2 <- Zara_top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

Zara_top_terms_facet2

# Podemos observar que pierdo los tópicos, esto es, ya no se parecen tanto a los originales y se han desestructurado
# (por ejemplo, el 5 los perdemos y salgadelo aparece en otro tópico ahora). Por lo tanto, habría que volver a analizar
# los 5 tópicos y lo que hemos ganado con respecto al caso anterior debería ser unos tópicos más fáciles de entender
# y más fiables. Comprobémoslo:

#* Tópico 1: por un lado aparecen palabras relacionadas con regalar ropa (concretamente destaca la palabra vestido) y por otro
#* podemos observar palabras que se pueden asociar a una cena de empresa (muy normal en estas fechas): empresa, fiesta, trabajo, detalle,
#* gente... Así que podríamos resumirlo como "ropa como regalo/detalle en/para una cena de empresa", pero no lo veo claro.

#* Tópico 2: este parece una mezcla de la noticia mencionada anteriormente (cardena debe hacer referencia al apellido de la exdirectora de
#* Zara Home, Eva Cárdenas) por las palabras "cardena", "sargadelo", "verdad", "atención", ... Pero está combinada con otras con mayor 
#* probabilidad que no estoy seguro de cómo encajar en ese mismo contexto: vestido, compra, tienda, ropa, ... Quizá puede hacer referencia
#* al debate sobre si la decisión de dicha mujer fue acertada, de tal forma que las últimas palabras mencionadas se refieran a temas por
#* los que podríamos comparar ambas empresas (ventas, el tipo de producto, ...) o al atuendo que llevó en algún momento tras anunciarse
#* su contratación en el grupo Sargadelos (ya que en las noticias que he visto aparece con un traje, no con un vestido).

#* Tópico 3: en este caso se habla de ropa (traje, ropa, moda, prenda, pantalón), tiendas (tienda, Berskha, ...), dinero (dinero, euro)
#* y fiestas (fiesta, día, calle...). En consecuencia, podemos intuir que trata el tema de "conseguir prendas a buen precio para celebraciones
#* de navidad" (digo buen precio tanto por la parte de dinero como porque Zara y Berskha no son tiendas de ropa caras).

#* Tópico 4: este tema trata temas un poco difusos: se habla de ropa, pero también de complementos (ropa, prenda, bolso, ...) y, en
#* principio parece que se quiere que sean de una forma determinada (marca, modelo, punto, foto, ...). El resto de palabras no son 
#* fácilmente asociables al tema anterior, más allá de las típicas que hacen referencia a estas fechas (navidad). Yo resumiría el tema
#* diciendo que trata de la compra de ropa de un tipo determinado (metiendo "bolso" dentro de ropa), pero es arriesgado, ya que el 
#* hay muchas palabras sin conexión aparente.

#* Tópico 5: hay muchísimas palabras relacionadas con las compras online: pedido, compra, "enviano" (enviado), cuenta, mensaje, correo,
#* ... Por lo que yo diría que este tópico se refiere a las compras online en Zara, que son cada año más frecuentes en navidad y las 
#* semanas previas a esta festividad (a las que pertenecen los tweets analizados). En concreto, parece que está enfocada a ropa de marca
#* (marca, ropa, colección, vestido, ...), que es algo que se asocia fácilmente con Zara, así que tiene sentido.

#* Nota: por algún motivo, hay algunas palabras que filtra bien (como "hola") y otras que no (como "año") y que, por lo tanto,
#* siguen apareciendo.

#* Tengo que comentar que este segundo análisis (tras filtrar) me ha resultado más complicado y ambiguo que el primero y creo que
#* ha sido porque, filtrar no te asegura que los tópicos vayan a ser más claros (solo es más probable que así sea) y, en este caso,
#* quizá debería haber descartado este análisis y haber ejecutado el LDA un par de veces más para ver si obtenía tópicos más fáciles
#* de describir (no lo he hecho porque creo que podría dar problemas de reproducibilidad a la hora de evaluar este script, pero quería
#* dejar constancia de este razonamiento).
