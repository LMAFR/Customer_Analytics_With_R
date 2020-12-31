#Price Optimization es un analisis recurrente dentro de los equipos de Marketing
#No solo para poder establecer el coste de un nuevo producto al ingresar en una categoria
#Sino tambien en el mundo actual de E-commerce donde se puede probar de forma dinamica
#diferentes precios y promociones para comprender cuanto dinero un comprador esta dispuesto a pagar
#Trabajaremos con un ejemplo de un e-commerce que estuvo mostrando diferentes precios a los visitantes
#de un producto espec?fico y contamos con la fecha de compra
  
library(tidyverse) #transformar y visualizar la data
library(stargazer) #permite visualizar de forma facil los resultados de un modelo de regresion

#cargamos los datos, asegurando que detectDates sea T para que cargue las fechas correctamente
PriceOptimization <- read.csv("PriceOptimization_data.csv")

head(PriceOptimization)
summary(PriceOptimization)
table(PriceOptimization$precios)

#Analizamos la distribución de precios en las fechas analizadas

library(ggplot2)

ggplot(PriceOptimization, aes(fecha_compra, precios)) +
  geom_line() +
  ggtitle('Historico de precios')

#primero crearemos la funcion lineal de la demanda y cargaremos en el environment
#suponiendo una pendiente de -30 y una muestra de 1500
demanda <- function(p, alpha = -30, beta = 1500, sd = 10) {
  #la desviacion estandar -sd- permite cuantificar la dispersion de los datos en la misma unidad y otorga fiabilidad estadistica
  error = rnorm(length(p), sd = sd) #calculamos el error de la estimación
  q = alpha*p + beta + error #a nuestra ecuación lineal de la demanda le sumamos el calculo del error
  
  return(q)
}

#Generar un nuevo data frame resumiendo los datos para cada precio
#de esa forma obtenemos la cantidad de ventas que hubo para cada día
#haremos sencillamente una selección de fecha_compra única para simplificar el proceso
(PriceOptimization2 <- PriceOptimization %>%
  select(fecha_compra,precios) %>%
  filter(!duplicated(fecha_compra)) %>%
  mutate(fecha_compra = as.Date(fecha_compra)))

#apliquemos la función lineal de la demanda
PriceOptimization2$demanda <- NA
(PriceOptimization2$demanda <- demanda(PriceOptimization2$precios))

#visualizamos la demanda como una regresión lineal
ggplot(PriceOptimization2, aes(precios, demanda)) +
  geom_point(shape=1) +
  geom_smooth(method='lm') +
  ggtitle('Curva demanda')

#analizamos la correlación entre precio y demanda
#correlación: la fuerza de la relación lineal entre las variables
cor(PriceOptimization2$demanda, PriceOptimization2$precios)
#vemos que la correlación es engativa porque mientras una aumenta la otra desciende
#cumple la condición de la regresión lineal

    
###### MODELAMOS Y GRAFICAMOS LAS ESTIMACIONES DE PRECIO OPTIMO
##### Y SU CONSECUENTE REVENUE PARA PODER RECOMENDAR AL EQUIPO DE MARKETING

#1, modelamos la regresión lineal de la demanda con los datos historicos
#para ello primero aplicamos el calculo de la demanda a los datos de precios
curva_demanda <- lm(PriceOptimization2$demanda ~ PriceOptimization2$precios) #modelo regresión lineal
#lectura del lm: entender variacion de demanda por cada precio
summary(curva_demanda) #podemos aplicar para ver resultado

#Podemos leer en la consola el resultado de una forma más amigable con stargazer
stargazer(curva_demanda, type = 'text')
# R-2 viene a ser el R-cuadrado y tiene un valor de 91,5%, esto a nivel de negocios está genial (por 
# encima del 35% se considera la propuesta). Mi ecuacion estimada de demanda podría ser ahora
# Q = -29.958*p + 1498.422


#A partir de los resultados, nos interesan tomar los coeficientes que representan alpha y beta
(beta <- curva_demanda$coefficients[1]) #es el beta de la ecuacion de demanda, pero estimado a 1.517,04 
alpha <- curva_demanda$coefficients[2] #es el alpha de la ecuacion de la demanda, pero estimado a -30,75

# Los datos anteriores están almacenados en la variable del ajuste (curva_demanda) y se llaman desde ahí.

#Para entender el revenue
#Sabemos que el costo por unidad es de 16,57 y lo agregamos al dataframe
costo_unidad <- 16.57
(PriceOptimization2$Costo_unidad <- costo_unidad)

#Con esto vamos a calcular varios datos: coste total, revenue y profit para cada tipo de precio
PriceOptimization2$Revenue <- PriceOptimization2$precios * PriceOptimization2$demanda
PriceOptimization2$Costo_total <- PriceOptimization2$Costo_unidad * PriceOptimization2$demanda
PriceOptimization2$Profit <- PriceOptimization2$Revenue - PriceOptimization2$Costo_total


#Examinamos los datos para entender el revenue y el costo total de cada día
ggplot(PriceOptimization2) +
  geom_line(aes(fecha_compra, Revenue, color = 'Revenue')) +
  geom_line(aes(fecha_compra, Profit, color = 'Profit')) +
  geom_line(aes(fecha_compra, Costo_total, color = 'Costo'))

# Si vemos que costo está muy cerca de revenue, hay que ir a hablar con el equipo para que reconsideren el precio,
# porque estando tan cerca ambas variables es probable que no estén ganando todo lo que pudieran e incluso que, por
# el error, se produzcan perdidas en vez de beneficios en algún momento.

#estimamos el precio y el revenue entendiendo la demanda como funcion lineal
#de esta forma necesitamos
p_revenue <- -beta/(2*alpha) 
p_profit <- (alpha*costo_unidad - beta)/(2*alpha) #indicar el max profit posible *(en $/unidad)*.

# En las siguientes líneas va a modificar el valor de alpha (asume alpha =-40 en vez de -30).

#necesitamos generar funciones para obtener datos reales y estimados a partir de la función de demanda
revenue_real <- function(p) p*(-40*p + 1500) # asumiendo que la demanda ha sido 12500, nuestro dataset
profit_real <- function(p) (p - costo_unidad)*(-40*p + 1500) # calculado en base a datos del dataset
revenue_estimado <- function(p) p*(curva_demanda$coefficients[2]*p + curva_demanda$coefficients[1])
profit_estimado <- function(p) (p - costo_unidad)*(curva_demanda$coefficients[2]*p + curva_demanda$coefficients[1])

revenue_optimo <- revenue_real(p_revenue) # Revenue con el precio optimo estimado
profit_optimo <- profit_real(p_profit) # Profit con el precio optimo estimado


#preparamos el gráfico con los datos de los 4 valores para comparar
ggplot(data = data.frame(Precio = 0)) +
  stat_function(fun = revenue_real, mapping = aes(x = Precio, color = "Revenue Real")) +
  stat_function(fun = profit_real, mapping = aes(x = Precio, color = "Profit Real")) +
  stat_function(fun = revenue_estimado, mapping = aes(x = Precio, color = "Revenue Estimado")) +
  stat_function(fun = profit_estimado, mapping = aes(x = Precio, color = "Profit Estimado")) +
  ylab('Resultados') +
  scale_color_manual(name = "", values = c("Revenue Real" = 2, "Profit Real" = 3, "Revenue Estimado" = 4, "Profit Estimado" = 5)) +
  geom_segment(aes(x = p_revenue, y = revenue_optimo, xend = p_revenue, yend = 0)) +
  geom_segment(aes(x = p_profit, y = profit_optimo, xend = p_profit, yend = 0))

  # Las rectas verticales son el rango de precios en los que nos tenemos que mover. 
  # Se estiman en base a las curvas reales, pero usando los coeficientes estimados.
