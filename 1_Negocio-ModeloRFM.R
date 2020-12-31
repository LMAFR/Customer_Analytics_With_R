#Métricas de Negocio
library(readxl)
library(dplyr)

# Cargamos el archivo excel dataset_RFM
dataset_RFM <- read_excel(file.choose())


##### EXPLORAR DATA PARA COMPRENDER SUS CARACTERÍSTICAS #####
head(dataset_RFM)
summary(dataset_RFM)


# Modificamos naming columnas para que tengan sentido
colnames(dataset_RFM) = c('customer_id', 'gross_compra', 'fecha_compra')

# Ajustamos fecha y generamos una nueva variable solo con año para facilitar análisis
dataset_RFM$fecha_compra <- as.Date(dataset_RFM$fecha_compra, "%Y-%m-%d")
dataset_RFM$año <- as.numeric(format(dataset_RFM$fecha_compra, "%Y"))

#Exploramos nuevamente para ver los cambios
head(dataset_RFM)
summary(dataset_RFM)
table(dataset_RFM$año)
prop.table(table(dataset_RFM$año))

#Si quiero realizar comparativas de los datos más relevantes estre diferents factores, puedo utilizar la función by
#en conjunción con summary para obtener una inspección más profunda de los datos
by(dataset_RFM, dataset_RFM$año, FUN = summary)



##### CALCULAMOS RFM #####
Calculo_RFM <- dataset_RFM %>% 
  group_by(customer_id) %>%
  summarise(Recency=as.numeric(as.Date(Sys.Date())-max(fecha_compra)),
            Frequency=length(customer_id), # Aquí length está contando el número de veces que se repite cada customer_id.
            Monetary_Value= sum(gross_compra)) 

View(Calculo_RFM)

##### CALCULAMOS RFM FILTRANDO LAS COMPRAS DESDE 2019 #####
Calculo_RFM2019 <- dataset_RFM %>% 
  group_by(customer_id) %>%
  filter(fecha_compra > "2019-01-01") %>%
summarise(Recency=as.numeric(as.Date(Sys.Date())-max(fecha_compra)), # Sys.Date() devuelve la fecha actual
          Frequency=length(customer_id), 
          Monetary_Value= sum(gross_compra)) 

View(Calculo_RFM2019)



##### APLICAMOS ESTADÍSTICA DESCRIPTIVA PARA COMPRENDER EL RESULTADO #####
range(Calculo_RFM$Monetary_Value)
mean(Calculo_RFM$Frequency)
mean(Calculo_RFM$Monetary_Value)
mean(Calculo_RFM$Recency)

median(Calculo_RFM$Frequency)
median(Calculo_RFM$Monetary_Value)
median(Calculo_RFM$Recency)

quantile(Calculo_RFM$Frequency,0.70)
quantile(Calculo_RFM$Frequency,0.90)

quantile(Calculo_RFM$Recency,0.70)
quantile(Calculo_RFM$Recency,0.90)


#Por ejemplo podría filtrar para generar un dataframe que solo tuvieron los usuarios que considero más valiosos
#porque son ese 10% que compra con más frecuencia
#Utilizando la función de filtro, le pido a R que me genere el nuevo dataframe donde Frequency 
#sea mayor al valor del quantile 90

Calculo_RFM_P90 <- Calculo_RFM %>%
  filter(Frequency > quantile(Calculo_RFM$Frequency,0.90))


#O mayor al quantil 75
Calculo_RFM_P75 <- Calculo_RFM %>%
  filter(Frequency > quantile(Calculo_RFM$Frequency,0.75))




##### EJERCICIO
#Utilizando la función de filtrado de Dplyr, generar un nuevo dataset que contenga solo los datos de 2018
#Pista: necesito indicar que el año == 2018 
#Calcular nuevamente las columnas de RFM para ese año en específico.

a <- Calculo_RFM %>% 
  filter(año == 2018)

##### EJERCICIO CHURN RATE
#Calcular el churn rate de nuestro cliente para el año 2019, contando que la fórmula es número total de usuarios al inicio - numero total al final
#dividido volumen de usuarios al final del período
#¿Cuál ha sido el churn rate de 2019?

