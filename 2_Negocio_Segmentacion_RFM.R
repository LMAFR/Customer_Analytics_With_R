#Ahora utilizaremos los datos calculados de RFM para realizar un modelo que permita detectar los grupos de compradores
#de acuerdo a sus comportamientos de compra:

install.packages("caret")

library(readxl)
library(dplyr)
library(caret)

#VAMOS A REALIZAR UNA SEGMENTACIÓN DE LOS USUARIOS ASUMIENDO LAS SIGUIENTES CONDICIONES:
#Los usuarios se clasifican de acuerdo a los quantiles en los que se encuentran a nivel de recency y frequency
#El scoring se realiza de 1 a 5, siendo 1 quantil 0.2, 2 es cuantil 0.4, 3 es cuantil 0.6, 4 es cuantil 0.8 y 5 es quantil 1


#Realizamos el scoring agregando las columnas a nuestro dataset general 
Calculo_RFM$ScoreRecency <- 0
Calculo_RFM$ScoreRecency[Calculo_RFM$Recency <= quantile(Calculo_RFM$Recency,0.20)]<-1
Calculo_RFM$ScoreRecency[Calculo_RFM$Recency <= quantile(Calculo_RFM$Recency,0.40) & Calculo_RFM$Recency >= quantile(Calculo_RFM$Recency,0.20)]<-2
Calculo_RFM$ScoreRecency[Calculo_RFM$Recency <= quantile(Calculo_RFM$Recency,0.60) & Calculo_RFM$Recency >= quantile(Calculo_RFM$Recency,0.40)]<-3
Calculo_RFM$ScoreRecency[Calculo_RFM$Recency <= quantile(Calculo_RFM$Recency,0.80) & Calculo_RFM$Recency >= quantile(Calculo_RFM$Recency,0.60)]<-4
Calculo_RFM$ScoreRecency[Calculo_RFM$Recency >= quantile(Calculo_RFM$Recency,0.80)]<-5

Calculo_RFM$ScoreFrequency <- 0
Calculo_RFM$ScoreFrequency[Calculo_RFM$Frequency <= quantile(Calculo_RFM$Frequency,0.20)]<-1
Calculo_RFM$ScoreFrequency[Calculo_RFM$Frequency <= quantile(Calculo_RFM$Frequency,0.40) & Calculo_RFM$Frequency >= quantile(Calculo_RFM$Frequency,0.20)]<-2
Calculo_RFM$ScoreFrequency[Calculo_RFM$Frequency <= quantile(Calculo_RFM$Frequency,0.60) & Calculo_RFM$Frequency >= quantile(Calculo_RFM$Frequency,0.40)]<-3
Calculo_RFM$ScoreFrequency[Calculo_RFM$Frequency <= quantile(Calculo_RFM$Frequency,0.80) & Calculo_RFM$Frequency >= quantile(Calculo_RFM$Frequency,0.60)]<-4
Calculo_RFM$ScoreFrequency[Calculo_RFM$Frequency >= quantile(Calculo_RFM$Frequency,0.80)]<-5

Calculo_RFM$ScoreMV <- 0
Calculo_RFM$ScoreMV[Calculo_RFM$Monetary_Value <= quantile(Calculo_RFM$Monetary_Value,0.20)]<-1
Calculo_RFM$ScoreMV[Calculo_RFM$Monetary_Value <= quantile(Calculo_RFM$Monetary_Value,0.40) & Calculo_RFM$Monetary_Value >= quantile(Calculo_RFM$Monetary_Value,0.20)]<-2
Calculo_RFM$ScoreMV[Calculo_RFM$Monetary_Value <= quantile(Calculo_RFM$Monetary_Value,0.60) & Calculo_RFM$Monetary_Value >= quantile(Calculo_RFM$Monetary_Value,0.40)]<-3
Calculo_RFM$ScoreMV[Calculo_RFM$Monetary_Value <= quantile(Calculo_RFM$Monetary_Value,0.80) & Calculo_RFM$Monetary_Value >= quantile(Calculo_RFM$Monetary_Value,0.60)]<-4
Calculo_RFM$ScoreMV[Calculo_RFM$Monetary_Value >= quantile(Calculo_RFM$Monetary_Value,0.80)]<-5

#Scoring final sumando los anteriores
Calculo_RFM$ScoreRFM <- rowSums (Calculo_RFM[ , 5:7])


#Asignamos un segmento de consumidor de acuerdo a su score
Calculo_RFM$segment[which(Calculo_RFM$ScoreRFM < 4)] = "inactivo"
Calculo_RFM$segment[which(Calculo_RFM$ScoreRFM >= 4 & Calculo_RFM$ScoreRFM < 7)] = "en riesgo"
Calculo_RFM$segment[which(Calculo_RFM$ScoreRFM >= 7 & Calculo_RFM$ScoreRFM < 10)] = "potenciales"
Calculo_RFM$segment[which(Calculo_RFM$ScoreRFM >= 10 & Calculo_RFM$ScoreRFM < 13)] = "leales"
Calculo_RFM$segment[which(Calculo_RFM$ScoreRFM >= 13)] = "champions"

View(Calculo_RFM)

#### Consultas
#¿Cuántos consumidores existen para cada segmento identificado?
#¿Cuál es el porcentaje de usuarios champions? Pista: utilizamos prop.table para obtener las frecuencias relativas

#Practiquen RFM con los consumidores del 2019 y con los de 2018

