#Realizaremos un análisis de la canasta de los usuarios para comprender qué productos son más propensos a comprarse de forma conjunta.
#Es necesario realizar modelos de reglas de asociación que permita detectar aquellos productos que se compran en conjunto.
#El denominado Association Rule Analysis se enfoca en detectar patrones en los cuales
#el ratio de ocurrencia de 2 eventos es mayor a la frecuencia de aparicion general de uno
#de ellos en toda la data. Por ejemplo si pan tiene una ocurrencia del 5% en los tickects
#pero aparece un 80% de las veces que se compra nutella, entonces podemos inferir algun tipo
#de patrón de compra a explotar.


#Para calcularlo utilizamos una librería que se encarga de hacer los calculos por nosotros, denominada arules

install.packages("arules")
install.packages("arulesViz")

library(arules)
library(readr)
library(arulesViz)

#La libreria arules incluye un dataset para trabajar, que es el que vamos a utilizar de ejemplo
#cargamos el dataset y lo inspeccionamos con summary
data("Groceries")
summary(Groceries)

#Podemos realizar tablas de frecuencias para hacernos una idea de los productos más comprados
FreqProductos <- data.frame(Producto = names(itemFrequency(Groceries)), 
                            Frecuencia = itemFrequency(Groceries), 
                            row.names=NULL, stringsAsFactors = F)
FreqProductos <- FreqProductos[order(FreqProductos$Frecuencia, decreasing = T),]

#verlos en una gráfica
itemFrequencyPlot(Groceries, topN = 20, type = "absolute")

#luego simplemente aplico el algoritmo apriori que busca posibles reglas de asociacion
#de cada uno de los productos para con el resto de los productos
#esas reglas de asociacion estaran condicionadas por cierto nivel de support y confianza

#Por ejemplo probemos una confianza de 0.4 y un support de 1% de todas las compras
basket_analysis <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.4, target = "rules"))

#Entendamos los resultados principales del algoritmo
#A
#"sorting and recoding items" son los productos que componen las regla de los 169 en el dataset
#En casos reales si este numero es muy pequeño o muy cercano al volumen total de productos
#es necesario ajustar los valores de confianza y support

#B
#"writing ..." son las reglas de asociacion que ha podido generar el algoritmo
#si el volumen de reglas es mayor que el volumen total de productos, es otro indicador
#de que se deben ajustar los valores de confianza y support.


#3
#Podemos inspeccionar las reglas utilizando la siguiente funcion
inspect(basket_analysis)
#puedo utilizar subset para inspeccionar reglas con condiciones específicas
inspect(subset(basket_analysis, confidence > 0.5)) #señalar el nivel de confianza
inspect(subset(basket_analysis, support > 0.02)) #señalar el nivel de support
inspect(subset(basket_analysis, lift > 3)) #señala el nivel de lift

#también podemos ordenar las reglas con sort
basket_analysis <- sort(basket_analysis, by = "confidence", decreasing = T) # orden desc 
inspect(head(basket_analysis,5)) #utilizar head
inspect(tail(basket_analysis,5)) #o tail



#4
#La logica para entender las reglas es la siguiente:
#lhs significa left hand sides / rhs significa right hand sides
#lhs viene a ser el(los) producto(s) que causa(n) la compra de rhs
#por lo tanto rhs siempre será el producto consecuente de compra el que se encuentra en 
#la columna lhs (que se denomina formalmente antecedente)

#Inspeccionemos las reglas con una confianza mayor a 0.5
inspect(subset(basket_analysis, confidence > 0.5))
#viendo la primera fila, en el 1% (support) de los casos en que se compra curd y yogurt (lhs), 
#suele comprarse whole milk (aparece en la columna rhs)
#y esta compra conjunta suele co-ocurrir 2,2 veces (lift) mas que de forma individual.

#Podemos nuevamente utilizar subset pero en las reglas para inspeccionar aquellas
#que pueden llegar a ser relevantes para nuestro análisis
#por ejemplo en casos reales donde obtengo la scanner data de los supermercados
#y solo me interesa comprender qué productos se compran conjuntamente con los de mi marca
itemsets_filtrado <- arules::subset(basket_analysis,
                                    subset = items %in% "rolls/buns")
inspect(itemsets_filtrado)


#Finalmente puedo analizar la redundancia de las reglas, es decir, si existen
#reglas que contengan los mismos productos en el lhs y rhs pero su soporte es mayor
reglas_redundantes <- basket_analysis[is.redundant(x = basket_analysis, measure = "confidence")]
reglas_redundantes #vemos que el resultados es "0 rules" lo que implica que no hay redundantes


#5
#Visualizacion con scatter para ver dispersión
plot(basket_analysis) #vemos que la mayoría tiene un support menor a 0.02

#graficamos todas las reglas
plot(basket_analysis, method = "graph", control = list(type = "items"))
#El grafico se interpreta de la siguiente manera:
#los circulos representan el valor de support y su tamaño depende de dicho valor
#el color representa el lift, cuanto mayor valor de lift mas oscuro
#Lo ideal es realizar diferentes plots con subset de las reglas para ir analizando las relaciones
#y obteniendo conclusiones utiles

#graficamos las reglas más relevantes
plot(head(basket_analysis, 30), method = "graph", control = list(type = "items"))


#También podemos graficar una matrix 
#con las reglas para enteder cómo se relacionan
plot(basket_analysis, method = "grouped")



