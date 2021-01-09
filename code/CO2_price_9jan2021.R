#Ejercicio 1: análisis de la serie temporal precios de CO2 en mercado regulado de carbono 2008-2020

#Paso 1: Importación de datos
library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
prices <- read_csv2("C:/Users/Rose/Documents/ESTADISTICA/Proyectos/CO2_price/dataset/historico.csv")

#Paso 2: Visualización de datos
#View: muestra los datos en una matriz
View(prices)
#dim: muestra el número de columnas y filas
dim(prices)
#names: muestra las variables de "prices"
names(prices)
#head: muestra las primeras 6 filas
head(prices)
#tail: muestra las últimas 6 filas
tail(prices)

#Paso 3: Estructura de datos
#str: muestra la estructura de los datos, incluyendo matrices, columnas y filas
str(prices)
#class: indica el tipo de dato. Recibe como argumento un dato o vector
#y devuelve el nombre del tipo al que pertenece en inglés
class(prices$Fecha)
#con la función dmy (día,mes,año) de la libreria lubridate puedo cambiar el formato 
prices$Fecha <- dmy(prices$Fecha)
class(prices$Fecha)

################################################################
  
#Paso 4: Declarar como serie temporal
#Si queremos que R trate a un objeto como serie temporal, tenemos que determinar
#apropiadamente sus características con la función "ts"
#El argumento frequency se utiliza para indicar la periodicidad de la serie 
#(en este caso mensual), mientras que el argumento start indica la fecha de la primera observación (enero de 2012).
seriet <- ts(prices$CER,start = c(2008,1),end = c(2020,6),frequency = 12)
print(seriet)
jpeg("seriet.jpeg")
plot(seriet)
dev.off()

jpeg("seriet_darkcyan.jpeg")
plot(seriet,col="darkcyan", main="Precios de certificados de reducción de emisiones (CER) en 
     mercado regulado período 2008-2020",xlab="Tiempo (años)",ylab="Precio por tonelada de CO2 (???/tCO2e)")
grid()
dev.off()

#Paso 5: Inspeccionar la frecuencia mediante el periodograma
---------
#previo
library(leaps)
library(locfit)
install.packages(file.choose(), repos=NULL)

###listo
library(TSA)
jpeg("periodograma.jpeg")
per_seriet <- periodogram(seriet) #Para generar el periodograma
dev.off()

per_seriet$spec #Para ver los datos del periodograma
which.max(per_seriet$spec) #Encontrar la posición del espectro más alto
frec_seriet = 1/per_seriet$freq[which.max(per_seriet$spec)] #Encontrar el periodo del espectro más grande

plot(seriet,col="darkcyan", main="Precios de certificados de reducción de emisiones (CER) en 
     mercado regulado período 2008-2020",xlab="Tiempo (años)",ylab="Precio por tonelada de CO2 (???/tCO2e)")
grid()

#Obtuve que 5.1724 es la frecuencia más relevante
seriet.2 <- ts(prices$CER,start = c(2008,1),end = c(2020,6),frequency = 5)
print(seriet.2)
jpeg("seriet.2.jpeg")
plot(seriet.2)
dev.off()

jpeg("seriet.2_darkcyan.jpeg")
plot(seriet.2,col="darkcyan", main="Precios de certificados de reducción de emisiones (CER) en 
     mercado regulado período 2008-2020",xlab="Tiempo (años)",ylab="Precio por tonelada de CO2 (???/tCO2e)")
grid()
dev.off()

#Paso 6: Hacer la descomposición para ver el tipo de serie de datos
library(stats)

#para la serie con frecuencia = 12
seriet.desc = decompose(seriet)
jpeg("decompose_seriet.jpeg")
plot(seriet.desc, col= "darkcyan", xlab = "Tiempo (años)")
dev.off()

#para la serie con frecuencia = 5
seriet.desc = decompose(seriet.2)
jpeg("decompose_seriet.2.jpeg")
plot(seriet.desc, col= "darkcyan", xlab = "Tiempo (años)")
dev.off()

#Paso 7: Hacer el forecast
library(forecast)

#para la serie con frecuencia = 12
modelo_arima <- auto.arima(seriet,stepwise = FALSE,approximation = FALSE,trace = TRUE)
jpeg("residuos_seriet.jpeg")
checkresiduals(modelo_arima) #Chequear la dispersion de los residuos
dev.off()

fsct <- forecast(modelo_arima,h=12,level = 95)
jpeg("forecast_seriet.jpeg")
autoplot(fsct,main = "Forecast: Precios de certificados de reducción de emisiones (CER) en mercado regulado período 2008-2020)", xlab="Tiempo (años)", ylab="Precio por tonelada de CO2 (???/tCO2e)")
dev.off()

jpeg("forecast_seriet2.jpeg")
autoplot(seriet, series ="Data")+
  autolayer(fsct, series = "Forecast")+
  autolayer(fitted(fsct), series = "Fitted")
dev.off()

#para la serie con frecuencia = 5
modelo_arima <- auto.arima(seriet.2,stepwise = FALSE,approximation = FALSE,trace = TRUE)
jpeg("residuos_seriet.2.jpeg")
checkresiduals(modelo_arima) #Chequear la dispersion de los residuos
dev.off()

fsct <- forecast(modelo_arima,h=12,level = 80)
jpeg("forecast_seriet.2.jpeg")
autoplot(fsct,main = "Forecast: Precios de certificados de reducción de emisiones (CER) en mercado regulado período 2008-2020)", xlab="Tiempo (años)", ylab="Precio por tonelada de CO2 (???/tCO2e)")
dev.off()

#################################################3

#GRAFICOS COMPLEMENTARIOS
#Gráfica estacional
#Los datos se grafican contra las estaciones individuales en las que se observaron
library(forecast)
ggseasonplot(seriet.2)

ggseasonplot(seriet.2, year.labels = FALSE, continuous = TRUE)+
  ggtitle("Precios de certificados de reducción de emisiones (CER) en mercado regulado período 2008-2020)")

jpeg("gráfico estacional_seriet.2.jpeg",
     width=1200,height=820,units="px",
     pointsize=9,bg="white",res=125)
ggseasonplot(seriet.2)
g1 = ggseasonplot(seriet.2)
g1 + labs(title="Gráfico estacional: Precios de certificados de reducción de emisiones (CER) en mercado regulado período 2008-2020)",
          subtitle="Datos vrs.estaciones de observación",
          caption="fuente:Sistema Europeo de Negociación de CO2",x="Mes",
          y="Precio por tonelada de CO2 (???/tCO2e)")
dev.off()

#Gráfica de coordenadas polares
#El eje de la serie temporal es circular en lugar de horizontal

ggseasonplot(seriet.2, year.labels=FALSE, continuous=TRUE, polar = TRUE)+ 
  ggtitle("Coordenadas polares: Precios de certificados de reducción de emisiones (CER) en 
          mercado regulado período 2008-2020)")

jpeg("coordenadas polares_seriet.2.jpeg",
     width=1200,height=820,units="px",
     pointsize=9,bg="white",res=125)
ggseasonplot(seriet.2, polar = TRUE)
dev.off()

#Gráfico alternativo
#Enfatiza los patrones temporales, los datos para cada temporada se recopilan 
#juntos en minigráficas de t separadas. Las lineas horizontales indican las 
#medias para cada trimestre y mes. Este diagrama muestra el patrón estacional
#subyacente y también muestra los cambios en la estacionalidad a lo largo del
#del tiempo
#util para ver cambios estacionales a lo largo del tiempo
ggsubseriesplot(seriet)

#Parcelas de retraso
jpeg("parcelas retraso_seriet.jpeg")
gglagplot(seriet.2) + ggtitle("Lag plots: precios de créditos de carbono en mercado regulado (???)")
dev.off()

#Definir la unidad de tiempo a la que pertence cada unidad
#Si queremos comparar la distribución de precios de carbono por cada mes usamos boxplot
jpeg("boxplot_seriet.jpeg")
boxplot(seriet ~ cycle(seriet), main="Precios de certificados de reducción de emisiones (CER) en mercado regulado período 2008-2020", col="cyan3", xlab="Meses",
        ylab="Precio por tonelada de CO2 (???/tCO2e)")
grid()
dev.off()

jpeg("boxplot_seriet.2.jpeg")
boxplot(seriet.2 ~ cycle(seriet.2), main="Precios de certificados de reducción de emisiones (CER) en mercado regulado período 2008-2020", col="cyan3", xlab="Meses",
        ylab="Precio por tonelada de CO2 (???/tCO2e)")
grid()
dev.off()


#Comando cycle determina la unidad de tiempo a la que pertenece c/unidad
cycle(seriet.2)






