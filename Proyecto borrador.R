## Bibliotecas
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gridExtra)
library(grid)
library(tidyr)





Datos_del_proyecto <- read.csv("liberia_datos_climaticos.csv",
                               sep = ","
                               ,na.strings = "",
                               dec = ",")

Datos_del_proyecto <- na.omit (Datos_del_proyecto)

Datos_del_proyecto <-
  Datos_del_proyecto %>%
  rename(HumeRela = "HumedadRelativa....",
         velo_viento = "VelocidadViento..m.s.",
         lluvia = "Lluvia..mm.",
         irradiac = "Irradiacion..W.m2.",
         evapotran = "EvapoTranspiracion..mm.",
         temp_celcius = "Temperatura..Celsius.",
         fecha = "Date"
  )
Datos_del_proyecto <-
  Datos_del_proyecto %>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))

## Graficación de Histogramas


Uno <- ggplot(Datos_del_proyecto, aes(x= temp_celcius, group = 1))+geom_histogram(col = "#66CC99",fill = "#99FFCC")
Dos <- ggplot(Datos_del_proyecto, aes(x= velo_viento, group = 1))+geom_histogram(col = "#330033",fill = "#660066")
Tres <- ggplot(Datos_del_proyecto, aes(x= evapotran, group = 1))+geom_histogram(col = "#FFFF00", fill = "#FFFF99")
Cuatro <- ggplot(Datos_del_proyecto, aes(x= irradiac, group = 1))+geom_histogram(col = "#33cc66", fill = "#009966")
Cinco <- ggplot(Datos_del_proyecto, aes(x= lluvia, group = 1))+geom_histogram(col = "#000033", fill = "#000066")
Seis <- ggplot(Datos_del_proyecto, aes(x= HumeRela, group = 1))+geom_histogram(col = "#FF3300",fill = "#CC3300")

grid.arrange(Uno,Dos,Tres,Cuatro,Cinco,Seis,nrow = 3, ncol = 2)

## Lineas Promediadas

data <-
  Datos_del_proyecto %>%
  select(fecha, temp_celcius, velo_viento, evapotran, irradiac, lluvia, HumeRela)%>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))%>%
  group_by (fecha = format(fecha,"%m"))%>%
  summarise (lluvia=sum(lluvia),evapotran=sum(evapotran),temp_celcius=mean(temp_celcius),velo_viento=mean(velo_viento),HumeRela=mean(HumeRela),irradiac=mean(irradiac))

Siete <- ggplot(data, aes(x= fecha,y= temp_celcius, group = 1))+geom_line(col = "#99CC00")
Ocho <- ggplot(data, aes(x= fecha,y= velo_viento, group = 1))+geom_line(col = "#FF3399")
Nueve <- ggplot(data, aes(x= fecha,y= evapotran, group = 1))+geom_line(col = "#CC0000")
Diez <- ggplot(data, aes(x= fecha,y= irradiac, group = 1))+geom_line(col = "#CCCCFF")
Once <- ggplot(data, aes(x= fecha,y=lluvia, group = 1))+geom_line(col = "#003366")
Doce <- ggplot(data, aes(x= fecha,y= HumeRela, group = 1))+geom_line(col = "#669900")

grid.arrange(Siete,Ocho,Nueve,Diez,Once,Doce,nrow = 3, ncol = 2) 





