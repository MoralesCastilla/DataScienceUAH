
# Preparaciones ####



# Establecemos el directorio de trabajo

setwd("~/GitHub/prac_reptiles/1_temp")



# Llamamos a las librerías que vamos a usar

library(tidyverse)



# Ajustamos el tema de los gráficos

theme_set(theme_bw())



# Leemos y visualizamos los datos de temperatura

datos_temp_clean <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/datos_temp_clean.csv", sep = ",", dec = ".")[,-1]

glimpse(datos_temp_clean)



# Plots ####



## Generales ####



# Relacionamos la temperatura ambiental y la temperatura corporal:

ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb)) +
  
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)")
  

# Existe una relación positiva entre la temperatura media ambiental y corporal



# Añadimos barras de error:

datos_temp_clean <- mutate(datos_temp_clean, 
                           meantemp_max = mean_temp + temp_seasonality/100,
                           meantemp_min = mean_temp - temp_seasonality/100)

ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb)) +
  
  geom_point() +
  geom_smooth(method = "lm")  +
  geom_errorbar(aes(ymin = min_tb, ymax = max_tb), width = .3, colour = "darkgray", alpha = .2) +
  geom_errorbar(aes(xmin = meantemp_min, xmax = meantemp_max), width = .3, colour = "darkgray", alpha = .4) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)")



# Agrupamos los datos con distintos criterios para ver cómo es la relación



## Por familias ####



ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, colour = family)) +
  
  geom_point() +
  geom_smooth(method = "lm")  +
  scale_color_manual(values = c(Alligatoridae = "#0096FF", Boidae = "#4CBB17", Colubridae = "#FF7518", Elapidae = "#FF69B4",
                                Psammophiidae = "#FF0000", Sphenodontidae = "#FFEA00", Viperidae = "#8e44ad")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Familia")


ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, colour = family)) +
  
  geom_point() +
  geom_smooth(method = "lm")  +
  geom_errorbar(aes(ymin = min_tb, ymax = max_tb), width = .3, colour = "darkgray", alpha = .2) +
  geom_errorbar(aes(xmin = meantemp_min, xmax = meantemp_max), width = .3, colour = "darkgray", alpha = .4) +
  scale_color_manual(values = c(Alligatoridae = "#0096FF", Boidae = "#4CBB17", Colubridae = "#FF7518", Elapidae = "#FF69B4",
                                Psammophiidae = "#FF0000", Sphenodontidae = "#FFEA00", Viperidae = "#8e44ad")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Familia")

# La mayoría de las familias muestran una correlación positiva

# La familia Alligatoridae, sin embargo, muestra una correlación negativa (pero esta familia solo cuenta con dos valores)

# Las familias Psammophiidae y Sphenodontidae solo tienen 1 representante, por lo que no podemos observar
# la correlación (si es que existe)



## Por órdenes ####



ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, colour = order)) +
  
  geom_point() +
  geom_smooth(method = "lm")  +
  scale_color_manual(values = c(Crocodilia = "#0096FF", Rhynchocephalia = "#8e44ad", Squamata = "#FF7518")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Orden")


ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, colour = order)) +
  
  geom_point() +
  geom_smooth(method = "lm")  +
  geom_errorbar(aes(ymin = min_tb, ymax = max_tb), width = .3, colour = "darkgray", alpha = .2) +
  geom_errorbar(aes(xmin = meantemp_min, xmax = meantemp_max), width = .3, colour = "darkgray", alpha = .4) +
  scale_color_manual(values = c(Crocodilia = "#0096FF", Rhynchocephalia = "#8e44ad", Squamata = "#FF7518")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Orden")

# En el orden Crocodilia vemos la misma correlación que en la familia Alligatoridae (negativa)

# El orden Rhynchocephalia solo tiene 1 representante, por lo que no podemos analizar la correlación

# En el orden Squamata, la correlación positiva es muy notable



## Por región biogeográfica ####



ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, colour = bio_region)) +
  
  geom_point() +
  geom_smooth(method = "lm")  +
  scale_color_manual(values = c(Neotropic = "#4CBB17", Nearctic = "#FF7518", "Australo-Pacific" = "#FF0000",
                                "Saharo-Sindian" = "#FF69B4", Palearctic = "#8e44ad")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Región biogeográfica")


ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, colour = bio_region)) +
  
  geom_point() +
  geom_smooth(method = "lm")  +
  geom_errorbar(aes(ymin = min_tb, ymax = max_tb), width = .3, colour = "darkgray", alpha = .2) +
  geom_errorbar(aes(xmin = meantemp_min, xmax = meantemp_max), width = .3, colour = "darkgray", alpha = .4) +
  scale_color_manual(values = c(Neotropic = "#4CBB17", Nearctic = "#FF7518", "Australo-Pacific" = "#FF0000",
                                "Saharo-Sindian" = "#FF69B4", Palearctic = "#8e44ad")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Región biogeográfica")

# En todas las regiones se observa una correlación positiva, excepto en la paleártica, que solo tiene 1 representante

# La región Saharo-Sindia solo cuenta con dos representantes



# OBSERVACIÓN:

# En la gráfica, el orden Squamata parece coincidir con la región neártica

# Lo comprobamos:

squamata <- which(datos_temp_clean$order == "Squamata")

# Hay 68 especies del orden Squamata

squam_nearc <- which(datos_temp_clean$order == "Squamata" & datos_temp_clean$bio_region == "Nearctic")

# Hay 49 especies del orden Squamata que habitan la región neártica



# Latitud y altitud ####



# Leemos los datos de latitud de las especies:

mean_lat <- read.csv("1_data/mean_lat_rep2000.csv")

glimpse(mean_lat)



# Juntamos los dos dataframes con la función merge():

datos_tem_lat <- merge(x = datos_temp_clean, y = mean_lat)

glimpse(datos_tem_lat)



# Añadimos la diferencia entre la temperatura corporal y la ambiental en una nueva columna:

datos_tem_lat$dif_temp <- datos_temp_clean$mean_tb - datos_temp_clean$mean_temp

# Un valor positivo significa que la temperatura corporal de la especie es mayor que la ambiental



## Plots ####



# Hacemos Un plot para ver cómo se relaciona esta diferencia de temperatura con la latitud que habitan las especies:

ggplot(datos_tem_lat, aes(x = abs(latmedia), y = dif_temp)) +
  
  geom_point(aes(colour = family)) +
  geom_smooth(method = "lm", aes(colour = family)) +
  scale_color_manual(values = c(Alligatoridae = "#0096FF", Boidae = "#4CBB17", Colubridae = "#FF7518", Elapidae = "#FF69B4",
                                Psammophiidae = "#FF0000", Sphenodontidae = "#FFEA00", Viperidae = "#8e44ad")) +
  labs(title = "Relación entre la temeperatura de los reptiles y la latitud en la que se encuentran",
       x = "Latitud media",
       y = "Diferencia entre temperatura corporal y temperatura ambiental (ºC)",
       colour = "Familia")

# Y otro plot utilizando la altitud:

ggplot(datos_tem_lat, aes(x = (max_elevation + min_elevation)/2, y = dif_temp)) +
  
  geom_point(aes(colour = family)) +
  geom_smooth(method = "lm", aes(color = family)) +
  scale_color_manual(values = c(Alligatoridae = "#0096FF", Boidae = "#4CBB17", Colubridae = "#FF7518", Elapidae = "#FF69B4",
                                Psammophiidae = "#FF0000", Sphenodontidae = "#FFEA00", Viperidae = "#8e44ad")) +
  labs(title = "Relación entre la temeperatura de los reptiles y la altitud en la que se encuentran",
       x = "Altitud media",
       y = "Diferencia entre temperatura corporal y temperatura ambiental (ºC)",
       colour = "Familia")

# Hay una correlación positiva en ambos casos, por lo que cuanto mayores son la altitud y latitud,
# mayor es la diferencia entre la temperatura corporal y la ambiental

# Esto quiere decir que los reptiles son capaces de termorregular, probablemente mediante mecanismos comportamentales



# Limpieza de datos ####



## Familias ####



# Hay cuatro familias que tienen datos insuficientes: Alligatoridae (2), Psammophiidae (1), Sphenodontidae (1) y
# Boidae (2)



# Quitamos las 4 familias:

datos_tem_lat$family

temp_lat_fam <- datos_tem_lat[-c(3, 5, 6, 8, 46, 52),]

                                                               

# Repetimos los plots:

ggplot(temp_lat_fam, aes(x = mean_temp, y = mean_tb, colour = family)) +
  
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.2, aes(fill = family))  +
  scale_color_manual(values = c(Colubridae = "#FF7518", Elapidae = "#0096FF", Viperidae = "#8e44ad")) +
  scale_fill_manual(values = c(Colubridae = "#ffa76b", Elapidae = "#7cc4f7", Viperidae = "#b279c9")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Familia",
       fill = "Familia")


ggplot(temp_lat_fam, aes(x = abs(latmedia), y = dif_temp)) +
  
  geom_point(aes(colour = family)) +
  geom_smooth(method = "lm", alpha = 0.2, aes(color = family, fill = family)) +
  scale_color_manual(values = c(Colubridae = "#FF7518", Elapidae = "#0096FF", Viperidae = "#8e44ad")) +
  scale_fill_manual(values = c(Colubridae = "#ffa76b", Elapidae = "#7cc4f7", Viperidae = "#b279c9")) +
  labs(title = "Relación entre la temeperatura de los reptiles y la latitud en la que se encuentran",
       x = "Latitud media",
       y = "Diferencia entre temperatura corporal y temperatura ambiental (ºC)",
       colour = "Familia",
       fill = "Familia")


ggplot(temp_lat_fam, aes(x = (max_elevation + min_elevation)/2, y = dif_temp)) +
  
  geom_point(aes(colour = family)) +
  geom_smooth(method = "lm", alpha = 0.2, aes(color = family, fill = family)) +
  scale_color_manual(values = c(Colubridae = "#FF7518", Elapidae = "#0096FF", Viperidae = "#8e44ad")) +
  scale_fill_manual(values = c(Colubridae = "#ffa76b", Elapidae = "#7cc4f7", Viperidae = "#b279c9")) +
  labs(title = "Relación entre la temeperatura de los reptiles y la altitud en la que se encuentran",
       x = "Altitud media",
       y = "Diferencia entre temperatura corporal y temperatura ambiental (ºC)",
       colour = "Familia",
       fill = "Familia")



## Órdenes ####



# Hay dos órdenes que tienen datos insuficientes: Crocodilia (2) y Rhynchocephalia (1)



# Quitamos las 4 familias:

datos_tem_lat$order

temp_lat_order <- datos_tem_lat[-c(3, 6, 52),]



# Repetimos el plot:

ggplot(temp_lat_order, aes(x = mean_temp, y = mean_tb, colour = order)) +
  
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.2, aes(colour = order, fill = order))  +
  scale_color_manual(values = c(Squamata = "#FF7518")) +
  scale_fill_manual(values = c(Squamata = "#ffa76b")) +
  labs(title = "Relación entre la temeperatura ambiental y la temperatura corporal de reptiles",
       x = "Temperatura media ambiental (ºC)",
       y = "Temperatura corporal media (ºC)",
       colour = "Orden",
       fill = "Orden")



# Y hacemos los plots correspondientes teniendo en cuenta la latitud y la altitud

ggplot(temp_lat_order, aes(x = abs(latmedia), y = dif_temp)) +
  
  geom_point(aes(colour = order)) +
  geom_smooth(method = "lm", alpha = 0.2, aes(colour = order, fill = order))  +
  scale_color_manual(values = c(Squamata = "#FF7518")) +
  scale_fill_manual(values = c(Squamata = "#ffa76b")) +
  labs(title = "Relación entre la temeperatura de los reptiles y la latitud en la que se encuentran",
       x = "Latitud media",
       y = "Diferencia entre temperatura corporal y temperatura ambiental (ºC)",
       colour = "Orden",
       fill = "Orden")


ggplot(temp_lat_order, aes(x = (max_elevation + min_elevation)/2, y = dif_temp)) +
  
  geom_point(aes(colour = order)) +
  geom_smooth(method = "lm", alpha = 0.2, aes(colour = order, fill = order))  +
  scale_color_manual(values = c(Squamata = "#FF7518")) +
  scale_fill_manual(values = c(Squamata = "#ffa76b")) +
  labs(title = "Relación entre la temeperatura de los reptiles y la altitud en la que se encuentran",
       x = "Altitud media",
       y = "Diferencia entre temperatura corporal y temperatura ambiental (ºC)",
       colour = "Orden",
       fill = "Orden")

