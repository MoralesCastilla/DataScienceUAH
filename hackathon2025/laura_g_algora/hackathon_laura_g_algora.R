################################################################# #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'
################################################################# #



## Limpiamos el ambiente de trabajo

rm(list=ls())
options(stringsAsFactors = FALSE)



## Establecemos el directorio de trabajo (wd)

getwd()  
setwd("~/GitHub/DataScienceUAH/hackathon2025/laura_g_algora/")



## Cargamos paquetes

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(googleway)
library(ncdf4)
library(ggplot2)
library(geodata)
library(mapSpain)
library(raster)
library(rgbif)
library(patchwork)

## Y ajustamos el tema que vamos a usar en los plots

theme_set(theme_bw())



# 1. Carga de datos espaciales ####
############################### ##



## 1.a Datos de temperaturas máximas (Europa) ####

##' Busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 

tmax_europa <- rast("~/GitHub/DataScienceUAH/hackathon2025/laura_g_algora/tx_ens_mean_0.25deg_reg_1980-1994_v30.0e.nc")

tmax_europa



## 1.b Datos de Parques Nacionales ####

##' Lee datos de Parques Nacionales de España (en Github)

parques_nacionales <- st_read("~/GitHub/DataScienceUAH/hackathon2025/data/ENP.shp")

parques_nacionales



## 1.c Datos de temperaturas de enero y julio (España) ####

##' Lee datos de temperaturas mensuales de España, para enero y julio (en Github)

# Usamos la función raster() del paquete raster

temp_enero <- raster("~/GitHub/DataScienceUAH/hackathon2025/data/temp.Spain.jan07.tif")

plot(temp_enero)


temp_julio <- raster("~/GitHub/DataScienceUAH/hackathon2025/data/temp.Spain.jul07.tif")

plot(temp_julio)



## 1.d Datos del lince ibérico ####

##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif

occ_search(scientificName = "Lynx pardinus")

# En la base de datos hay 2021 registros del lince ibérico

lince <- occ_data(scientificName = "Lynx pardinus",
                  limit = 5000) 

lince



# 2. Manipulación de datos ####
############################### ##

## 2.a Temperaturas máximas de España en enero y julio ####

##' Crea un objeto con los datos de temperaturas máximas de España del mes de enero de 1981. 
##' Repite para el mes de julio de 1981.
##' Pista: tendrás que recortar y enmascarar los datos de E-OBS.


# Utilizando el mismo método que en clase, le resta 10 años a las fechas. Como no he podido solucionarlo, he buscado
# otra alternativa

# fechas <- time(tmax_europa)

# Creamos un objeto con las fechas de enero de 1981

# which(format(fechas, "%Y") == "1981" &
#       format(fechas, "%m") == "01")

# dias_enero <- fechas[367:397]

# Y otro objeto con la fechas de julio de 1981

# which(format(fechas, "%Y") == "1981" &
#         format(fechas, "%m") == "07")

# dias_julio <- fechas[548:578]

# De esta forma no consigo hacer el subset, así que le preguntamos a ChatGPT :)


# Hacemos el subset directamente sobre el archivo SpatRaster

tmax_spain <- crop(tmax_europa, ext(-10, 5, 35, 44))  # Primero lo recortamos para quedarnos solo con España

fechas <- time(tmax_spain)

which(format(fechas, "%Y") == "1981" &  # Los días correspondientes al mes de enero de 1981 son
      format(fechas, "%m") == "01")     # los días 367 a 397

tmax_enero <- tmax_spain[[367:397]]

plot(tmax_enero)


which(format(fechas, "%Y") == "1981" &  # Los días correspondientes al mes de julio de 1981 son
        format(fechas, "%m") == "07")   # Los días 548 a 578

tmax_julio <- tmax_spain[[548:578]]

plot(tmax_julio)


# Buscamos un mapa de España (quitamos Portugal, Francia...), porque los mapas de tmax_enero y tmax_julio
# muestran toda la península ibérica y parte de África

mapa_mundo <- world(resolution = 2 , path ="~/Github" )

mapa_spain <- mapa_mundo[mapa_mundo$NAME_0 == "Spain"]

mapa_peninsula <- crop(mapa_spain, ext(-10, 5, 35, 44))


# Enmascaramos el mapa de España con nuestros datos de temeperatura

tmax_enero_mask <- mask(tmax_enero, mapa_peninsula)

plot(tmax_enero_mask)


tmax_julio_mask <- mask(tmax_julio, mapa_peninsula)

plot(tmax_julio_mask)



## 2.b Datos del lince ibérico en España ####

##' Crea un subset con los datos de ocurrencia del lince, solo para España

table(lince$data$country)

lince_spain <- subset(lince$data, country == "Spain")



##' Extrae los datos de temperaturas mensuales de julio y enero para cada ocurrencia del lince en España.
##' Repite para las temperaturas máximas de los días del mes de enero y del mes de julio de 1981.

# Creamos un dataframe con las coordenadas de cada ocurrencia del lince

coordenadas_lince <- data.frame(x=lince_spain$decimalLongitude,
                                y=lince_spain$decimalLatitude)

# Extraemos los datos de temperatura para enero y julio (también en 1981)

temp_lince_enero <- extract(temp_enero, coordenadas_lince)

tmax_lince_enero_1981 <- extract(tmax_enero_mask, coordenadas_lince)


temp_lince_julio <- extract(temp_julio, coordenadas_lince)

tmax_lince_julio_1981 <- extract(tmax_julio_mask, coordenadas_lince)



##' Extrae las ocurrencias del lince que tienen lugar dentro de los Parques Nacionales de España

# Nos quedamos solo con los Parques Nacionales

str(parques_nacionales)

parques_nacionales$figura_lp

parques_nacionales <- subset(parques_nacionales, figura_lp == "Parque Nacional")

# Ajustamos el sistema de referencia de coordenadas

coordenadas_parques = st_transform(parques_nacionales, crs(parques_nacionales))

coordenadas_lince <- na.omit(coordenadas_lince)  # En el caso del lince, omitimos los NA's

# Las coordenadas de los parques y los linces deben estar en el mismo sistema de referencia

st_crs(coordenadas_parques)  # Los datos de los Parques Nacionales utilizan el sistema de coordenadas 4258
str(coordenadas_lince)       # Los datos de coordenadas del lince son un dataframe con las columnas x e y

coordenadas_lince <- st_as_sf(coordenadas_lince, coords = c("x", "y"), crs = 4258)

# Extraemos los datos de ocurrencia del lince que coinciden con las coordenadas de Parques Nacionales

lince_parques <- st_intersection(coordenadas_lince, coordenadas_parques)

# Comprobamos que ha funcionado

lince_parques

plot(temp_enero)
lines(coordenadas_parques)
points(lince_parques, col = "red")



##' Calcula un mapa raster con la media de las temperaturas máximas de enero de 1981 y de julio de 1981

tmedia_enero_1981 <- app(tmax_enero_mask, mean)

plot(tmedia_enero_1981)


tmedia_julio_1981 <- app(tmax_julio_mask, mean)

plot(tmedia_julio_1981)



##' Transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##' y proyección utilizada por los mapas mensuales que descargaste de Github

# Comprobamos la resolución, el extent y la proyección de ambos mapas para el mes de enero

temp_enero
tmedia_enero_1981

# Transformamos el mapa de enero de 1981

tmedia_enero_1981_rasterlayer <- as(tmedia_enero_1981, "Raster")  # Lo convertimos primero en un RasterLayer

tmedia_enero_1981_0.1 <- resample(tmedia_enero_1981_rasterlayer, temp_enero, method = "bilinear")

# Comprobamos que ha funcionado

tmedia_enero_1981_0.1

plot(tmedia_enero_1981_0.1)

# Repetimos el proceso para julio de 1981

tmedia_julio_1981_rasterlayer <- as(tmedia_julio_1981, "Raster")  # Lo convertimos primero en un RasterLayer

tmedia_julio_1981_0.1 <- resample(tmedia_julio_1981_rasterlayer, temp_julio, method = "bilinear")

tmedia_julio_1981_0.1

plot(tmedia_julio_1981_0.1)



# 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ####
###################################################################################################### ##

## 3.a. Temperaturas del lince en Doñana en enero y julio de 1981 ####

##' Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas experimentadas
##' por el lince dentro del Parque Nacional de Doñana en enero y julio de 1981. 

# Hacemos un subset con las coordenadas del Parque Nacional de Doñana

coordenadas_donana <- subset(coordenadas_parques, coordenadas_parques$sitename == "Doñana")

# Hallamos las ocurrencias del lince que coinciden con las coordenadas de Doñana

coordenadas_lince_donana <- st_intersection(coordenadas_lince, coordenadas_donana)

# Extraemos las temperaturas máximas de enero y julio de 1981 para cada ocurrencia del lince en Doñana

tmax_lince_enero_1981_donana <- extract(tmax_enero_mask, coordenadas_lince_donana)

str(tmax_lince_enero_1981_donana)


tmax_lince_julio_1981_donana <- extract(tmax_julio_mask, coordenadas_lince_donana)

str(tmax_lince_julio_1981_donana)

# Calculamos la temperatura media para cada una de las ocurrencias del lince

tmax_lince_enero_1981_donana <- rowMeans(tmax_lince_enero_1981_donana)

str(tmax_lince_enero_1981_donana)


tmax_lince_julio_1981_donana <- rowMeans(tmax_lince_julio_1981_donana)

str(tmax_lince_julio_1981_donana)

# Hacemos un dataframe con los datos de temperaturas de ambos meses

dataframe_tmax_lince_1981_donana <- data.frame(tmax_enero = tmax_lince_enero_1981_donana,
                                    tmax_julio = tmax_lince_julio_1981_donana)

# Hacemos plots para comparar las temperaturas experimentadas por el lince en Doñana en enero y julio de 1981

boxplot(dataframe_tmax_lince_1981_donana,
        xlab = "Mes",
        ylab = "Temperatura (ºC)",
        names = c("Enero", "Julio"),
        main = "Temperaturas máximas del lince en Doñana (1981)")


plot_lince_enero_1981_donana  <- ggplot(dataframe_tmax_lince_1981_donana, aes(x = c(1:225), y = tmax_enero)) +
                                 geom_point() +
                                 labs(
                                   title = "Temperaturas máximas del lince en Doñana (Enero, 1981)",
                                   x = "Número de ocurrencia",
                                   y = "Temperatura (ºC)"
                                 ) +
                                 theme(
                                   plot.title = element_text(size = 10, hjust = 0.5)
                                 )
      

plot_lince_julio_1981_donana <- ggplot(dataframe_tmax_lince_1981_donana, aes(x = c(1:225), y = tmax_julio)) +
                                geom_point() +
                                labs(
                                  title = "Temperaturas máximas del lince en Doñana (Julio, 1981)",
                                  x = "Número de ocurrencia",
                                  y = "Temperatura (ºC)"
                                ) +
                                theme(
                                  plot.title = element_text(size = 10, hjust = 0.5)
                                )
    

plot_lince_enero_1981_donana + plot_lince_julio_1981_donana



## 3.b. Temperatura media y máxima de enero de 1981 ####

##' Haz un mapa que compare la media de las temperaturas máximas del mes de enero de 1981 con
##' las temperaturas medias del mes de enero. ¿En qué parte de España las diferencias entre
##' máximas del 81 y medias son más pequeñas? ¿Dónde son más grandes?   
##' Pista: usa operaciones aritméticas

plot(tmedia_enero_1981_0.1)
plot(temp_enero)

# Calculamos la diferencia de temperaturas

comparacion_tmax_tmedia_enero <- abs(tmedia_enero_1981_0.1 - temp_enero)

plot(comparacion_tmax_tmedia_enero,
     main = "Diferencia entre las temperaturas máximas de enero de 1981 y la temperatura media de enero (ºC)",
     cex.main = 0.9,
     xlab = "Longitud",
     ylab = "Latitud")

# Buscamos los lugares con las diferencias mínima y máxima de temperatura

min_dif_temp_enero <- which.min(comparacion_tmax_tmedia_enero[])  # Buscamos la celda con el valor mínimo

coords_min_dif_temp_enero <- xyFromCell(comparacion_tmax_tmedia_enero, min_dif_temp_enero)  # Extraemos las coordenadas


max_dif_temp_enero <- which.max(comparacion_tmax_tmedia_enero[])  # Buscamos la celda con el valor máximo

coords_max_dif_temp_enero <- xyFromCell(comparacion_tmax_tmedia_enero, max_dif_temp_enero)  # Extraemos las coordenadas

# Lo visualizamos en el plot

plot(comparacion_tmax_tmedia_enero,
     main = "Diferencia entre las temperaturas máximas de enero de 1981 y la temperatura media de enero (ºC)",
     cex.main = 0.9,
     xlab = "Longitud",
     ylab = "Latitud")
points(coords_min_dif_temp_enero, col = "blue", pch = 20, cex = 2)
points(coords_max_dif_temp_enero, col = "red", pch = 20, cex = 2)
legend("bottomright",
       legend = c("Diferencia mínima", "Diferencia máxima"),
       col = c("blue", "red"),
       pch = 20,
       pt.cex = 2,
       bty = "n")



## 3.c. Diferencias de temperaturas medias y máximas en Parques Nacionales ####

##' ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

plot(tmedia_julio_1981_0.1)
plot(temp_enero)

# Calculamos la diferencia de temperaturas y extraemos las temperaturas para los parques nacionales

comparacion_tmax_tmedia <- abs(tmedia_julio_1981_0.1 - temp_enero)

comparacion_tmax_tmedia_parques <- extract(comparacion_tmax_tmedia, coordenadas_parques)

# La extracción sale con valores NULL, NA y NaN porque no tenemos datos de temperatura para todos los parques

plot(comparacion_tmax_tmedia)  
lines(coordenadas_parques)     # Dos parques se salen del mapa (no tienen datos de temperatura)

coordenadas_parques$sitename  # Se trata de los parques que se encuentran en zonas marinas/oceánicas

# Eliminamos las coordenadas de estos parques

coordenadas_parques_peninsula <- coordenadas_parques[!(coordenadas_parques$sitename %in%
                                                         c("Islas Atlánticas de Galicia",
                                                           "Archipiélago de Cabrera")), ]

comparacion_tmax_tmedia_parques <- extract(comparacion_tmax_tmedia, coordenadas_parques_peninsula)

str(comparacion_tmax_tmedia_parques)  # Seguimos teniendo valores NULL

which(sapply(comparacion_tmax_tmedia_parques, is.null))  # Tampoco tenemos datos de estos parques (11-14)

coordenadas_parques_peninsula$sitename  # Buscamos sus nombres a ojo porque tenemos pocos datos (11-14)

coordenadas_parques_peninsula <- coordenadas_parques_peninsula[!(coordenadas_parques_peninsula$sitename %in%
                                                                   c("Garajonay", "La Caldera de Taburiente",
                                                                     "Timanfaya", "El Teide")), ]

comparacion_tmax_tmedia_parques <- extract(comparacion_tmax_tmedia, coordenadas_parques_peninsula)

str(comparacion_tmax_tmedia_parques)  # Ya no tenemos valores NULL

plot(comparacion_tmax_tmedia)  
lines(coordenadas_parques_peninsula)

# Como tenemos una lista de temperaturas para cada parque, hacemos la media de cada una de ellas y nos quedamos
# solo con un valor por parque nacional

comparacion_tmax_tmedia_parques <- sapply(comparacion_tmax_tmedia_parques, mean)

comparacion_tmax_tmedia_parques

# Hacemos un dataframe con las diferencias de temperatura que incluya los nombres de los parques nacionales

comparacion_tmax_tmedia_parques <- data.frame(parque_nacional = coordenadas_parques_peninsula$sitename, 
                                              diferencia_temperatura = comparacion_tmax_tmedia_parques)

comparacion_tmax_tmedia_parques

# Buscamos la diferencia más alta de temperatura y el parque al que pertenece

diferencia_temp_max_parques <- max(comparacion_tmax_tmedia_parques$diferencia_temperatura)  # El valor más alto

parque_diferencia_max <- which.max(comparacion_tmax_tmedia_parques$diferencia_temperatura)   # Posición del valor más alto
parque_diferencia_max <- comparacion_tmax_tmedia_parques$parque_nacional[parque_diferencia_max]  # Parque al que pertenece

cat("La diferencia de temperatura más alta entre la media de las temperaturas máximas de julio de 1981
    y la temperatura media de enero es", diferencia_temp_max_parques,
    "y corresponde al Parque Nacional", parque_diferencia_max, "\n")

# Repetimos el proceso para la diferencia más baja de temperatura

diferencia_temp_min_parques <- min(comparacion_tmax_tmedia_parques$diferencia_temperatura)  # El valor más bajo

parque_diferencia_min <- which.min(comparacion_tmax_tmedia_parques$diferencia_temperatura)  # Posición del valor más bajo
parque_diferencia_min <- comparacion_tmax_tmedia_parques$parque_nacional[parque_diferencia_min]  # Parque al que pertenece

cat("La diferencia de temperatura más baja entre la media de las temperaturas máximas de julio de 1981
    y la temperatura media de enero es", diferencia_temp_min_parques,
    "y corresponde al Parque Nacional", parque_diferencia_min, "\n")



# 4. Bonus ####
################# ##

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos de github.
##' Cárgalo en R, multiplícalo por el mapa que has generado en el apartado 3b, y visualízalo.
##' ¿Qué ha pasado? ¿Serías capaz de generar un resultado parecido, pero con una imagen propia?
##' Recuerda que una fotografía, no deja de ser un mapa raster...

# Cargamos el mapa y lo visualizamos

easter_egg <- rast("~/GitHub/DataScienceUAH/hackathon2025/bonus.tif")

easter_egg
plot(easter_egg)

# Lo multiplicamos por el mapa del apartado 3b

comparacion_tmax_tmedia_enero  # El mapa del apartado 3b es un RasterLayer
easter_egg  # El mapa nuevo es un SpatRaster

easter_egg_raster_layer <- as(easter_egg, "Raster")  # Transformamos el mapa nuevo en un RasterLayer

mapa <- comparacion_tmax_tmedia_enero * easter_egg_raster_layer

plot(mapa)

# Visualizamos todos los mapas juntos

x11()

par(mfrow = c(2, 2))

plot(easter_egg, main = "Easter egg")

plot(easter_egg_raster_layer, main = "Easter egg (RasterLayer)")

plot(comparacion_tmax_tmedia_enero, main = "Temperatura enero")

plot(mapa, main = "Mapa")

# Al multiplicar los dos mapas, las celdas que tenían valores de 0 en el mapa easter_egg (y, por lo tanto, en el 
# mapa easter_egg_raster_layer), también tienen valor 0 en el mapa resultante

# Cargamos una imagen propia

#install.packages("png")  # Necesitamos el paquete png para poder leer la imagen
library(png)

imagen <- readPNG("~/GitHub/DataScienceUAH/hackathon2025/laura_g_algora/among_us.png")

#install.packages("grid")  # Y el paquete grid para poder visualizarla
library(grid)

grid.raster(imagen)

# Convertimos la imagen en un objeto de tipo RasterLayer

dim(imagen)  # La imagen tiene 4 dimensiones porque está en color y tiene el fondo transparente (RGBA)

imagen_gris <- raster((imagen[,,1] + imagen[,,2] +        # Hacemos la media de las 4 capas (la imagen pasa a tener 1 dimensión,
                         imagen[,,3]) + imagen[,,4] / 4)  # es decir, ahora está en escala de grises)

imagen_gris
plot(imagen_gris)

# Modificamos la imagen para que algunas de las celdas tengan valor 0 (en este caso, las que tengan un valor menor a 0.75)

imagen_modificada <- calc(imagen_gris, fun = function(x) { ifelse(x < 0.75, 0, x) })

imagen_modificada
plot(imagen_modificada)

# Antes de multiplicar los dos objetos, deben tener el mismo formato (extent, crs, resolución...)

comparacion_tmax_tmedia_enero
imagen_modificada

# La imagen no tiene un sistema de coordinadas asignado, así que debemos asignar uno

crs(imagen_modificada) <- CRS("+proj=longlat +datum=WGS84")

imagen_modificada  # Ahora el sistema de coordenadas coincide con el del mapa

# Ajustamos el extent de la imagen al del mapa

extent(imagen_modificada) <- extent(comparacion_tmax_tmedia_enero)

# Ajustamos la resolución de la imagen para que coincida con la del mapa

imagen_modificada <- resample(imagen_modificada, comparacion_tmax_tmedia_enero, method = "bilinear")

# Multiplicamos los objetos y visualizamos el resultado

resultado <- comparacion_tmax_tmedia_enero * imagen_modificada

plot(resultado)


