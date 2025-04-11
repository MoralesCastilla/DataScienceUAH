################################################################# #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2025
##'       by Adrian Fernandez
##'
################################################################# #


## limpiando el ambiente de trabajo
rm(list=ls())

options(stringsAsFactors = FALSE)


## establecer directorio de trabajo (wd)
getwd()  
setwd("~/sandbox/")


## cargar paquetes
library(tidyverse)
library(terra)
library(sf)
library(dplyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(googleway)
library(ncdf4)
library(ggplot2)
library(geodata)
library(mapSpain)
library(rgbif)
library(raster)
theme_set(theme_bw())



## 1. Carga de datos espaciales ##
############################### ##

##' busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984

##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php
##' Usamos el segundo enlace para poder descargar solo la porcion que corresponde de
##' 1980 a 1994 en vez de descargar los datos desde 1950

tmax80_94 <- rast("tx_ens_mean_0.1deg_reg_1980-1994_v31.0e.nc")

#Ahora seleccionamos el periodo que nos interesa

#Primero averiguamos hasta donde llega 1984 en nustrros datos
fechas80_94 <- time(tmax80_94)
which(format(fechas80_94, "%Y"  ) == "1984")

#Despues hacemos un subset de los datos con esta información

tmax80_84 <- subset(tmax80_94, 1:1827)

##' lee datos de Parques Nacionales de España (en github)
##' 

enps <- st_read("~/GitHub/DataScienceUAH/hackathon2025/data/ENP.shp")

##' lee datos de temperaturas mensuales de España, para enero y julio (en github)
##' 

temp_spain01 <-rast("~/GitHub/DataScienceUAH/hackathon2025/data/temp.Spain.jan07.tif")
temp_spain07 <-rast("~/GitHub/DataScienceUAH/hackathon2025/data/temp.Spain.jul07.tif")

##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##' 

lince <- occ_data(scientificName = "Lynx pardinus",
                    limit = 5000)



## 2. Manipulación de datos ##
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

mapa_mundo <- world(resolution = 2 , path ="~/sandbox" )

mapSpain <- mapa_mundo[mapa_mundo$NAME_0 == "Spain"]

map_penin <- crop(mapSpain, ext(-10, 5, 35, 44))

climaspaincrop <- crop(tmax80_84, ext(-10,5,35,44))

climaspain_mask <- mask(climaspaincrop, map_penin)

tmax81 <- subset(tmax80_84, 367:731)

fechas81 <- time(tmax81)

format(fechas81, "%d.%m.%Y")

fechas_enero_81 <- which(format(fechas81, "%m"  ) == "01")

fechas_julio_81 <- which(format(fechas81, "%m"  ) == "07")

tmax_spain_enero <- subset(climaspain_mask, fechas_enero_81)
tmax_spain_julio <- subset(climaspain_mask, fechas_julio_81)

##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 
 
lince_spain <- subset(lince$data, country == "Spain")

##' Extrae los datos de temperaturas mensuales de julio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981
##' 

lince_coords <- data.frame(x = lince_spain$decimalLongitude,
                             y = lince_spain$decimalLatitude)

lince_monthlytemps_enero <- extract(temp_spain01, lince_coords)

lince_monthlytemps_julio <- extract(temp_spain07, lince_coords)

lince_dailytemps_enero <- extract(tmax_spain_enero, lince_coords)

lince_dailytemps_julio <- extract(tmax_spain_julio, lince_coords)

##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España

ppnn <- subset(enps, figura_lp %in% "Parque Nacional")

ppnn_ETRS89 <- st_transform(ppnn, crs(ppnn))

anyNA(lince_coords, recursive = FALSE)#Como hay NAs, los quitamos en la siguiente funcion
lince_coords_ETRS89 <- st_as_sf(na.omit(lince_coords), coords = c("x", "y"), crs = 4258)


lince_ppnn <- st_intersection(ppnn_ETRS89, lince_coords_ETRS89)                                                      

##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981


tmax_mean_enero <- terra::mean(tmax_spain_enero)   
plot(tmax_mean_enero)

tmax_mean_julio <- terra::mean(tmax_spain_julio)   
plot(tmax_mean_julio)

##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github

tmax_mean_enero_resample <- resample(tmax_mean_enero, temp_spain01 , method = "bilinear")

tmax_mean_julio_resample <- resample(tmax_mean_julio, temp_spain07 , method = "bilinear")

## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ##
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 

donana_coords <- subset(ppnn_ETRS89, sitename == "Doñana")

lince_coords_donana <- st_intersection(donana_coords, lince_coords_ETRS89)

lince_temps_donana01 <- extract(tmax_spain_enero, lince_coords_donana)

lince_temps_donana07 <- extract(tmax_spain_julio, lince_coords_donana)

#Como hemos obtenido los datos de las temperaturas diarias hacemos una media para las ocurrencias

comparacion_temps <- data.frame(temp_enero = rowMeans(lince_temps_donana01),
                                temp_julio =rowMeans(lince_temps_donana07))

boxplot(comparacion_temps)

##' Lo siguiente ha sido un intento fallido donde quedaba mal el boxplot
##' Aquí utilizaba la media directamente y quedaba mal (por algun motivo que desconozco)

##' lince_temps_donana01 <- extract(tmax_mean_enero_resample, lince_coords_donana)

##' lince_temps_donana07 <- extract(tmax_mean_julio_resample, lince_coords_donana)

##' comparacion_temps <- data.frame(temp_enero = lince_temps_donana01$mean,
##'  temp_julio = lince_temps_donana07$mean)
##' boxplot(comparacion_temps)


##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas

temp_diff <- abs(tmax_mean_enero_resample - temp_spain01)

alto <- selectHighest(temp_diff, n = 1, low=FALSE )
bajo <- selectHighest(temp_diff, n = 1, low=TRUE )

plot(temp_diff)
points(alto, col= "red", cex=1)
points(bajo, col= "pink", cex=1)

##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

ppnn_names <- data.frame(ID = c(1:16),
                         ppnn_names = ppnn$sitename)

temp_diff_3c <- abs(tmax_mean_julio_resample - temp_spain01)


temp_diff_ppnn <- as.data.frame(extract(temp_diff_3c, ppnn))

temp_diff_ppnn <- na.omit(temp_diff_ppnn)

temp_diff_ppnn_nombre <- temp_diff_ppnn %>%
  left_join(ppnn_names, by = "ID")

# Encontramos el valor máximo

valor_max <- max(temp_diff_ppnn_nombre$mean)

# Buscamos su posicion

coincidencias <- which(temp_diff_ppnn_nombre == valor_max, arr.ind = TRUE)

# Vemos la fila completa

fila_max <- temp_diff_ppnn_nombre[coincidencias[,"row"], ]

#El que tiene la mayor diferencia de temperaturas es Cabañeros

## 4. Bonus ##
#################

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...

bonus <-rast("~/GitHub/DataScienceUAH/hackathon2025/bonus.tif")
plot(bonus)



