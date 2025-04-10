##'############################################################### #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'       solved by Mengna Zhou
##'
##'############################################################### #


## limpiando el ambiente de trabajo
rm(list=ls())
options(stringsAsFactors = FALSE)


## establecer directorio de trabajo (wd)
getwd()  
setwd("C:/Users/usuario/Desktop/hackathon")

## instalar paquete
install.packages("ggplot2")
install.packages("sf")

## cargar librerías
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
theme_set(theme_bw())



## 1. Carga de datos espaciales ####
##'############################# ##

##' busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 

## Cargar datos raster

tmaxtotal <- rast("tx_ens_mean_0.25deg_reg_1980-1994_v31.0e.nc")

## Ver qué pinta tienen nuestros datos

tmaxtotal
  # 201 filas, 464 columnas, 5479 capas = 5479 días
  # resolución 0.25
  # De 1980 hasta 1994. Queremos solo de los años 1980 a 1984.

## Seleccionar los datos de los años 1980 a 1984:

  ## Haremos un subset y seleccionaremos datos de los 4 años
365*5
tmax8084 <- subset(tmaxtotal, 1:1827)
tmax8084
# 201 filas, 464 columnas, 1827 capas = 1827 días



##' lee datos de Parques Nacionales de España (en github --> ENP.shp)

  # Daba error "st_read()" --> solución: descargar también ENP.dbf y ENP.shx
enp <- st_read("ENP.shp")
enp
  # Tipo de objeto multipoligonal, con 1840 objetos y 8 fields, longitud según 
  # X e Y (coordenadas)
  # 1840 objetos se refieren a monumentos naturales, parques naturales, 
  # parques nacionales...

  # Queremos solo parques nacionales
  ## Para ver qué tipos/categorías de parques tengo (46 tipos)
unique(enp$figura_lp)

  # seleccionar solo parques nacionales (imponiendo un operador lógico)
enppn <- subset(enp, figura_lp %in% "Parque Nacional")
enppn
unique(enppn$figura_lp)


##' lee datos de temperaturas mensuales de España, para enero y julio 
##' (en github)

install.packages("raster")
library(raster)

tempenero <- raster("temp.Spain.jan07.tif")
tempenero
plot(tempenero)
  # 79 filas, 138 columnas, 10902 capas

tempjulio <- raster("temp.Spain.jul07.tif")
tempjulio
plot(tempjulio)
  # 79 filas, 138 columnas, 10902 capas



##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif

  # Para instalar datos de especies y su distribución (de GBIF), se instala 
  # el paquete:

install.packages("rgbif")
library(rgbif)

# Para buscar si la especie está en la base de datos de GBIF:

occ_search(scientificName = "Lynx pardinus")

lincetotal <- occ_data(scientificName = "Lynx pardinus")
lincetotal

class(lincetotal)
table(lincetotal$data$country)
  # 459 en España, 35 en Portugal


## 2. Manipulación de datos ####
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

# Generamos el mapa de España
# paquete geodata


mapamundo <- world(resolution = 2,
                   path = "~")

mapamundo
plot(mapamundo)

# Puedo hacer un "subset" para seleccionar los datos --> utilizando 
# "[filas, columnas]" para seleccionar
# filas y columnas, tratar los datos como si fueran un data.frame
# Si se deja en blanco el espacio, se selecciona todos los datos

# para ver en qué fila está (Spain está en la fila 69):
mapamundo$NAME_0  
# which(mapamundo$NAME_0 == "Spain")


mapspain <- mapamundo[69, ]
# mapspain <- mapamundo[mapamundo$NAME_0 == "Spain", ]

mapspain
plot(mapspain)


# cut map (para recortar el mapa, seleccionando solo España peninsular)

mappeninsula <- crop(mapspain, ext(-10, 5, 35, 44))
plot(mappeninsula)
CCAA_sf <- esp_get_ccaa()  #comunidades autónomas

# ext(xmin, xmax, ymin, ymax) --> para poner la extensión, los límites de las 
# coordenadas

  # Seleccionar año 1981
tmax8084
tmax81 <- subset(tmax8084, 367:731)
tmax81

  # Seleccionar mes de enero y de julio
tmax81date <- as.Date(time(tmax81))

enero1981 <- which(format(tmax81date, "%m") == "01")
enero1981

julio1981 <- which(format(tmax81date, "%m") == "07")
julio1981

  # Para seleccionar las temperaturas máximas del mes de enero y junio
tmaxenero <- subset(tmax81, enero1981)
tmaxenero
plot(tmaxenero$tx_367) # Vemos que sale temperaturas de toda Europa

cortado <- crop(tmaxenero, ext(-10, 5, 35, 44))
enerotmaxspain <- mask(cortado, mappeninsula)
plot(enerotmaxspain$tx_368)
lines(mappeninsula)

tmaxjulio <- subset(tmax81, julio1981)
tmaxjulio

cortado2 <- crop(tmaxjulio, ext(-10, 5, 35, 44))
juliotmaxspain <- mask(cortado2, mappeninsula)
plot(juliotmaxspain$tx_548)
lines(mappeninsula)

##' Crea un subset con los datos de ocurrencia del lince, solo para España
lincetot <- lincetotal$data
lincespain <- subset(lincetot, country == "Spain")
lincespain
  # 459 observaciones en España


##' Extrae los datos de temperaturas mensuales de junio (¿¿julio??) y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas 
##' de los
##' días del mes de enero y del mes de julio de 1981

  # Mapa del clima mensual de enero y de julio:
plot(tempenero)
plot(tempjulio)




  # Hacemos un data.frame con las coordenadas:
lincecoords <- data.frame(x = lincespain$decimalLongitude, 
                            y = lincespain$decimalLatitude)
lincecoords

  # Ver si tenemos y dónde NA
print(lincecoords[apply(lincecoords, 
                            1, 
                            function(x) any(is.na(x)))
                      , ])

  # Eliminar NA
lincecoords_sin_na <- na.omit(lincecoords)
print(lincecoords_sin_na)

  # Comprobar que se han eliminado los NA
print(lincecoords_sin_na[apply(lincecoords_sin_na, 
                        1, 
                        function(x) any(is.na(x)))
                  , ])

  # Extraer los datos del lince junto con las temperaturas mensuales
linceenero <- extract(tempenero, lincecoords_sin_na)
head(linceenero)

lincejulio <- extract(tempjulio, lincecoords_sin_na)
head(lincejulio)


 # Lo mismo pero con las temperaturas máximas
linceneromax <- extract(tmaxenero, lincecoords_sin_na)
head(linceneromax)

lincejuliomax <- extract(tmaxjulio, lincecoords_sin_na)
head(lincejuliomax)



##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España

  # st_crs(enppn) me indica que mi archivo "enppn" no tiene un sistema de
  # coordenadas asignado. No sé con exactitud a qué se debe esto, así que fuerzo
  # el sistema de coordenadas original (dato otorgado por mis compañeros)
st_crs(enppn) <- 4258
st_crs(enppn)

  # Convertir ambos archivos para que tengan el mismo sistema de coordenadas

enppn_sf <- st_as_sf(enppn, crs = 4258)
enppn_coords <- st_crs(enppn)

lince_sf <- st_as_sf(lincecoords_sin_na, coords = c("x", "y"), crs = 4258)
  # "st_transform(el que queremos cambair, crs(al tipo que le queremos cambiar))
  # "st_crs()" da informacion sobre las coordenadas

map_sf <- st_transform(lince_sf, enppn_coords)
st_crs(map_sf)

lincemap <- st_intersection(lince_sf, map_sf)
lincemap

plot(enerotmaxspain$tx_367)
lines(mappeninsula)
plot(enppn$geometry, add = TRUE)
plot(lincemap, add = TRUE, col = "red", size = 0.1)



##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981

meanenero <- mean(enerotmaxspain)
meanenero
plot(meanenero,
     main = "Temperaturas máximas de enero 1981",
     xlab = "Longitud",
     ylab = "Latitud")

meanjulio <- mean(juliotmaxspain)
meanjulio
plot(meanjulio,
     main = "Temperaturas máximas de julio 1981",
     xlab = "Longitud",
     ylab = "Latitud")


##' transforma los mapas con esas medias correspondientes a 1981 a la resolución,
##' extent
##' y proyección utilizada por los mapas mensuales que descargaste de github

tempenero
  # resolución de 0.1; extent: -9.35, 4.45, 35.95, 43.85;

cambiado <- resample(meanenero, tempenero)
  # No funciona porque meanenero es un SpatRaster, hay que cambiarlo a un 
  # RasterLayer

meanenero_raster <- as(meanenero, "Raster")
meanenero_ajust <- resample(meanenero_raster, tempenero)
meanenero_ajust

meanjulio_raster <- as(meanjulio, "Raster")
meanjulio_ajust <- resample(meanjulio_raster, tempenero)
meanjulio_ajust

## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas####
############################################################################# ##

### 3.a ####
##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y 
##' julio de 1981. 

## Hay que:
  # Seleccionar solo Doñana
  # Seleccionar solo los linces en Doñana
  # Seleccionar solo las temperaturas de enero 1981 en Doñana
  # Seleccionar solo las temperaturas de julio 1981 en Doñana

  # Seleccionar solo Doñana
unique(enppn$sitename) # Doñana es el nº 5 y está escrito como "Doñana"
doñanapn <- subset(enppn, sitename %in% "Doñana")
doñanapn

  # Coordenadas de lince solo en Doñana
lincedoñana <- st_intersection(lince_sf, doñanapn)
lincedoñana

  # Cogemos la temperatura de las coordenadas que coinciden con lince en Doñana
linceenerodoñana <- extract(tempenero, lincedoñana)
linceenerodoñana  # 174 observaciones

df_linceenerodoñana <- as.data.frame(x = linceenerodoñana)
str(df_linceenerodoñana)

df_enero <- mutate (df_linceenerodoñana, 
                    id = c(1:174),
                    mes = "enero",
                    temp = linceenerodoñana) %>% 
  dplyr::select(-linceenerodoñana)
str(df_enero)

lincejuliodoñana <- extract(tempjulio, lincedoñana)
lincejuliodoñana  # 174 observaciones

df_lincejuliodoñana <- as.data.frame(x = lincejuliodoñana)
str(df_lincejuliodoñana)

df_julio <- mutate (df_lincejuliodoñana, 
                    id = c(1:174),
                    mes = "julio",
                    temp = lincejuliodoñana) %>% 
  dplyr::select(-lincejuliodoñana)
str(df_julio)


df_enerojulio <- bind_rows(df_enero, df_julio) 
str(df_enerojulio)

mean(df_enero$temp)  # 18.37311
mean(df_julio$temp)  # 32.59493

## Gráficas juntos
x11()
ggplot(df_enerojulio, aes(x = mes, y = temp)) +
  geom_boxplot(aes(fill = mes)) +
  labs(
    x = "mes",
    y = "temperatura") +
  theme_light()

ggplot(df_enerojulio, aes(x = id, y = temp, colour = mes)) +
  geom_point() + geom_smooth(method = lm, aes(colour = mes)) +
  theme_light()

## Resultados
  # En julio hace más temperatura que en enero. Además, en junio hay más
  # variación de temperatura que en enero.


### 3.b ####
##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas

## Hay que:
  # Hacer un mapa que muestre la diferencia: tmax - tmean
  # Pasarlos a data.frame y restar los valores en una nueva columna
  # Un mapa a partir de la nueva columna

meanenero_ajust  # temp max enero
tempenero  # temp mensual enero

  # abs para que el resultado de la resta sea valor positivo, pues lo que nos
  # interesa es el valor de la diferencia
resta <- abs(meanenero_ajust - tempenero)
resta

plot(resta,
     main = "Diferencia de temperaturas",
     xlab = "Longitud",
     ylab = "Latitud")
lines(mappeninsula)
lines(CCAA_sf)

  # Localizar los 10 valores más pequeños --> los que tienen menor diferencia
valoresresta <- values(resta)
valoresmin <- sort(valoresresta)[1:10]
print(valoresmin)
indices_minimos <- order(valoresresta)[1:10]
coordenadas_minimas <- xyFromCell(resta, indices_minimos)

  # Ver las coordenadas y los valores en un data.frame
puntosmin <- data.frame(Coordenadas = coordenadas_minimas, Valor = valoresmin)

  # Colocar las coordenadas en el mapa
points(x = puntosmin$Coordenadas.x, y = puntosmin$Coordenadas.y, 
       col = "red",
       pch = 19, cex = 1)

## Vemos que las menores diferencias entre temperatura máxima y mensual de
  # enero de 1981 se reúnen en Galicia, Aragón y Andalucía

# Localizar los 10 valores más grandes --> los que tienen mayor diferencia
valoresresta <- values(resta)
valoresmax <- sort(valoresresta, decreasing = TRUE)[1:10]
print(valoresmax)
indicesmax <- order(valoresresta, decreasing = TRUE)[1:10]
coordsmax <- xyFromCell(resta, indicesmax)

# Ver las coordenadas y los valores en un data.frame
puntosmax <- data.frame(coordsmax, valor = valoresmax)

# Colocar las coordenadas en el mapa
points(x = puntosmax$x, y = puntosmax$y, 
       col = "blue",
       pch = 19, cex = 1)

## Vemos que las mayores diferencias entre temperatura máxima y mensual de
# enero de 1981 se reúnen en Cantabria y noreste de Cataluña.

###3.c ####
##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

diferencia <- abs(meanjulio_ajust - tempenero)
diferencia

plot(diferencia,
     main = "Diferencia de temperaturas",
     xlab = "Longitud",
     ylab = "Latitud")
lines(mappeninsula)
lines(CCAA_sf)

  # Sacar la geometría de los parques nacionales
  # Hacer un extract con las diferencias de temperatura
  # Sacar la media de todos los píxeles por parque nacional

diferenciaenp <- extract(diferencia,
                         enppn_sf)
diferenciaenp
  # Hay datos NULL, NA y Nan en elementos de la lista 12,13,14,15,16
lista <- diferenciaenp[-c(12,13,14,15,16)]
lista
lista[[2]] <- lista[[2]][!is.nan(lista[[2]])]

listamedia <- lapply(lista, mean)
listamedia

valormaximo <- max(unlist(listamedia))
print(valormaximo)  # Valor máximo de 23.7232 pertenece a nº4
enppn_sf$sitename  # El nº4 es Cabañeros

cab <- subset(enppn_sf, enppn_sf$sitename == "Cabañeros")
lines(cab, col = "red")

## Las diferencias fueron mayores en el parque nacional de Cabañeros


## 4. Bonus ####
##'############## #

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...

  # bonus
bonus <- raster("bonus.tif")
plot(bonus)
bonus


  # mapa 3b
resta
plot(resta,
     main = "Diferencia de temperaturas",
     xlab = "Longitud",
     ylab = "Latitud")
lines(mappeninsula)
lines(CCAA_sf)

  # multiplicarlos
mult <- bonus*resta
mult
plot(mult)
plot(resta)
plot(mult)
  # Se multiplican los valores

## Imagen propia
gato <- raster("gato2.tif")
class(gato)
ext(gato)

crs(mult)
crs(gato) <- "Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs "
crs(gato)

gatomiau <- resample(gato, mult)  # Ajustar resoluciones y dimensiones
gatomiau
plot(gatomiau)

gatogato <- mult*gatomiau
plot(mult)
plot(gatogato)

  # No se comprueba casi nada

ave <- raster("ave2.tif")
class(ave)
ext(ave)

crs(mult)
crs(ave) <- "Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs "
crs(ave)

avepio <- resample(ave, mult)  # Ajustar resoluciones y dimensiones
avepio
plot(avepio)

aveave <- mult*avepio
plot(mult)
plot(aveave)

  # La imagen no carga bien, por lo que sale cortada