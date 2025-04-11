################################################################# #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'
################################################################# #


## limpiando el ambiente de trabajo
rm(list=ls())
options(stringsAsFactors = FALSE)


## establecer directorio de trabajo (wd)
getwd()  
setwd("C:/Users/icorr/Desktop/Uni/GAR")

## cargar paquetes
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
library (tidyterra)

## 1. Carga de datos espaciales ##
############################### ##

##' busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 
##' 

tmax <- rast("Tmax80.nc")


##' lee datos de Parques Nacionales de España (en github)
##' 

enp <- st_read("C:/Users/icorr/Desktop/Uni/GAR/ENP.shp")
ppnn <- subset(enp, figura_lp %in% c("Parque Nacional"))

##' lee datos de temperaturas mensuales de España, para enero y julio (en github)
##' 

tjul <- rast("temp.Spain.jul07.tif")
tjan <- rast("temp.Spain.jan07.tif")

##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##' 

lynx <- occ_data(scientificName = "Lynx pardinus",
                 limit = 5000)



## 2. Manipulación de datos ##
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

años <- 1980:1994

# Definimos la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Creamos una lista donde almacenaremos los datos por año
tmedia_por_año <- list()

# Extraemos datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  
  tmedia_por_año[[as.character(años[i])]] <- subset(tmax, inicio:fin)
  inicio <- fin + 1  
}

# Obtenemos datos del año especifico
tmax81 <- tmedia_por_año[["1981"]]

# Pasamos a formato fecha las fechas
fechas <- as.Date(time(tmax81))
#En teoria el as.Date no es necesario, pero lo pongo por si acaso.


# Seleccionamos los meses especificos
tmaxjul <-subset(tmax81, which(format(fechas, "%m") == "07")) 
tmaxjan <- subset(tmax81, which(format(fechas, "%m") == "01"))

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa


tmaxjulspain <- crop(tmaxjul, CCAA_sf)
tmaxjanspain <- crop(tmaxjan, CCAA_sf)

##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 

lynx_spain <- subset(lynx$data, countryCode == "ES")


##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981


lynx_jul <- extract(tjul, data.frame(x = lynx_spain$decimalLongitude,
                                     y = lynx_spain$decimalLatitude))
lynx_jan <- extract(tjan, data.frame(x = lynx_spain$decimalLongitude,
                                     y = lynx_spain$decimalLatitude))
lynx_jul81 <- extract(tmaxjulspain, data.frame(x = lynx_spain$decimalLongitude,
                                               y = lynx_spain$decimalLatitude))
lynx_jan81 <- extract(tmaxjanspain, data.frame(x = lynx_spain$decimalLongitude,
                                               y = lynx_spain$decimalLatitude))

##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España
lynx_spain <- lynx_spain[!is.na(lynx_spain$decimalLongitude) & !is.na(lynx_spain$decimalLatitude), ]
lynx_sf <- st_as_sf(lynx_spain, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
lynx_sf <- st_transform(lynx_sf, st_crs(ppnn))
lynx_ppnn <- st_intersection(lynx_sf, ppnn)




##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981

tmed81 <- (tmaxjulspain+tmaxjanspain)/2

##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github

#Se puede usar crop porque es menor que el raster tjul
tmedia81 <- crop(tmed81, ext(-9.35, 4.45, 35.95, 43.85))
## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ##
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 

donana <- ppnn[ppnn$sitename == "Doñana", ] 
lynx_donana <- st_intersection(lynx_sf, donana)
lynx_donana <- st_transform(lynx_donana, st_crs(tmaxjanspain))  
donana_jul <- extract(tmaxjulspain, lynx_donana)
donana_jan <- extract(tmaxjanspain, lynx_donana)

jan81day <- colMeans(donana_jan, na.rm = TRUE)
jul81day <- colMeans(donana_jul, na.rm = TRUE)

df <- data.frame(jan = c(jan81day), jul = (jul81day))
df <- filter(df[-1,])
ggplot(df, aes(x = jan, y = jul)) + geom_smooth() + geom_point()

## 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas


t81 <- resample(tmaxjanspain, tjan)
resta <- t81 - tjan
resta_media <- app(resta, fun = mean, na.rm = TRUE)
plot(resta_media)

# Las diferencias son más grandes en los colores amarillo y morado, y más pequeñas en los colores verde y azul.

##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

c3 <- resample(tmaxjulspain, tjan)
resta2 <- c3 - tjan
resta_media2 <- app(resta2, fun = mean, na.rm = TRUE)
plot(resta_media2)

range(resta2, na.rm = TRUE)

difppnn <- extract(resta_media2, ppnn) 
# media <- sapply(difppnn, function(x) media(x, na.rm = TRUE))

parque_final <- ppnn$sitename[which.max(difppnn$mean)]
cat(parque_final, "es el parque nacional con mayores diferencias")

# lines(CCAA_sf, col = "black", lwd = 1)
plot(resta_media2)
lines(ppnn)
centroides <- st_centroid(ppnn)
coordenadas_centroides <- st_coordinates(centroides)
text(coordenadas_centroides[, 1], coordenadas_centroides[, 2], labels = ppnn$sitename, col = "red", cex = 0.3, pos = 4)


## 4. Bonus ##
#################

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...

bonus <- rast("bonus.tif")

ext(resta_media)
ext(bonus)

multi <- bonus*resta_media
plot(bonus)
plot(multi)
plot(resta_media)

egg <- data.frame(bonus$easter_egg_1)


val_multi <- values(multi)
val_resta <- values(resta_media)

correlacion <- cor(val_multi, val_resta, use = "complete.obs")
cat("Correlación entre bonus y resta_media:", correlacion, "\n")

plot(val_resta, val_multi)

#No hay correlacion entre los valores de ambos mapas, y no encuentro 
#la manera de obtener el filtro que se le está aplicando a bonus para
#obtener los datos 