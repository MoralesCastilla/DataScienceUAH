################################################################# #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'       alumno: Juan Julián Martínez
##'
################################################################# #


## Limpiamos el ambiente de trabajo de variables de otros trabajos.
rm(list=ls())
options(stringsAsFactors = FALSE)


## Establecemos el directorio de trabajo (wd)
getwd()  
setwd("C:/Users/julia/OneDrive/Documentos/GitHub/DataScienceUAH/hackathon2025/juan_julian/")


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
theme_set(theme_bw())



## 1. Carga de datos espaciales ##
############################### ##

##' busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 
##' 

Puede q sea del 1970 en vez de 1980


##' lee datos de Parques Nacionales de España (en github)
##' 



##' lee datos de temperaturas mensuales de España, para enero y julio (en github)
##' 



##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##' 





## 2. Manipulación de datos ##
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero de 1981.
##'  Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.


##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 


##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981
##' 
2 fuentes de datos de clima: datos de temperatura media de enero y de junio (los que están en el Github)
 y generar los datos de las temperaturas bajándonos las cosas.


##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España
Tanbién con el nombre del parque nacional

##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981
calcular un solo mapa a partir de los dos tipos de datos. 

##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github

igualar sistema de proyecciones



## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ##
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 
diferencia entre enero y julio

##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas
mapas entre las temperaturas bajadas de iobs y de las descargadas de Git

##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

si no especifica el año son los datos de Git

## 4. Bonus ##
#################

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...


multiplicar literalmente con *
transformar los NA en un valor q no de error (por ejemplo 0)

# Subir las cosas a git
git status
git pull
git add # y el directorio del archivo que quiero subir
git commit -m "" # el -m es para poner un mensaje y se pone en las comillas
git push
git status
