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
setwd("~/sandbox/")


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

tmax <- rast("climate/tx_ens_mean_0.25deg_reg_1980-1994_v31.0e.nc")





##' lee datos de Parques Nacionales de España (en github)
##' 

enps <- st_read("data/ENP.shp")
plot(enps$geometry)


##' lee datos de temperaturas mensuales de España, para enero y julio (en github)
##' 

tempene07 <- rast("climate/temp.Spain.jan07.tif")
tempjul07 <- rast("climate/temp.Spain.jul07.tif")

plot(tempene07)
plot(tempjul07)


##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##' 
library(rgbif)

lince <- occ_data(scientificName = "Lynx pardinus",
                    #country = "Spain",
                    limit = 5000)

lince <- lince$data




## 2. Manipulación de datos ##
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

tmax81 <- subset(tmax, 367:731)
tmax81ene <- subset(tmax81, 
                    format(time(tmax81),"%m")=="01")
tmax81jul <- subset(tmax81, 
                    format(time(tmax81),"%m")=="07")

tmax81enespain <- crop(tmax81ene, tempene07)
tmax81julspain <- crop(tmax81jul, tempene07)

plot(tmax81julspain[[1]])


##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 

lincespain <- subset(lince, country=="Spain")

##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981
##' 
coordlince <- data.frame(lon=lincespain$decimalLongitude,
                         lat=lincespain$decimalLatitude)
coordlince <- coordlince[complete.cases(coordlince),]

templinceene <- terra::extract(tempene07,coordlince)
templincejul <- terra::extract(tempjul07,coordlince)

tmaxlinceene <- terra::extract(tmax81enespain,coordlince)
tmaxlincejul <- terra::extract(tmax81julspain,coordlince)



##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España
ppnn <- subset(enps, figura_lp=="Parque Nacional")
lines(ppnn, col="black",lwd=1.5)
ppnnvect <- vect(ppnn)

linceinppnn <- extract(ppnnvect, coordlince)

linceinppnnnona <- linceinppnn[!is.na(linceinppnn$sitename),]
table(linceinppnnnona$sitename)

coordlinceinppnn <- coordlince[!is.na(linceinppnn$sitename),]
points(coordlinceinppnn,col="red",pch=19,cex=0.7)

linceppnn <- cbind(coordlinceinppnn, 
                   parque=linceinppnnnona$sitename)

##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981 y de julio de 1981

tmaxene81 <- app(tmax81enespain, mean)  
tmaxjul81 <- app(tmax81julspain, mean)  

  

##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github

plot(tmaxene81) 
plot(tempene07) 
tmaxene81trans <- resample(tmaxene81,tempene07)
values(tmaxene81trans)[is.na(values(tempene07))]<-NA
plot(tmaxene81trans)

tmaxjul81trans <- resample(tmaxjul81,tempene07)
values(tmaxjul81trans)[is.na(values(tempene07))]<-NA
plot(tmaxjul81trans)



## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ##
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 
coordslincedonana <- subset(linceppnn, parque == "Doñana")
coordslincedonana.xy <- coordslincedonana[,1:2] 
tmaxlinceene81 <- terra::extract(tmaxene81trans,
                                 coordslincedonana.xy)
tmaxlincejul81 <- terra::extract(tmaxjul81trans,
                                 coordslincedonana.xy)

plot(jitter(tmaxlinceene81$mean,5),
     jitter(tmaxlincejul81$mean,5),
     xlab="temperaturas máximas enero 1981",
     ylab="temperaturas máximas julio 1981",
     pch=19, col=adjustcolor("black",0.1), cex=0.7)
abline(lm(tmaxlincejul81$mean~tmaxlinceene81$mean),
       lty=2, col="grey")
cor(tmaxlincejul81$mean,tmaxlinceene81$mean,
    use="pairwise.complete.obs")


##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas

comparetempjan <- tmaxene81trans - tempene07
plot(comparetempjan)
plot(hist(values(comparetempjan)))
mean(values(comparetempjan),na.rm=T)

mindiffs <- comparetempjan > 1 | comparetempjan < -1
plot(mindiffs) # en las áreas moradas, las diferencias son inferiores a 1 gradoC

maxdiffs <- comparetempjan > 4 | comparetempjan < -4
plot(maxdiffs) # en las áreas amarillas, las diferencias son mayores a 4 gradosC


##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

diffjul81meanjan <- tmaxjul81trans - tempene07

tempdiffsppnn <- extract(diffjul81meanjan, ppnnvect)
nombresppnn <- data.frame(ID=1:nrow(ppnnvect),
                          parque=ppnnvect$sitename)
tempdiffsppnn <- merge(tempdiffsppnn,nombresppnn)
tempdiffeachppnn <- aggregate(mean~parque, data=tempdiffsppnn, mean, na.rm=T)
tempdiffeachppnn[order(tempdiffeachppnn$mean),]
#la mayor diferencia está en Cabañeros

## 4. Bonus ##
#################

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...


## os comento a continuación cómo generar una imagen como la del bonus para llegar a la solución del "easter egg"

easteregg <- rast("data/easter_egg.tif")[[1]] # cargamos la imagen con la que queremos "jugar"
plot(easteregg)

easteregg <- flip(easteregg) # hay que rotar la imagen para que el paquete terra la lea e interprete correctamente
plot(easteregg)

crs(easteregg) <- crs(comparetempjan) # le damos una proyección y sistema de coordenadas de referencia
ext(easteregg) <- ext(-10,5,35,45) # definimos el extent (usando el de la Península)
plot(easteregg)

eastereggtrans <- resample(easteregg, comparetempjan) #la imagen que hemos cargado, tiene más resolución que nuestro mapa
                                                      # resampleamos esa imagen a la resolución de nuestro mapa
plot(eastereggtrans)

bonus = eastereggtrans/comparetempjan  # dividimos la imagen que hemos creado entre la imagen por la que la vamos a multiplicar 
plot(bonus)

writeRaster(bonus,filename = "data/bonus.tif") # guardamos el resultado

voila = bonus * comparetempjan # comprobamos que ha funcionado
plot(voila)

