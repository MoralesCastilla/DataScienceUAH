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
setwd("C:/Users/laura/Desktop/universidad/tercero/2 cuatri/GAR/hackathon")

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
tmax_diarias80_84 <- rast("tx_ens_mean_0.25deg_reg_1980-1994_v31.0e.nc")


##' lee datos de Parques Nacionales de España (en github)
##' 
enp <- st_read("C:/Users/laura/Desktop/universidad/tercero/2 cuatri/GAR/hackathon")

##' lee datos de temperaturas mensuales de España, para enero y julio (en github)
##' 
tenero <- rast("temp.Spain.jan07.tif")

tjulio <- rast("temp.Spain.jul07.tif")



##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##' 
library(rgbif)
lince <- occ_search(scientificName = "Lynx pardinus")
lince


## 2. Manipulación de datos ##
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

#Mes de enero
tmax <- rast("tx_ens_mean_0.25deg_reg_1980-1994_v31.0e.nc")
tmax
tmax_81 <-  subset(tmax, 365:730)

fechas <- time(tmax_81)
fechas <- as.Date(fechas)
format(fechas, "%m") #para meses
fech_en <- which(format(fechas, "%m")=="01")
fech_en

tmax_en81 <- subset(tmax_81, fech_en)

#Mes de julio
tmax <- rast("tx_ens_mean_0.25deg_reg_1980-1994_v31.0e.nc")
tmax_81 <-  subset(tmax, 365:730)
fechas <- time(tmax_81)
fechas <- as.Date(fechas)
format(fechas, "%m") #para meses
fech_jul <- which(format(fechas, "%m")=="07")
fech_jul

tmax_jul81 <- subset(tmax_81, fech_jul)

#hacemos el mapa de España para enero
library(mapSpain)
CCAA_sf <- esp_get_ccaa()
tmaxenesp <- crop(tmax_en81, CCAA_sf)
plot(tmaxenesp[[1]]) #1 porque enero es el primer mes
lines(CCAA_sf)

#hacemos el mapa de España para julio
tmaxjulesp <- crop(tmax_jul81, CCAA_sf)
plot(tmaxjulesp[[7]]) #7 porque julio es el septimo mes
lines(CCAA_sf)


##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 
lince
linceespaña <- subset(lince$data, country == "Spain")
linceespaña

##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981

#temperatura mensual
tenero_lince <- extract(tenero, data.frame(linceespaña$decimalLongitude,
                                                 linceespaña$decimalLatitude))

tjulio_lince <- extract(tjulio, data.frame(linceespaña$decimalLongitude,
                                                  linceespaña$decimalLatitude))

tenero_lince
tjulio_lince

#temperaturas maximas
tmaxenero_lince <- extract(tmaxenesp, data.frame(linceespaña$decimalLongitude,
                                              linceespaña$decimalLatitude))

tmaxjulio_lince <- extract(tmaxjulesp, data.frame(linceespaña$decimalLongitude,
                                            linceespaña$decimalLatitude))

tmaxenero_lince
tmaxjulio_lince

##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España
##' 
ppnn <- subset(enp, figura_lp %in% c("Parque Nacional"))
lines(ppnn, col="red", lwd=1.5) 

linceespaña2 <- linceespaña %>% filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))
linces_sf <- st_as_sf(linceespaña2, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
ppnn_sf <- st_transform(ppnn, st_crs(linces_sf))

# Ocurrencias del lince dentro de los Parques Nacionales de España
linceespaña2_ppnn <- st_filter(linces_sf, ppnn_sf)

nrow(linceespaña2_ppnn) 



##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981
##' 
#para Europa
tmedia_enero1 <- mean(tmax_en81)
tmedia_julio1 <- mean(tmax_jul81)

#unimos las medias
mean81_en_jul1 <- c(tmedia_enero1, tmedia_julio1)

plot(mean81_en_jul1, main = c("Tmax enero 1981", "Tmax julio 1981"))

#para España
tmedia_enero2 <- mean(tmaxenesp)
tmedia_julio2 <- mean(tmaxjulesp)

#unimos las medias
mean81_en_jul2 <- c(tmedia_enero2, tmedia_julio2)

plot(mean81_en_jul2, main = c("Tmax enero 1981", "Tmax julio 1981"))

##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github
##'   
tmes_enero <- rast("temp.Spain.jan07.tif")
tmes_julio <- rast("temp.Spain.jul07.tif")

crs_enero <- crs(tmes_enero)  
res_enero <- res(tmes_enero)   
ext_enero <- ext(tmes_enero)
crs_julio <- crs(tmes_julio)  
res_julio <- res(tmes_julio)   
ext_julio <- ext(tmes_julio)


tmedia_enero_proyeccion <- project(tmedia_enero2, tmes_enero, method = "bilinear")
tmedia_julio_proyeccion <- project(tmedia_julio2, tmes_julio, method = "bilinear")

plot(tmedia_enero_proyeccion, main = "Tmedia enero 1981 ajustada")
plot(tmedia_julio_proyeccion, main = "Tmedia julio 1981 ajustada")


## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ##
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 

tmaxenero_lince <- extract(tmaxenesp, data.frame(linceespaña$decimalLongitude,
                                                 linceespaña$decimalLatitude))

tmaxjulio_lince <- extract(tmaxjulesp, data.frame(linceespaña$decimalLongitude,
                                                  linceespaña$decimalLatitude))

tmax_lince_total <- merge(tmaxenero_lince, tmaxjulio_lince, by = "row.names", all =TRUE) #unimos las temperaturas con merge
colnames(tmax_lince_total) <- c("ID", "temp_enero", "temp_julio")

ppnn_d <- subset(ppnn, sitename == "Doñana") #solo queremos la zona de Doñana
lines(ppnn_d, col="black", lwd=1.5)

#con otro merge unimos Doñana y las temperaturas del lince en enero y julio (totales)
lince_doñana <- merge(tmax_lince_total, ppnn_d)

#correlacion
cor(lince_doñana$temp_enero, lince_doñana$temp_julio, use = "complete.obs")

#grafico de dispersion
library(ggplot2)
ggplot(lince_doñana, aes(x = temp_enero, y = temp_julio)) +
  geom_point(color = "#1b7837", size = 2.5, alpha = 0.8) +  # puntos en verde
  geom_smooth(method = "lm", color = "#d73027", se = FALSE, linetype = "dashed") +  # línea de tendencia roja
  labs(
    title = "Temperatura del lince en Doñana (1981)",
    subtitle = "Comparación entre enero y julio",
    x = "Temperatura en enero (ºC)",
    y = "Temperatura en julio (ºC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "#555555"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas
library(terra)
CCAA_sf <- esp_get_ccaa()
tmax_enero_81_esp <- crop(tmax_en81, CCAA_sf) #Tmax de enero 1981 en España

#hacemos las medias
tmax_enero_81_esp_media <- mean(tmax_enero_81_esp)
tenero_media <- mean(tenero)

#mismo CSR
tmax_en81_esp_media <- project(tmax_enero_81_esp_media, crs(tenero))

#representamos
par(mfrow = c(1, 2))  #para mostrar ambos mapas a la vez
plot(tmax_en81_esp_media[[1]], main = "Tmax enero 1981")
plot(tenero_media, main = "Tmedias enero")

#ahora hacemos la diferencia de temperatura
crs(tmax_enero_81_esp_media)  
crs(tenero_media)

#mismo CSR
tmax_enero_81_esp_media <- project(tmax_enero_81_esp_media, crs(tenero_media))
tmax_enero_81_esp_media <- resample(tmax_enero_81_esp_media, tenero_media, method = "bilinear")

#diferencia 
diferencia <- tmax_enero_81_esp_media - tenero_media

df_diferencia <- as.data.frame(diferencia, xy = TRUE)

# Renombrar la columna de valores para que tenga un nombre adecuado
names(df_diferencia) <- c("x", "y", "diferencia_temperaturas")

# Verificar los primeros datos
head(df_diferencia)

# Graficar la diferencia con ggplot
ggplot() +
  geom_raster(data = df_diferencia, aes(x = x, y = y, fill = diferencia_temperaturas)) +  # Usar la nueva columna "diferencia_temperaturas"
  scale_fill_viridis_c() +
  labs(
    title = "Diferencia de temperaturas (Tmax 1981 - Media enero)",
    subtitle = "Comparación de la temperatura máxima de enero de 1981 con la media",
    fill = "Diferencia (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )
#en el mapa se observa que el la zona noreste la diferencia es mayor, mientras que en la
#suroeste es mas pequeña


##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   
##' 
##
library(terra)
library(sf)
library(mapSpain)
library(dplyr)
library(ggplot2)

CCAA_sf <- esp_get_ccaa()
tmax_julio_81_esp <- crop(tmax_jul81, CCAA_sf) #Tmax de julio 1981 en España
tmax_julio_81_esp_media <- mean(tmax_julio_81_esp)
tenero_media <- mean(tenero)

#mismo CSR
tmax_julio_81_esp_media <- project(tmax_julio_81_esp_media, crs(tenero_media))
tmax_julio_81_esp_media <- resample(tmax_julio_81_esp_media, tenero_media, method = "bilinear")

#hacemos la diferencia
diferencia_julio <- tmax_julio_81_esp_media - tenero_media
df_diferencia_julio <- as.data.frame(diferencia_julio, xy = TRUE)

names(df_diferencia_julio) <- c("x", "y", "diferencia_julio")

df_diferencia_total <- merge(df_diferencia, df_diferencia_julio, by = c("x", "y"))

df_enero <- as.data.frame(diferencia, xy = TRUE)
df_julio <- as.data.frame(diferencia_julio, xy = TRUE)

names(df_enero) <- c("x", "y", "diferencia_enero") #renombramos columnas
names(df_julio) <- c("x", "y", "diferencia_julio") #renombramos columnas

df_diferencia_total <- merge(df_enero, df_julio, by = c("x", "y"))

nrow(df_diferencia_total)
#diferencia entre enero y julio
df_diferencia_total$diferencia_total <- df_diferencia_total$diferencia_julio - df_diferencia_total$diferencia_enero



Diferencia_raster <- rast(df_diferencia_total, crs = "EPSG:4326", 
                          extent = ext(df_diferencia_total))

diferencia_raster <- rast(df_diferencia_total, type = "xyz", crs = "EPSG:4326")
plot(diferencia_raster)

ppnn <- st_transform(ppnn, crs(diferencia_raster))

ppnn_diferencia <- terra::extract(diferencia_raster, ppnn)
ppnn_diferencia

ppnn_diferencia$sitename <- ppnn$sitename[match(ppnn_diferencia$ID, ppnn$ID)]

parquenacional <- ppnn$sitename[which.max(ppnn_diferencia$mean)]
cat(parquenacional, "es el parque nacional con mayores diferencias")

## 4. Bonus ##
#################

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...






