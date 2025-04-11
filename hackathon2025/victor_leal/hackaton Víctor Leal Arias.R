############################################################################# ##
##'                                                                           ##
##'       "Seminario - Hackathon datos espacialmente explícitos en R"         ##
##'                                                                           ##
##'       Feb 2025                                                            ##
##'       by Ignacio Morales-Castilla                                         ##
##'                                                                           ##
##'       Alumno: Víctor Leal Arias                                           ##
##'                                                                           ##
############################################################################# ##


## limpiando el ambiente de trabajo
rm(list=ls())
options(stringsAsFactors = FALSE)


## establecer directorio de trabajo (wd)
getwd()  
setwd("~/Uni/R/Uni/hackaton")

# instalar paquetes
# install.packages("ggspatial")
# install.packages("googleway")
# install.packages("ncdf4")
# install.packages("mapSpain")
# install.packages("cowplot")

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
library(cowplot) # Añado librería cowplot para poder visualizar varios ggplot juntos más adelante.
theme_set(theme_bw())



# 1. Carga de datos espaciales ####
###############################  ##

##' busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1984
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 
##' 

## Abrir el archivo .nc ####
tmax <- rast("~/Uni/R/Uni/hackaton/archivos/tn_ens_mean_0.1deg_reg_1980-1994_v31.0e.nc")

##' lee datos de Parques Nacionales de España (en github)
##' 

## Leer datos de Parques Nacionales ####
parques <- st_read("~/Uni/R/Uni/hackaton/archivos/ENP.shp")


##' lee datos de temperaturas mensuales de España, para enero y julio (en github)
##' 

## Leer datos de temperaturas mensuales ####
tempespjan <- rast("~/Uni/R/Uni/hackaton/archivos/temp.Spain.jan07.tif")
tempespjul <- rast("~/Uni/R/Uni/hackaton/archivos/temp.Spain.jul07.tif")


##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif
##'

## Descarga de datos del lince ibérico ####
lince <- occ_data(scientificName = "Lynx pardinus")




# 2. Manipulación de datos ####
###########################  ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero 
##' de 1981. Repite para el mes de julio de 1981. Pista: tendras 
##' que recortar y enmascarar
##' los datos de E-OBS.

## Objetos con las temperaturas máximas de España ####
fechas <- time(tmax)
fechas <- as.Date(fechas)

enero81 <- which(format(fechas, "%Y-%m") == "1981-01") # Seleccionamos solo enero de 1981.
julio81 <- which(format(fechas, "%Y-%m") == "1981-07") # Seleccionamos solo julio de 1981.

tmax_enero81 <- subset(tmax, enero81) # Para Europa.
tmax_julio81 <- subset(tmax, julio81) # Para Europa.
# plot(tmax_enero81) # Para comprobar que se ha cargado correctamente.
# plot(tmax_julio81) # Para comprobar que se ha cargado correctamente.

spain <- esp_get_country() %>% vect()
tmax_enero81_esp <- crop(tmax_enero81, spain) %>% mask(spain) # Para España.
tmax_julio81_esp <- crop(tmax_julio81, spain) %>% mask(spain) # Para España.
# plot(tmax_enero81_esp) # Para comprobar que se ha recortado correctamente.
# plot(tmax_julio81_esp) # Para comprobar que se ha recortado correctamente.

##' Crea un subset con los datos de ocurrencia del lince, solo para España
##' 

## Subset con las ocurrencias del lince ####
linceesp <- subset(lince$data, country == "Spain")
table(linceesp$country) # Para comprobar que se ha seleccionado solo las ocurrencias de España.


##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981
##' 

## Datos de temperaturas mensuales para las ocurrencias del lince en España ####
lincepos <- data.frame(x=linceesp$decimalLongitude, y=linceesp$decimalLatitude)
lincepos # Hemos extraido las coordenadas del lince en España


lince_temp_jan <- extract(tempespjan, lincepos) # Para enero.
head(lince_temp_jan)

lince_temp_jul <- extract(tempespjul, lincepos) # Para julio.
head(lince_temp_jul)

lince_temp_enero91 <- extract(tmax_enero81_esp, lincepos) # Para enero de 1981.
lince_temp_enero91
head(lince_temp_enero91) # valores de tmax en cada mes.

lince_temp_junio91 <- extract(tmax_julio81_esp, lincepos) # Para julio de 1981.
lince_temp_junio91
head(lince_temp_junio91)


##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales
##' de España

## Ocurrencias del lince en Parques Nacionales ####
parquesnat <- subset(parques, figura_lp %in% c("Parque Nacional")) # Seleccionamos solo los Parques Nacionales.
# plot(st_geometry(parquesnat), main = "Parques Nacionales de España") # Para comprobar que el subset ha funcionado.

linceesp_no_na <- linceesp %>% 
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) # Eliminamos los valores NA que ocasionarían problemas más adelante.

lince_sf <- st_as_sf(linceesp_no_na, # Lo convertimos a objeto sf para poder manejar los datos con los comandos.
                     coords = c("decimalLongitude", "decimalLatitude"),
                     crs = st_crs(parquesnat)) # Y establecemos el mismo CRS que el de los Parques Naturales.

lince_parques <- st_intersection(lince_sf, parquesnat) # Hacemos ahora la intersección entre los linces y los Parques Naturales.
nrow(lince_parques)
head(lince_parques)

##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981

## Mapa raster de la media de las temperaturas máximas ####
# Para enero:
media_enero81 <- mean(tmax_enero81_esp) # Hacemos la media.

# Me propongo realizar los gráficos con ggplot2. Tras varios errores encuentro
# que para hacerlo de esta manera, debo introducir los datos como un dataframe,
# por lo que procedo a crearlo:
media_enero81_frame <- as.data.frame(media_enero81, xy = TRUE) %>%
  rename(long = x, lat = y) # Establecemos el nombre deseado a las variables.

raster_enero81 <- ggplot() +
  geom_raster(data = media_enero81_frame, aes(x = long, y = lat, fill = mean)) +
  scale_fill_viridis_c(option = "mako", name = "Temperatura (°C)") +
  labs(title = "Temperatura máxima media - Enero 1981") +
  xlab("Longitud") +
  ylab("Latitud") +
  theme_bw()
raster_enero81

# Para julio:
media_julio81 <- mean(tmax_julio81_esp) # Hacemos la media.

media_julio81_frame <- as.data.frame(media_julio81, xy = TRUE) %>%
  rename(long = x, lat = y) # Establecemos el nombre deseado a las variables.

raster_julio81 <- ggplot() +
  geom_raster(data = media_julio81_frame, aes(x = long, y = lat, fill = mean)) +
  scale_fill_viridis_c(option = "turbo", name = "Temperatura (°C)") +
  labs(title = "Temperatura máxima media - Julio 1981") +
  xlab("Longitud") +
  ylab("Latitud") +
  theme_bw()
raster_julio81

##' transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github

## Transformar mapas y proyección ####
# Para enero:
media_enero81_git <- project(media_enero81, crs(tempespjan), method = "bilinear") %>% # Cambiamos la proyección.
  resample(tempespjan, method = "bilinear") 

media_enero81_frame_git <- as.data.frame(media_enero81_git, xy = TRUE) %>% # Creamos el dataframe para incorporarlo a ggplot.
  rename(long = x, lat = y) # Establecemos el nombre deseado a las variables.

raster_enero81_git <- ggplot() +
  geom_raster(data = media_enero81_frame_git, aes(x = long, y = lat, fill = mean)) +
  scale_fill_viridis_c(option = "mako", name = "Temperatura (°C)") +
  labs(title = "Temperatura máxima media - Enero 1981 (Transformado)") +
  xlab("Longitud") +
  ylab("Latitud") +
  theme_bw()
raster_enero81_git

# Para ver la diferencia representamos primero uno y luego el otro:
raster_enero81
raster_enero81_git

# Para julio:
media_julio81_git <- project(media_julio81, crs(tempespjul), method = "bilinear") %>% # Cambiamos la proyección.
  resample(tempespjul, method = "bilinear")

media_julio81_frame_git <- as.data.frame(media_julio81_git, xy = TRUE) %>% # Creamos el dataframe para incorporarlo a ggplot.
  rename(long = x, lat = y) # Establecemos el nombre deseado a las variables.

raster_julio81_git <- ggplot() +
  geom_raster(data = media_julio81_frame_git, aes(x = long, y = lat, fill = mean)) +
  scale_fill_viridis_c(option = "turbo", name = "Temperatura (°C)") +
  labs(title = "Temperatura máxima media - Julio 1981 (Transformado)") +
  xlab("Longitud") +
  ylab("Latitud") +
  theme_bw()
raster_julio81_git

# Para ver la diferencia representamos primero uno y luego el otro:
raster_julio81
raster_julio81_git

# Para ver mejor la diferencia representamos los 4 mapas juntos:
plot_grid(raster_enero81, raster_enero81_git, raster_julio81, raster_julio81_git, 
          ncol = 2, nrow = 2) # Comando extraído de la librería cowplot.

# 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ####
######################################################################################  ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 

## Actividad 3.a. ####
parquesnat$sitename # Para ver los nombres de los Parques Nacionales.
donana <- subset(parquesnat, sitename == "Doñana") # Hacemos un cubset para el Parque Nacional de Doñana.

lince_donana <- st_intersection(lince_sf, donana) # Hacemos la intersección entre los linces y el Parque Nacional de Doñana.

donana_coord <- st_coordinates(lince_donana) #Sacamos las coordenadas de Doñana para poder extraer la temperatura en ese punto.

donana_temp_enero <- extract(tmax_enero81_esp, donana_coord) # Extraemos la temperatura de enero en Doñana.
donana_temp_julio <- extract(tmax_julio81_esp, donana_coord) # Extraemos la temperatura de julio en Doñana.

media_enero_donana <- colMeans(donana_temp_enero) # Hacemos la media de cada día de enero.
media_julio_donana <- colMeans(donana_temp_julio) # Hacemos la media de cada día de julio.

# De nuevo, al querer hacer un ggplot(), es necesario hacer un dataframe:
donana_frame <- data.frame(tmax_enero = media_enero_donana, # Creamos el dataframe
  tmax_julio = media_julio_donana)

# Creamos un diagrama de dispersión para comparar las temperaturas:
donana_comp <- ggplot(donana_frame, aes(x = tmax_enero, y = tmax_julio)) +
  geom_point(color = "darkorange", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(
    x = "T. máx. Enero 1981 (°C)",
    y = "T. máx. Julio 1981 (°C)",
    title = "Temperatura máxima del lince en Doñana"
  ) +
  theme_bw()
donana_comp

# Los puntos don los días del mes, del 1 al 31, donde se compara la temperatura
# máxima media de julio y enero.

# Compararemos también estas temperaturas mediante la correlación:
cor(donana_frame$tmax_enero, donana_frame$tmax_julio, use = "complete.obs") # Calculamos la correlación

# Finalmente, compararemos las temperaturas medias de cada mes para ver las
# diferencias:
media_temp_enero <- mean(as.matrix(donana_temp_enero), na.rm = TRUE) # Hacemos la media para la temperatura de enero.
media_temp_enero

mdeia_temp_julio <- mean(as.matrix(donana_temp_julio), na.rm = TRUE) # Hacemos la media para la temperatura de julio.
mdeia_temp_julio

# Juntamos las medias de enero y julio para poderlo comparar mejor:
media_comparacion <- data.frame(Mes = c("Enero", "Julio"), # Para ello, creamos un dataframe.
  Temperatura_Media = c(media_temp_enero, mdeia_temp_julio))
media_comparacion

##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas

## Actividad 3.b. ####
diferencia_enero <- abs(media_enero81_git - tempespjan) # Usamos abs() para obtener el valor absoluto y eliminar así los números negativos.

diferencia_enero_frame <- as.data.frame(diferencia_enero, xy = TRUE) %>% # Creamos un dataframe.
  rename(long = x, lat = y) # Nombramos a las variables por el nombre deseado.
names(diferencia_enero_frame)[3] <- "diferencia" # Cambiamos el nombre "mean" por uno mejor ("diferencia").

# Generamos el mapa con las diferencias:
mapa_diferencias_enero <- ggplot(diferencia_enero_frame) +
  geom_raster(aes(x = long, y = lat, fill = diferencia)) +
  scale_fill_gradient2(low = "black", mid = "#020024", high = "#00D4FF",
                       midpoint = 0, name = "Diferencia (°C)") +
  labs(
    title = "Diferencias de temperaturas máximas en enero",
    x = "Longitud",
    y = "Latitud") +
  theme_bw()
mapa_diferencias_enero 

# Las zonas más claras corresponden con las mayores diferencias, mientras que
# las zonas más oscuras corresponden con las menores diferencias.

# Parece que en el norte de España es donde menos diferencia de temperatura ha
# habido, mientras que la zona central y la zona sur presentan, en general,
# grandes diferencias.

##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?

## Actividad 3.c. ####  
diferencia_julio81_enero <- abs(media_julio81_git - tempespjan) # Usamos abs() para obtener el valor absoluto y eliminar así los números negativos.

diferencia_julio81_enero_frame <- as.data.frame(diferencia_julio81_enero, xy = TRUE) %>% # Creamos un dataframe.
  rename(long = x, lat = y) # Nombramos a las variables por el nombre deseado.
names(diferencia_julio81_enero_frame)[3] <- "diferencia" # Cambiamos el nombre "mean" por uno mejor ("diferencia").

# Creamos el mapa con las diferencias:
mapa_diferencias_julio81_enero <- ggplot(diferencia_julio81_enero_frame) +
  geom_raster(aes(x = long, y = lat, fill = diferencia)) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "#590000",
                       midpoint = 0, name = "Diferencia (°C)") +
  labs(
    title = "Diferencias de temperaturas máximas en julio",
    x = "Longitud",
    y = "Latitud") +
  theme_bw() +
  geom_sf(data = parquesnat, fill = NA, color = "black", size = 0.4) + # Añadimos las líneas de los Parques Nacionales al mapa.
  coord_sf(
    xlim = c(-10, 5),
    ylim = c(36, 44)
  )
mapa_diferencias_julio81_enero

# El color oscuro representa los mayores cambios y el amarillo los menores.

mapa_diferencias_julio81_enero +
  annotate("rect", xmin = -5, xmax = -4, ymin = 39, ymax = 40,
           color = "blue", fill = NA, size = 2, linetype = "solid") +
  annotate("rect", xmin = -6.5, xmax = -5.5, ymin = 39.5, ymax = 40,
           color = "green", fill = NA, size = 2, linetype = "solid")

# Visualmente parece ser que las mayores diferencias se encuentran en el 
# Parque Nacional de MonfragÜe y en el Parque Nacional de Cabañeros.

# Para asegurarnos, lo comprobamos numéricamente:
parquesnat_crs <- vect(st_transform(parquesnat, crs(diferencia_julio81_enero))) # Nos aseguramos que los CRS coincidan.

medias_parques <- terra::extract(diferencia_julio81_enero, parquesnat_crs, fun = mean, na.rm = TRUE) # Extraemos la media de las diferencias de cada Parque Nacional.
medias_parques

medias_parques$nombre <- parquesnat$sitename # Añadimos los nombres de los parques para saber a qué Parque Nacional corresponde cada valor.

medias_ordenadas <- medias_parques[order(-medias_parques[[2]]), ] # Ordenamos de mayo a menor los valores para ver de un vistazo rápido el Parque Nacional con mayor diferencia.

head(medias_ordenadas) # Visualizamos los datos.

# Por lo que podemos observar, el Parque Nacional de Monfragüe es el que más
# diferencias de temperatura máxima ha sufrido.


# 4. Bonus ####
############ ##

##' En unos días habrá un nuevo mapa cargado en la carpeta de datos
##' de github. Cárgalo en R, multiplícalo por el mapa que has generado
##' en el apartado 3b, y visualízalo. ¿Qué ha pasado? ¿Serías capaz de
##' generar un resultado parecido, pero con una imagen propia? Recuerda
##' que una fotografía, no deja de ser un mapa raster...

# Cargamos el archivo:
bonus <- rast("~/Uni/R/Uni/hackaton/archivos/bonus.tif")

# Multiplicamos los mapas:
bonus_x_3b <- bonus * diferencia_enero # Multiplico por diferencia_enero porque no se puede multiplicar un SpatRaster con un ggplot.

bonus_x_3b_frame <- as.data.frame(bonus_x_3b, xy = TRUE) %>% # Creamos un dataframe
  rename(long = x, lat = y)
names(bonus_x_3b_frame)[3] <- "valor"

mapa_bonus <- ggplot(bonus_x_3b_frame) +
  geom_raster(aes(x = long, y = lat, fill = valor)) +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "Bonus multiplicado por el mapa 3.b.",
    x = "Longitud", y = "Latitud"
  ) +
  theme_bw()
mapa_bonus

# El mapa esta "roto". Con la multiplicación no se pueden observar diferencias
# en él.


# No entiendo muy bien el concepto de imagen propia, por lo que exporto una
# imagen de un ggplot hecho anteriormente: mapa_diferencias_enero:
# ggsave("prueba.png", plot = mapa_diferencias_enero, width = 10, height = 6, dpi = 400)

# Posteriormente cargo la imagen como un objeto raster.
grafico_raster <- rast("prueba.png")
crs(grafico_raster)

grafico_raster_res <- resample(grafico_raster, diferencia_enero) # Ajusto la resolución 
mapa_multiplicado <- grafico_raster_res * diferencia_enero # Multiplico los mapas

mapa_multiplicado_frame <- as.data.frame(mapa_multiplicado, xy = TRUE) # Creo un dataframe
names(mapa_multiplicado_frame)[3] <- "valor"

# Lo represento con ggplot:
ggplot(mapa_multiplicado_frame) + 
  geom_raster(aes(x = x, y = y, fill = valor)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Mapas multiplicados",
    x = "Longitud", y = "Latitud"
  ) +
  theme_minimal() +
  coord_sf(
    xlim = c(-10, 5),
    ylim = c(36, 44)
  )
# No se representa correctamente

# Pruebo a hacerlo con plot() para ver como sale (sin dataframe, porque no es ggplot):
plot(mapa_multiplicado)

# Parece que sigue saliendo cortado. 
