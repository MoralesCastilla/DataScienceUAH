#'##################################################################
#   Asignatura Transversal, Universidad de Alcalá
#   Asignatura: Ciencia de datos práctica
#
#   Script para la teoría/seminario Módulos IV / V: 
#     Obtención, tratamiento y visualización de datos geográficamente explícitos
#
#   Autor: Ignacio Morales-Castilla
#   Fecha: Marzo 2022
#'##################################################################
#'##################################################################


#'########
#### PASO 1: PREPARACIÓN DEL ENTORNO DE TRABAJO ####
#'########

# 1A ## Eliminamos objetos que pudiese haber en la memoria de RStudio
rm(list=ls())

# 1B ## Creamos un directorio de trabajo para esta práctica en el ordenador, y
# asignamos este directorio desde el que vamos a trabajar con 'setwd()':
# pega entre comillas la dirección física de esa carpeta en tu ordenador
# (atención a la dirección de las barras /)
# o seleccionala directamente usando el atajo Ctrl+Shift+H
setwd("C:/Usuarios/pon tu direccion aqui/")
setwd('~/MEGA/Work_UAH_BeaGal/teaching/2021/BigData en Ciencias Naturales/')



# 1C ## Instalamos y cargamos los paquetes que utilizaremos durante los análisis
install.packages("raster")
install.packages("maptools")
install.packages("sp")
install.packages("rgdal")
install.packages("dismo")
install.packages("spam")
install.packages("rworldmap")
install.packages("rworldxtra")
install.packages("jsonlite")
install.packages("rgeos")
install.packages("mapSpain")
install.packages("ggmap")
install.packages('ggplot2')
install.packages('maps')
install.packages('mapdata')
install.packages("marmap")
install.packages("mapproj")
install.packages("sf")
install.package("slippymath")
install.package("ggspatial")


library(raster)
library(maptools)
library(sp)
library(sf)
library(rgdal)
library(dismo)
library(spam)
library(rworldmap)
library(rworldxtra)
library(jsonlite)
library(rgeos)
library(mapSpain)
library(ggmap)
library(ggplot2)
library(maps)
library(mapdata)
library(marmap)
library(mapproj)
library(slippymath)
library(ggspatial)



#'########
# PASO 2: OBTENCIÓN DE DATOS DE PAISES PARA GENERAR MAPAS ####
#'########

# Descargamos el mapa de paises y nos quedamos con los límites de la España peninsular (mapaspain) 

# Obtenemos el mapa mundial en alta resolución:
mapamundo <- getMap(resolution="high") 

# cómo son esos datos?
head(mapamundo)


# Lo visualizamos:
plot(mapamundo)


# Seleccionamos sólo la región de España:
mapaspain <- mapamundo["Spain",]

mapaitaliafrancia <- mapamundo[c('Italy','France'),]
plot(mapaitaliafrancia)


# Lo visualizamos:
plot(mapaspain)

# Seleccionamos la región de la península y Baleares "cortando" el mapa para 
# dicha extensión geográfica específica:
mapaspain <- crop(mapaspain, extent(-10,5,35,45))


# Lo visualizamos:
plot(mapaspain)

# Lo visualizamos en gris: 
plot(mapaspain, col="lightgrey", border="black")




#'########
# PASO 3: Explorar otras formas de visualizar mapas en R ####
#'########


## aproximación simple
## mapa sin fondo 
mapDevice() #crear una ventana aparte para dibujar los mapas
map("world", fill=TRUE
    ,col="grey65"
    ,boundary=F,interior=F
    ,ylim=c(-60, 65), mar=c(0,0,0,0)
    ,projection='albers',par=c(0,0),wrap=T
    ,resolution=1,border="lightgrey",myborder=0)

## mapa con fondo azul
map("world", fill=TRUE
    ,col="grey65"
    ,bg="deepskyblue4"
    ,boundary=F,interior=F
    ,ylim=c(30, 65),xlim=c(-5,25), mar=c(0,0,0,0)
    ,projection='albers',par=c(0,0),wrap=T
    ,resolution=0,border="white",myborder=0)


#Usando GGPLOT, dibujar mapa del mundo
mapWorld <- borders("world", colour="gray72", fill="gray65",
                    ylim=c(-60,60),xlim=c(-180,180)) # create a layer of borders
mp <- ggplot() +   mapWorld
mp + theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())


## usando el paquete rworldmap

trymap<-getMap()
mapBubbles(dF=trymap
           ,colourPalette="rainbow"
           #,oceanCol="white"
           ,landCol="grey62"
           ,borderCol="grey62"
           ,ylim=c(-49.5,70)
           ,lwd=0.1,add)

## añadir líneas de contorno
map('worldHires',add=T,col="lightgrey",lwd=1.5)




## además de los paquetes de R de mapas existe la posibilidad de bajar
## mapas disponibles online (p.ej. http://www.naturalearthdata.com/) 

setwd("~/MEGA/Work_UAH_BeaGal/teaching/2021/BigData en Ciencias Naturales/teoria/sig en R/vector themes/")
land <- st_read("ne_50m_land.shp") ## 
boundars <- st_read("ne_50m_admin_0_countries.shp")
plot(land$geometry, col="grey", lty=0, ylim=c(30,60),xlim=c(-5,35))
plot(land)

land$geometry


## o:
plot(boundars$geometry,col="grey",border="black",ylim=c(30,60),xlim=c(-5,35))




## además podemos añadir batimetría
BATHYMET<-getNOAA.bathy(lon1=45,lon2=-15,lat1=65,lat2=30, resolution=5)


# crear paleta de colores con azules
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")

# dibujar batimetría
plot(BATHYMET, image = TRUE, land = TRUE, lwd = 0.05,lty=0,
     ylim=c(30,65),xlim=c(-8,35),
     bpal = list(c(0, max(BATHYMET), "lightsteelblue1"),
                 c(min(BATHYMET),0,blues)),add=F)

plot(boundars$geometry,col="grey",border="white",ylim=c(30,65),xlim=c(-8,35),add=T)



# añadir altitud - hillshade

mapaspain

# obtener datos de topografía
altSpain <- getData('alt', country='ESP', mask=T)


plot(altSpain)


alt2 = altSpain
res(alt2) = 0.02166666

alt3 <- resample(altSpain,alt2,method='bilinear')
plot(alt3)


# generate reasonably looking hillshade
slope <- terrain(alt3, opt='slope')
aspect <- terrain(alt3, opt='aspect')
hill <- hillShade(slope, aspect, 30, 250, normalize=T)
plot(hill, col=adjustcolor(grey(0:100/100),0.8), legend=FALSE, add=F)
lines(Spain,lwd=1.5)






#'########
# PASO 4: Ejercicios seminario ####
#'########

## En base a lo aprendido hasta ahora realiza los siguientes ejercicios y guárdalos
## en un Rmarkdown:

# 1) Haz un mapa la francia continental (Europa) excluyendo sus colonias de ultramar

francia = mapamundo["France",]
francia = crop(francia, extent(-5,10,40,52))
plot(francia)

# 2) Haz un mapa del continente sudamericano incluyendo límites de fronteras, altitudes y batimetría
colnames(mapamundo@data)
sudamerica = subset(mapamundo, REGION == "South America and the Caribbean") 
plot(sudamerica)
extent(sudamerica)

BATHYMET<-getNOAA.bathy(lon1=-28.9,lon2=-118.5,lat1=33,lat2=-56, resolution=20)

# dibujar batimetría
plot(BATHYMET, image = TRUE, land = TRUE, lwd = 0.05,lty=0,
     #ylim=c(30,65),xlim=c(-8,35),
     bpal = list(c(0, max(BATHYMET), "lightsteelblue1"),
                 c(min(BATHYMET),0,blues)),add=F)

plot(sudamerica,col="grey62",border="grey62",add=T)
lines(sudamerica, col="white")


# 3) Haz un mapa de España con los elementos anteriores








#'########
# PASO 5: Utilizando el paquete mapSpain https://github.com/rOpenSpain/mapSpain/blob/main/README.md ####
#'########

# cargamos datos del censo de población del INE, que viene disponible por 
#defecto en el paquete

census <- pobmun19
#census <- mapSpain::pobmun19


# extraemos los códigos para cada comunidad autónoma

codelist <- esp_codelist
#codelist <- mapSpain::esp_codelist
str(codelist)


# unimos las dos tablas utilizando el comando 'merge'
census <-
  unique(merge(census, codelist[, c("cpro", "codauto")], all.x = TRUE))



# agregamos los valores por comunidad autónoma
census_ccaa <-
  aggregate(cbind(pob19, men, women) ~ codauto, data = census, sum)

# calculamos porcentajes

# mujeres
census_ccaa$porc_women <- census_ccaa$women / census_ccaa$pob19
census_ccaa$porc_women_lab <-
  paste0(round(100 * census_ccaa$porc_women, 2), "%")

# hombres
census_ccaa$porc_men <- census_ccaa$men / census_ccaa$pob19
census_ccaa$porc_men_lab <-
  paste0(round(100 * census_ccaa$porc_men, 2), "%")

census_ccaa



# unir los datos anteriores a un shapefile de españa (disponible en el paquete mapSpain)
# con información de los límites de las comunidades autónomas, para poder mapear

# obtener el shapefile
CCAA_sf <- esp_get_ccaa()
plot(CCAA_sf$geometry)

# unir datos, de nuevo utilizando el comando merge
CCAA_sf <- merge(CCAA_sf, census_ccaa)

# obtener la cajita para Canarias
Can <- esp_get_can_box()
plot(Can, add=T)



# Dibujar el mapa con ggplot


ggplot(CCAA_sf) +   
  geom_sf(aes(fill = porc_women), ## este módulo elige qué variable se dibuja
          color = "grey70",
          lwd = .3
  ) +       
  geom_sf(data = Can, color = "grey70") +   ## aquí añadimos la línea de Canarias
  geom_sf_label(aes(label = porc_women_lab),  ## aquí añadimos etiquetas
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Blues", rev = TRUE),  ## cambiamos la escala de colores
    n.breaks = 10,
    labels = function(x) {
      sprintf("%1.1f%%", 100 * x)
    },
    guide = guide_legend(title = "Porc. women")  ## añadimos leyenda
  ) +
  theme_void() +  ## eliminamos el fondo gris feote
  theme(legend.position = c(0.1, 0.6))  ## especificamos dónde queremos la leyenda



### ahora vamos a hacer otro ejercicio para una sóla provincia

# añadir porcentaje de mujeres a la tabla census
census$porc_women <- census$women / census$pob19
census$porc_men <- census$men / census$pob19


# obtenemos capas shapefile de municipios y provincias
shape <- esp_get_munic_siane(region = "Avila", epsg = 3857)
provs <- esp_get_prov_siane(epsg = 3857)


# exploramos qué pinta tienen
plot(shape$geometry)
plot(provs$geometry)

head(shape)
head(census)


# unimos datos a escala de municipio con el comando merge
shape_pop <- merge(shape,census,
                   by = c("cpro", "cmun"),
                   all.x = TRUE)

plot(shape_pop$geometry)

# obtenemos información raster para el fondo del mapa (tesela correspondiente al
# extent de la capa shape_pop)
tile <- esp_getTiles(shape_pop,
               type = "IGNBase.Todo",
               zoom = 10,
               bbox_expand = .1)

plot(tile)



# Dibujamos

## obtenemos el extent geográfico con la función ext del paquete terra
lims <- as.double(terra::ext(tile)@ptr$vector)


## dibujamos con ggplot
ggplot(remove_missing(shape_pop, na.rm = TRUE)) +
  layer_spatraster(tile) +
  geom_sf(aes(fill = porc_women), color = NA) +
  geom_sf(data = provs, fill = NA) +
  scale_fill_gradientn(
    colours = hcl.colors(10, "RdYlBu", alpha = .4),
    n.breaks = 8,
    labels = function(x) {
      sprintf("%1.0f%%", 100 * x)
    },
    guide = guide_legend(title = "", )
  ) +
  coord_sf(
    xlim = lims[c(1, 2)],
    ylim = lims[c(3, 4)],
    expand = FALSE
  ) +
  labs(
    title = "Proporción de mujeres en Ávila por municipio (2019)",
    caption = "Source: INE"
  ) +
  theme_void() +
  theme(
    title = element_text(face = "bold")
  )



## probar con cuencas hidrográficas

hydroland <- esp_get_hydrobasin(domain = "land")
hydrolandsea <- esp_get_hydrobasin(domain = "landsea")

plot(hydroland$geom)

ggplot(hydroland) +
  geom_sf(data = hydrolandsea, fill = "skyblue4", alpha = .4) +
  geom_sf(fill = "skyblue", alpha = .5) +
  geom_sf_text(aes(label = rotulo),
               size = 3, check_overlap = TRUE,
               fontface = "bold",
               family = "serif"
  ) +
  coord_sf(
    xlim = c(-9.5, 4.5),
    ylim = c(35, 44)
  ) +
  theme_void()



## altitudes y batimetría

hypsobath <- esp_get_hypsobath()
# hay que corregir un error en los datos de origen
# Remove:
hypsobath <- hypsobath[!sf::st_is_empty(hypsobath), ]



# Colores a partir de Wikipedia
# https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps
bath_tints <- colorRampPalette(
  rev(
    c(
      "#D8F2FE", "#C6ECFF", "#B9E3FF",
      "#ACDBFB", "#A1D2F7", "#96C9F0",
      "#8DC1EA", "#84B9E3", "#79B2DE",
      "#71ABD8"
    )
  )
)

hyps_tints <- colorRampPalette(
  rev(
    c(
      "#F5F4F2", "#E0DED8", "#CAC3B8", "#BAAE9A",
      "#AC9A7C", "#AA8753", "#B9985A", "#C3A76B",
      "#CAB982", "#D3CA9D", "#DED6A3", "#E8E1B6",
      "#EFEBC0", "#E1E4B5", "#D1D7AB", "#BDCC96",
      "#A8C68F", "#94BF8B", "#ACD0A5"
    )
  )
)

## ordenamos los niveles de alturas
levels <- sort(unique(hypsobath$val_inf))


# generamos una paleta de colores
br_bath <- length(levels[levels < 0])
br_terrain <- length(levels) - br_bath
pal <- c(bath_tints((br_bath)), hyps_tints((br_terrain)))



# hacemos el dibujo para las islas Canarias
ggplot(hypsobath) +
  geom_sf(aes(fill = as.factor(val_inf)),
          color = NA
  ) +
  coord_sf(
    xlim = c(-18.6, -13),
    ylim = c(27, 29.5)
  ) +
  scale_fill_manual(values = pal) +
  guides(fill = guide_legend(
    title = "Elevation",
    direction = "horizontal",
    label.position = "bottom",
    title.position = "top",
    nrow = 1
  )) +
  theme(legend.position = "bottom")



# Y ahora dibujamos el mapa de altura/batimetría para el continente
spainhypbat <- ggplot(hypsobath) +
  geom_sf(aes(fill = as.factor(val_inf)),
          color = NA
  ) +
  coord_sf(
    xlim = c(-9.5, 4.4),
    ylim = c(35.8, 44)
  ) +
  scale_fill_manual(values = pal) +
  guides(fill = guide_legend(
    title = "Elevation",
    reverse = TRUE,
    keyheight = .8
  )) 

spainhypbat





#'########
# PASO 6: extraer datos biológicos p.ej. distribución de especies (GBIF) ####
#'########


## Extraer datos de presencias para el lince ibérico
# extraemos datos de gbif
lynx <- gbif("Lynx", "pardinus")      
lynx

# hacemos un subset de esos datos y lo convertimos en un data.frame 
# con coordenadas
pres.lynx <- subset(lynx, select=c("country", "lat", "lon"))
head(pres.lynx)    


# Descartamos posibles errores:
pres.lynx <- subset(pres.lynx, pres.lynx$lat<90)


# Mapeando las ocurrencias de Lince sobre el mapa anterior

dev.off()
spainhypbat +
  geom_sf(data = provs, colour='red', fill = "white", alpha = .001) +
  geom_point(data=pres.lynx, aes(x = lon, y = lat), color = "black") +
  coord_sf(
    xlim = c(-9.5, 4.4),
    ylim = c(35.8, 44)
  )  
  

# Mapeando las ocurrencias de Lince sobre uno de los mapas primeros
par(mar=c(0.5,0.5,0.5,0.5)) ## ajustando los márgenes
plot(newmap, col="lightgrey", border="white", xlim=c(-7,3), ylim=c(34,45))
points(pres.lynx$lon,pres.lynx$lat,
       pch=19,cex=1,col=adjustcolor("cyan4",0.15))
text(0,45,substitute(italic("Lynx pardinus")),cex=2)

# crear SpatialPointsDataFrame
coordinates(pres.lynx) <- c("lon", "lat")    
plot(pres.lynx)
pres.lynx

# exploramos el sistema de coordenadas
projection(pres.lynx) # no hay


## la función CRS permite definir el sistema de coordenadas de referencia. 
## Vamos a asignar una proyección geográfica basada en el datum WGS84 que
## suele utilizarse como referencia por defecto
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# asignamos el sistema de coordenadas a nuestros datos
proj4string(pres.lynx) <- crs.geo     

str(pres.lynx)



## crear un subconjunto a partir del mapa del mundo, solo para España
spainmap=newmap["Spain",]

plot(spainmap)

## explorar estructura del objeto shapefile

spainmap@polygons[[1]]@Polygons[[1]]


## quedarse sólo con el polígono correspondiente a la España peninsular
PeninSpain<-SpatialPolygons(list(
                            Polygons(list(
                            spainmap@polygons[[1]]@Polygons[[1]]),ID=1)),
                            proj4string = CRS(projection(spainmap)))



# podemos ir deconstruyendo el comando anterior:
length(spainmap@polygons[[1]]@Polygons) # hay 17 polígonos

# y dibujar distintos sub-polígonos, por ejemplo el segundo, tercero, etc.
plot(SpatialPolygons(list(
  Polygons(list(
    spainmap@polygons[[1]]@Polygons[[2]]),ID=2)),
  proj4string = CRS(projection(spainmap))))

plot(PeninSpain)

## mapear la distribución del lince de distintas maneras
plot(pres.lynx, pch=20, col="cyan4")
plot(PeninSpain, add=T)

plot(PeninSpain, add=F,col="lightgrey",border="grey")
plot(pres.lynx, pch=20, col="cyan4",add=T)


# guardar el shapefile de puntos
writePointsShape(pres.lynx, "data/presenciaslince")
td <- file.path(getwd(), "data") 
#dir.create(td)

writeOGR(pres.lynx, dsn=td, 
         layer="presenciaslince", driver="ESRI Shapefile")


# guardar el shapefile de polígonos
# para salvar hemos de convertir a un objeto SpatialPolygonsDataFrame primero
PeninDF<-SpatialPolygonsDataFrame(PeninSpain,data=data.frame(ID=1))
writeOGR(PeninDF, dsn=td, 
         layer="Espanapenin", driver="ESRI Shapefile")


# leer/cargar los shapefiles guardados (ejemplo con archivo de pntos)
presencias.lynx <- readShapePoints("data/presenciaslince.shp")
plot(presencias.lynx)



# cargamos capa de parques nacionales

pns<-readOGR("../../../../Work_UAH_postdoc/Teaching2018/SIG en R/data/Red_PN_LIM/LIM/Limites_PyB.shp")

plot(pns, col='red', add=T)


# comprobamos la proyección
projection(pns)


# cambiar la proyección de UTM a WGS con spTransform
pns.geo<-spTransform(pns,crs.geo)

# comprobamos la proyección
projection(pns.geo)


plot(pns.geo,add=T)

# nos quedamos solo con el nombre de cada parque en los datos y 
# quitamos el parque de Baleares
pns.geo@data$NOM_PARQUE

pnsSpain<-pns.geo[-1,1]


## extraer datos resultantes de solapar dos capas shapefile
pres.lynx
pns.geo

lynxinpark <- over(pns.geo,pres.lynx)
lynxinpark <- over(pres.lynx,pns.geo)
table(lynxinpark$NOM_PARQUE[!is.na(lynxinpark$NOM_PARQUE)])


## extract data from raster to polygon
tempmax <- getData("worldclim", var="tmax", res=10)   


plot(tempmax[[1]],main="Temperatura máxima Enero")
plot(tempmax[[7]],main="Temperatura máxima Julio")



# Vamos a leer archivos ráster correspondientes a las temperaturas y precipitaciones del 15 de enero y del 15 de julio de 2007 en España: 
temp.Spain.jan07<-raster("~/MEGA/Work_UAH_postdoc/Teaching2018/SIG en R/data/temp.Spain.jan07.tif")
temp.Spain.jul07<-raster("~/MEGA/Work_UAH_postdoc/Teaching2018/SIG en R/data/temp.Spain.jul07.tif")

# Mapear los ráster cargados:
plot(temp.Spain.jan07,main="Temperatura máxima 15 Enero 2007")
plot(temp.Spain.jul07,main="Temperatura máxima 15 Julio 2007")




# Podemos cortar archivos raster con el comando `crop()`. 
# Esto puede hacerse bien en base a un extent() indicando coordenadas
# máximas y mínimas (xmin, xmax, ymin, ymax): 
extent.spain <- extent(-9.35, 4.45, 35.95, 43.85)
temp.jan.world.cropped<-crop(tempmax[[1]],extent.spain)

plot(temp.jan.world.cropped)
temp.Spain.jan07<-raster("~/MEGA/Work_UAH_postdoc/Teaching2018/SIG en R/data/temp.Spain.jan07.tif")


## extraer valores a partir del archivo raster
plot(temp.Spain.jan07)
plot(pns.geo, add=T)

tempjanPN <- extract(temp.Spain.jan07,pns.geo)
names(tempjanPN) <- pns.geo$NOM_PARQUE

#'########
# PASO 7: Ejercicios seminario II ####
#'########


## En base a lo aprendido hasta ahora realiza los siguientes ejercicios y guárdalos
## en un Rmarkdown:

# 1) Repite el mapa de proporción de hombres a escala de provincia

# 2) Descarga la distribución de una especie de tu interés a partir de 
#    GBIF y muestra su distribución sobre la comunidad autónoma en qué se 
#    distribuye preferentemente

# 3) Con datos de otros estudiantes, carga los datos a escala de provincia
#    y haz un mapa




