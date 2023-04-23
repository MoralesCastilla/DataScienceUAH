#'##################################################################
#   Asignatura Transversal, Universidad de Alcal�
#   Asignatura: Ciencia de datos pr�ctica
#
#   Script para la teor�a/seminario M�dulos IV / V: 
#     Obtenci�n, tratamiento y visualizaci�n de datos geogr�ficamente expl�citos
#
#   Autor: Ignacio Morales-Castilla
#   Fecha: Marzo 2022
#'##################################################################
#'##################################################################


#'########
#### PASO 1: PREPARACI�N DEL ENTORNO DE TRABAJO ####
#'########

# 1A ## Eliminamos objetos que pudiese haber en la memoria de RStudio
rm(list=ls())
options(stringsAsFactors = FALSE)

# 1B ## Creamos un directorio de trabajo para esta pr�ctica en el ordenador, y
# asignamos este directorio desde el que vamos a trabajar con 'setwd()':
# pega entre comillas la direcci�n f�sica de esa carpeta en tu ordenador
# (atenci�n a la direcci�n de las barras /)
# o seleccionala directamente usando el atajo Ctrl+Shift+H
setwd("~/Temporada 2022-2023/Asignaturas/2o cuatrimestre/Ciencia de datos/script/2-script/")
setwd('~/MEGA/Work_UAH_BeaGal/teaching/2021/BigData en Ciencias Naturales/')



# 1C ## Instalamos y cargamos los paquetes que utilizaremos durante los an�lisis
#install.packages("raster")
#install.packages("maptools")
#install.packages("sp")
#install.packages("rgdal")
#install.packages("dismo")
#install.packages("spam")
#install.packages("rworldmap")
#install.packages("rworldxtra")
#install.packages("jsonlite")
#install.packages("rgeos")
#install.packages("mapSpain")
#install.packages("ggmap")
#install.packages('ggplot2')
#install.packages('maps')
#install.packages('mapdata')
#install.packages("marmap")
#install.packages("mapproj")
#install.packages("sf")
#install.packages("slippymath")
#install.packages("ggspatial")
#install.packages("tidyterra")

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
library(tidyterra)



#'########
# PASO 2: OBTENCI�N DE DATOS DE PAISES PARA GENERAR MAPAS ####
#'########

# Descargamos el mapa de paises y nos quedamos con los l�mites de la Espa�a peninsular (mapaspain) 

# Obtenemos el mapa mundial en alta resoluci�n:
mapamundo <- getMap(resolution="high") 

# c�mo son esos datos?
head(mapamundo)


# Lo visualizamos:
plot(mapamundo)


# Seleccionamos s�lo la regi�n de Espa�a:
mapaspain <- mapamundo["Spain",]

iberia <- mapamundo[c("Spain", "Portugal"),]

mapaitaliafrancia <- mapamundo[c('Italy','France'),]
plot(mapaitaliafrancia)


# Lo visualizamos:
plot(mapaspain)
plot(iberia)
# Seleccionamos la regi�n de la pen�nsula y Baleares "cortando" el mapa para 
# dicha extensi�n geogr�fica espec�fica:
mapaspain <- crop(mapaspain, extent(-10,5,35,45))
iberia <- crop(iberia, extent(-10,5,35,45))

# Lo visualizamos:
plot(mapaspain)
plot(iberia)

length(iberia@polygons[[1]]@Polygons[[1]])
#iberia@polygons[[1]] = iberia@polygons[[1]]@Polygons[[1]]
plot(iberia)
# Lo visualizamos en gris: 
plot(mapaspain, col="lightgrey", border="white")




#'########
# PASO 3: Explorar otras formas de visualizar mapas en R ####
#'########

mapas

## aproximaci�n simple
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

## a�adir l�neas de contorno
map('worldHires',add=T,col="lightgrey",lwd=1.5)




## adem�s de los paquetes de R de mapas existe la posibilidad de bajar
## mapas disponibles online (p.ej. http://www.naturalearthdata.com/) 

setwd("~/Temporada 2022-2023/Asignaturas/2o cuatrimestre/Ciencia de datos/script/Country/")
land <- st_read("ne_50m_land.shp")
boundars <- st_read("ne_50m_admin_0_countries_lakes.shp")
plot(land$geometry, col="grey", lty=0, ylim=c(30,60),xlim=c(-5,35))
plot(land)

land$geometry


## o:
plot(boundars$geometry,col="grey",border="white",ylim=c(30,60),xlim=c(-5,35))




## adem�s podemos a�adir batimetr�a
BATHYMET<-getNOAA.bathy(lon1=45,lon2=-15,lat1=65,lat2=30, resolution=5)


# crear paleta de colores con azules
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")

# dibujar batimetr�a
plot(BATHYMET, image = TRUE, land = TRUE, lwd = 0.05,lty=0,
     ylim=c(30,65),xlim=c(-8,35),
     bpal = list(c(0, max(BATHYMET), "lightsteelblue1"),
                 c(min(BATHYMET),0,blues)),add=F)

plot(boundars$geometry,col="grey",border="white",ylim=c(30,65),xlim=c(-8,35),add=T)



# a�adir altitud - hillshade

mapaspain

# obtener datos de topograf�a
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
lines(mapaspain,lwd=1.5)




#'########
# PASO 4: Ejercicios seminario ####
#'########

## En base a lo aprendido hasta ahora realiza los siguientes ejercicios y gu�rdalos
## en un Rmarkdown:

# 1) Haz un mapa la francia continental (Europa) excluyendo sus colonias de ultramar

francia = mapamundo["France",]
francia = crop(francia, extent(-5,10,40,52))
plot(francia)

# 2) Haz un mapa del continente sudamericano incluyendo l�mites de fronteras, altitudes y batimetr�a

View(mapamundo@data)
sudamerica = subset(mapamundo, REGION == "South America and the Caribbean") 
plot(sudamerica)
extent(sudamerica)

BATHYMET<-getNOAA.bathy(lon1=-28.9,lon2=-118.5,lat1=33,lat2=-56, resolution=20)

# dibujar batimetr�a
plot(BATHYMET, image = TRUE, land = TRUE, lwd = 0.05,lty=0,
     #ylim=c(30,65),xlim=c(-8,35),
     bpal = list(c(0, max(BATHYMET), "lightsteelblue1"),
                 c(min(BATHYMET),0,blues)),add=F)

plot(sudamerica,col="grey62",border="grey62",add=T)
lines(sudamerica, col="white")


# 3) Haz un mapa de Espa�a con los elementos anteriores








#'########
# PASO 5: Utilizando el paquete mapSpain https://github.com/rOpenSpain/mapSpain/blob/main/README.md ####
#'########

# cargamos datos del censo de poblaci�n del INE, que viene disponible por 
#defecto en el paquete

census <- pobmun19
#census <- mapSpain::pobmun19


# extraemos los c�digos para cada comunidad aut�noma

codelist <- esp_codelist
#codelist <- mapSpain::esp_codelist
str(codelist)


# unimos las dos tablas utilizando el comando 'merge'
census <-
  unique(merge(census, codelist[, c("cpro", "codauto")], all.x = TRUE))

head(census)

# agregamos los valores por comunidad aut�noma
census_ccaa <-
  ?aggregate(cbind(pob19, men, women) ~ codauto, data = census, sum)

head(census_ccaa)
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



# unir los datos anteriores a un shapefile de espa�a (disponible en el paquete mapSpain)
# con informaci�n de los l�mites de las comunidades aut�nomas, para poder mapear

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
  geom_sf(aes(fill = porc_women), ## este m�dulo elige qu� variable se dibuja
          color = "grey70",
          lwd = .3
  ) +       
  geom_sf(data = Can, color = "grey70") +   ## aqu� a�adimos la l�nea de Canarias
  geom_sf_label(aes(label = porc_women_lab),  ## aqu� a�adimos etiquetas
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
    guide = guide_legend(title = "Porc. women")  ## a�adimos leyenda
  ) +
  theme_void() +  ## eliminamos el fondo gris feote
  theme(legend.position = c(0.1, 0.6))  ## especificamos d�nde queremos la leyenda



### ahora vamos a hacer otro ejercicio para una s�la provincia

# a�adir porcentaje de mujeres a la tabla census
census$porc_women <- census$women / census$pob19
census$porc_men <- census$men / census$pob19


# obtenemos capas shapefile de municipios y provincias
shape <- esp_get_munic_siane(region = "Avila", epsg = 3857)
provs <- esp_get_prov_siane(epsg = 3857)


# exploramos qu� pinta tienen
plot(shape$geometry)
plot(provs$geometry)

head(shape)
head(census)


# unimos datos a escala de municipio con el comando merge
shape_pop <- merge(shape,census,
                   by = c("cpro", "cmun"),
                   all.x = TRUE)

plot(shape_pop$geometry)

# obtenemos informaci�n raster para el fondo del mapa (tesela correspondiente al
# extent de la capa shape_pop)
tile <- esp_getTiles(shape_pop,
                     type = "IGNBase.Todo",
                     zoom = 10,
                     bbox_expand = .1)

plot(tile)



# Dibujamos

## obtenemos el extent geogr�fico con la funci�n ext del paquete terra
lims <- as.double(terra::ext(tile)@ptr$vector)


## dibujamos con ggplot
ggplot(remove_missing(shape_pop, na.rm = TRUE)) +
  tidyterra::geom_spatraster_rgb(tile) +
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
    title = "Proporci�n de mujeres en �vila por municipio (2019)",
    caption = "Source: INE"
  ) +
  theme_void() +
  theme(
    title = element_text(face = "bold")
  )



## probar con cuencas hidrogr�ficas

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



## altitudes y batimetr�a

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



# Y ahora dibujamos el mapa de altura/batimetr�a para el continente
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
# PASO 6: extraer datos biol�gicos p.ej. distribuci�n de especies (GBIF) ####
#'########


## Extraer datos de presencias para el lince ib�rico
# extraemos datos de gbif
lynx <- gbif("Lynx", "pardinus")
View(lynx)

# hacemos un subset de esos datos y lo convertimos en un data.frame 
# con coordenadas
pres.lynx <- subset(lynx, select=c("country", "lat", "lon"))
head(pres.lynx)    

table(is.na[pres.lynx[,"lon"]])
# Descartamos posibles errores:
pres.lynx <- subset(pres.lynx, pres.lynx$lat<90)
presencias.lynx <- pres.lynx(complete.cases[pres.lynx],c("lon","lat"))

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
par(mar=c(0.5,0.5,0.5,0.5)) ## ajustando los m�rgenes
plot(iberia, col="lightgrey", border="white", xlim=c(-10,3), ylim=c(34,45))
points(pres.lynx$lon,pres.lynx$lat,
       pch=19,cex=1,col=adjustcolor("cyan4",0.15))
text(0,45,substitute(italic("Lynx pardinus")),cex=2)

# crear SpatialPointsDataFrame
coordinates(pres.lynx) <- c("lon", "lat")  
plot(pres.lynx)
pres.lynx

# crear un sf object

preslynx.sf <- st_as_sf(pres.lynx, coords = c("lon", "lat"), crs = 4326) 

plot(preslynx.sf, add=T)

# exploramos el sistema de coordenadas
projection(pres.lynx) # no hay


## la funci�n CRS permite definir el sistema de coordenadas de referencia. 
## Vamos a asignar una proyecci�n geogr�fica basada en el datum WGS84 que
## suele utilizarse como referencia por defecto
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# asignamos el sistema de coordenadas a nuestros datos
proj4string(pres.lynx) <- crs.geo     

str(pres.lynx)



## crear un subconjunto a partir del mapa del mundo, solo para Espa�a
spainmap=newmap["Spain",]

plot(spainmap)

## explorar estructura del objeto shapefile

spainmap@polygons[[1]]@Polygons[[1]]


## quedarse s�lo con el pol�gono correspondiente a la Espa�a peninsular
PeninSpain<-SpatialPolygons(list(
  Polygons(list(
    spainmap@polygons[[1]]@Polygons[[1]]),ID=1)),
  proj4string = CRS(projection(spainmap)))

##guardar el shapefile de puntos

st_write(preslynx.sf,"../data/lince.shp")
st_write(pres.lynx,"../data/lince.shp")
plot(preslynx.sf)

# podemos ir deconstruyendo el comando anterior:
length(spainmap@polygons[[1]]@Polygons) # hay 17 pol�gonos

# y dibujar distintos sub-pol�gonos, por ejemplo el segundo, tercero, etc.
plot(SpatialPolygons(list(
  Polygons(list(
    spainmap@polygons[[1]]@Polygons[[2]]),ID=2)),
  proj4string = CRS(projection(spainmap))))

plot(PeninSpain)

## mapear la distribuci�n del lince de distintas maneras
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


# guardar el shapefile de pol�gonos
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


# comprobamos la proyecci�n
projection(pns)


# cambiar la proyecci�n de UTM a WGS con spTransform
pns.geo<-spTransform(pns,crs.geo)

# comprobamos la proyecci�n
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


plot(tempmax[[1]],main="Temperatura m�xima Enero")
plot(tempmax[[7]],main="Temperatura m�xima Julio")



# Vamos a leer archivos r�ster correspondientes a las temperaturas y precipitaciones del 15 de enero y del 15 de julio de 2007 en Espa�a: 
temp.Spain.jan07<-raster("~/MEGA/Work_UAH_postdoc/Teaching2018/SIG en R/data/temp.Spain.jan07.tif")
temp.Spain.jul07<-raster("~/MEGA/Work_UAH_postdoc/Teaching2018/SIG en R/data/temp.Spain.jul07.tif")

# Mapear los r�ster cargados:
plot(temp.Spain.jan07,main="Temperatura m�xima 15 Enero 2007")
plot(temp.Spain.jul07,main="Temperatura m�xima 15 Julio 2007")




# Podemos cortar archivos raster con el comando `crop()`. 
# Esto puede hacerse bien en base a un extent() indicando coordenadas
# m�ximas y m�nimas (xmin, xmax, ymin, ymax): 
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


## En base a lo aprendido hasta ahora realiza los siguientes ejercicios y gu�rdalos
## en un Rmarkdown:

# 1) Repite el mapa de proporci�n de hombres a escala de provincia

# 2) Descarga la distribuci�n de una especie de tu inter�s a partir de 
#    GBIF y muestra su distribuci�n sobre la comunidad aut�noma en qu� se 
#    distribuye preferentemente

# 3) Con datos de otros estudiantes, carga los datos a escala de provincia
#    y haz un mapa


## podeis crear una envolvente convexa (convex hull)

rangolince.ch<-chull(pres.lynx[,c("lon","lat")])

rangolince.ch<-c(rangolince.ch,rangolince.ch[1])



# la dibujamos

polygon(presencias.lynx[rangolince.ch,c("lon","lat")],col=adjustcolor("red",0.3),add=T)

# crear un sf object

coords.CH.lince = as.data.frame(presencias.lynx[rangolince.ch,c("lon","lat")])



p = Polygon(coords.CH.lince)

ps = Polygons(list(p),1)

sps = SpatialPolygons(list(ps))

plot(sps)





coordinates(coords.CH.lince) <- c("lon", "lat")

convex.lince <- st_as_sf(sps, coords = c("lon", "lat"), crs = 4326) 

plot(convex.lince,add=T)





# salvar

st_write(convex.lince,"../data/convexhull_lince.shp")






#'########

# PASO 8: Intersección entre dos polígonos y cálculo del área ####

#'########



## Extraer datos de presencias para el oso pardo en España

# extraemos datos de gbif

oso <- gbif("Ursus", "arctos", ext = c(-10,5,35,45))  

oso <- subset(oso, lat>41)

oso <- subset(oso, country=="Spain")


silene <- gbif("Silene", "ciliata", ext = c(-10,5,35,45))

silene <- subset(silene, country=="Spain")

silene <- subset(silene,lat>40)

silene <- subset(silene,lon<0)





plot(iberia, col="grey", border="grey",axes=T)

points(oso$lon,oso$lat,pch=20,cex=0.2)

points(silene$lon,silene$lat,col="blue",pch=20,cex=0.2)





##  podeis crear una envolvente convexa (convex hull)

rangooso.ch<-chull(oso[,c("lon","lat")])

rangooso.ch<-c(rangooso.ch,rangooso.ch[1])



rangosilene.ch<-chull(silene[,c("lon","lat")])

rangosilene.ch<-c(rangosilene.ch,rangosilene.ch[1])



# los dibujamos

polygon(oso[rangooso.ch,c("lon","lat")],col=adjustcolor("black",0.3),add=T)

polygon(silene[rangosilene.ch,c("lon","lat")],col=adjustcolor("blue",0.3),add=T)





# creamos sf objects

coords.CH.oso = as.data.frame(oso[rangooso.ch,c("lon","lat")])

p = Polygon(coords.CH.oso)

ps = Polygons(list(p),1)

polygon.oso = SpatialPolygons(list(ps))

plot(polygon.oso)



coords.CH.silene = as.data.frame(silene[rangosilene.ch,c("lon","lat")])

p = Polygon(coords.CH.silene)

ps = Polygons(list(p),1)

polygon.silene = SpatialPolygons(list(ps))

plot(polygon.silene,add=T,border="blue")



convex.oso <- st_as_sf(polygon.oso, coords = c("lon", "lat"), crs = 4326)


convex.silene <- st_as_sf(polygon.silene, coords = c("lon", "lat"), crs = 4326)



# calcular solapamiento espacial



oso_silene_intersect <- st_intersection (convex.oso, convex.silene)

oso_silene_intersects <- st_intersects (convex.oso, convex.silene)



plot(oso_silene_intersect,border="red",add=T, lwd=2)





area.oso = st_area(convex.oso)
area.silene = st_area(convex.silene)
area.ambos = st_area(oso_silene_intersect)
area.total = (area.oso-area.ambos)+ (area.silene-area.ambos) + area.ambos
prop.solapamiento = area.ambos/area.total*100
prop.solapamiento.oso = area.ambos/area.oso*100
prop.solapamiento.silene = area.ambos/area.silene*100

table.solapamientos <- array(NA, dim=c(12,7))
colnames(table.solapamientos) <- c("area.spsA", "area.spsB", "area.both", "area.total", "area.overlap",
                                   "prop.spsA", "prop.spsB")
row.names(table.solapamientos) <- c("fms.spsA.vs.spsB", "fms.spsA.vs.spsC", "fms")

for(i in 1:ncol) {
  
  datos.mes.i = subset(datos1, mes == i)
  
  
  
  table.solapamientos [i,1] = area.oso
  table.solapamientos [i,2] = area.silene
  table.solapamientos [i,3] = area.ambos
  table.solapamientos [i,4] = area.total
  table.solapamientos [i,5] = prop.solapamiento
  table.solapamientos [i,6] = prop.solapamiento.oso
  table.solapamientos [i,7] = prop.solapamiento.silene
}
