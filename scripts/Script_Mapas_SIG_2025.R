################################################################# #
##'
##'  "Mapas, SIG y operaciones cartográficas sencillas en R"
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'
################################################################# #

## houskeeping
rm(list=ls())
options(stringsAsFactors = FALSE)


## Setting working directory. 
  #setwd("~/OneDrive - Universidad de Alcala/Work_UAH_BeaGal/teaching/2023/Gestion de datos/clases/") 
  #setwd("../OneDrive - Universidad de Alcala/Work_UAH_BeaGal/teaching/2023/Gestion de datos/clases/") 

getwd()  



## useful mapping packages (install in case you don't have them)
#install.packages(c("googleway", "libwgeom",
#                   "rnaturalearth", "rnaturalearthdata"))


## Loading packages
#install.packages()
#install.packages()
#install.packages()
#install.packages()

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



## 1. Loading spatial data ####
############################ ##

## raster data
### 1a. OBTAIN WORLDCLIM DATA (package geodata)----

clima <- worldclim_global("bio", 10, "../data/")
clima
plot(clima$wc2.1_10m_bio_12)

filestoload <- paste0("wc2.1_10m/",dir("wc2.1_10m/"))
clima <- rast(filestoload)
bio1517 <- rast(c("../data/wc2.1_10m/wc2.1_10m_bio_15.tif",
                "../data/wc2.1_10m/wc2.1_10m_bio_17.tif"))


climaspain <- worldclim_country("Spain", "bio", "../data/")
climaspain
plot(climaspain$wc2.1_30s_bio_1)

filestoload <- paste0("wc2.1_country/",dir("wc2.1_country/"))
climaspain <- rast(filestoload)


climames <- worldclim_global("tmax", 10, "../data/")



## explore a zoomed region
plot(climaspain$wc2.1_30s_bio_1, 
     xlim=c(-10,5), ylim=c(35,44))



## cut map
climaspaincrop <- crop(clima, ext(-10,5,35,44))
plot(climaspaincrop$wc2.1_10m_bio_1)


## polygon data
### 1b. Read in country shapefile data ----
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
plot(world$geometry)


## subset to Europe
europe <- subset(world, continent == "Europe")
plot(europe$geometry)

## subset to Iberia
iberia <- subset(europe, sovereignt %in% c("Spain","Portugal"))
plot(iberia$geometry)
lines(iberia, col="black", lwd = 2.5)

dev.off()


### 1c. Read in parques nacionales shapefile data ----
## https://www.mapama.gob.es/app/descargas/descargafichero.aspx?f=enp.zip
enps <- st_read("../data/ENP.shp")
enps

lines(enps, col="darkred",lwd=0.5)

ppnn <- st_read("../data/Limites_PyB.shp")
ppnn
plot(ppnn$geometry)

## cuantos tipos de parques
unique(enps$figura_lp)

## seleccionar solo parques nacionales y naturales
ppnnnat <- subset(enps, figura_lp %in% c("Parque Nacional",
                                         "Parque Natural"))
lines(ppnnnat, col="black",lwd=1.5)



### 2. get coordinates for a species distribution ----
## 
#install.packages("rgbif")
library(rgbif)
occ_search(scientificName = "Lynx pardinus")

lince <- occ_data(scientificName = "Lynx pardinus",
                   #country = "France",
                   limit = 4031)

class(lince)

lince$data


points(x = lince$data$decimalLongitude,
       y = lince$data$decimalLatitude,
       col="navy",pch=19,cex=0.7)


## extract data according to coordinates

plot(climaspain$wc2.1_30s_bio_1)

coordenadas <- data.frame(x=lince$data$decimalLongitude,
                          y=lince$data$decimalLatitude)

climacoords <- extract(climaspain, coordenadas)




## extraer datos according to polygons

bio1enp <- extract(climaspain$wc2.1_30s_bio_1, ppnnnat, 
                   fun=mean, na.rm=T)



bio1enp$nombre <- ppnnnat$sitename
bio1enp[which.min(bio1enp$wc2.1_30s_bio_1),3]


bio1ppnn <- extract(climaspain$wc2.1_30s_bio_1, ppnn, 
                   fun=mean, na.rm=T)



## hay que cambiar la proyecci?n
ppnncor = st_transform(ppnn, crs(enps))
lines(ppnncor, col="red")

bio1ppnn <- extract(climaspain$wc2.1_30s_bio_1, ppnncor, 
                    fun=mean, na.rm=T)
bio1ppnn$idppnnat <- ppnncor$NOM_PARQUE


bio1ppnn <- extract(climaspain$wc2.1_30s_bio_1, ppnncor)
bio1ppnn

boxplot(wc2.1_30s_bio_1~ID, 
        ylab = "Temperatura (C)",
        xlab = "Parque Nacional",
        data=bio1ppnn)



dev.off()
unique(bio1ppnn$ID)




#'########
# Utilizando el paquete mapSpain https://github.com/rOpenSpain/mapSpain/blob/main/README.md ####
#'########
library(mapSpain)

# cargamos datos del censo de poblaci?n del INE, que viene disponible por 
#defecto en el paquete

census <- pobmun19
#census <- mapSpain::pobmun19


# extraemos los c?digos para cada comunidad aut?noma

codelist <- esp_codelist
#codelist <- mapSpain::esp_codelist
str(codelist)


# unimos las dos tablas utilizando el comando 'merge'
census <-
  unique(merge(census, codelist[, c("cpro", "codauto")], all.x = TRUE))



# agregamos los valores por comunidad aut?noma
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



# unir los datos anteriores a un shapefile de espa?a (disponible en el paquete mapSpain)
# con informaci?n de los l?mites de las comunidades aut?nomas, para poder mapear

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
  geom_sf(aes(fill = porc_women), ## este m?dulo elige qu? variable se dibuja
          color = "grey70",
          lwd = .3
  ) +       
  geom_sf(data = Can, color = "grey70") +   ## aqu? a?adimos la l?nea de Canarias
  geom_sf_label(aes(label = porc_women_lab),  ## aqu? a?adimos etiquetas
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
    guide = guide_legend(title = "Porc. women")  ## a?adimos leyenda
  ) +
  theme_void() +  ## eliminamos el fondo gris feote
  theme(legend.position = c(0.1, 0.6))  ## especificamos d?nde queremos la leyenda



## altitudes y batimetr?a

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



# Y ahora dibujamos el mapa de altura/batimetr?a para el continente
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


######## package climateR ---- 
#install.packages("climateR")
#install.packages("tigris")
#remotes::install_github("mikejohnson51/AOI")
#library(AOI)
#library(tigris)

#states <- states(cb = TRUE)





### END ----



