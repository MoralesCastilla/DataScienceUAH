#.###################################.#
#      Mengna Zhou & Víctor Leal      #
#             Prácticas R             #
#          Pinzones de Darwin         #
#.###################################.#


## Borrar datos - reiniciar
rm(list = ls ())

## Establecer directorio de trabajo
setwd("C:/Users/mengna.zhou/Desktop/r/prácticas")
getwd()

## para el directorio en portátil Mengna
# setwd("C:/Users/usuario/Desktop/Darwin's finches")
# getwd()


## instalar paquetes y cargar librerías
# install.packages("tidyverse")
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)

#.#####################################.#

# HIPÓTESIS: 
# MICROECOLOGÍA --> el tamaño de los picos varía en función del clima: a menor
#   humedad, más semillas grandes, sobreviven individuos de picos más grandes.
# MACROECOLOGÍA --> mayor diferencia en precipitaciones que sufren dos especies, 
#   mayor será la diferencia en el tamaño de sus picos.

# MICROEVOLUTIVO ####

## Metabase datos 
  # PINZONES:
# https://datadryad.org/dataset/doi:10.5061/dryad.g6g3h
  # CLIMA:
# https://www.meteoblue.com/es/tiempo/historyclimate/weatherarchive/islas-gal%c3%a1pagos_ecuador_3658931


# Pinzones ####

## leer archivos
pico_total_1975 <- read.csv("Pico_1975.csv")
pico_total_1987 <- read.csv("Pico_1987.csv")
pico_total_1991 <- read.csv("Pico_1991.csv")
pico_total_2012 <- read.csv("Pico_2012.csv")


## Cambiar y corregir los nombres --> ordenar a formato tidy

## 1975 ####

str(pico_total_1975)
names(pico_total_1975)

unique(pico_total_1975$species)
 # Vemos que en nuestros datos hay 2 especies de pinzones: Geospiza fortis y
 # Geospiza scandens

## limpieza a tidy

# Ver si tenemos y dónde NA
print(pico_total_1975[apply(pico_total_1975, 
                            1, 
                            function(x) any(is.na(x)))
                      , ])

# NA en las filas 404 a 407

pico_1975 <- pico_total_1975 %>% 
  rename(beak_length = Beak.length..mm, 
         beak_depth = Beak.depth..mm) %>%
  slice(1:403)   # "slice()" para seleccionar filas y quitar las filas con NA
  
str(pico_1975)      # vemos que hay 403 observaciones de G. fortis y G. scandens

# Seguimos modificando las columnas
pico_1975 <- pico_1975 %>% 
  mutate(year = rep("1975", 403),     # asignar el año correspondiente
         id = c(1:403),               # darles un id a cada observacion
         year = as.factor(year)) %>% 
  select(-band)                       # quitar la columna "band"

str(pico_1975)     # está en formato tidy



## 1987 ####

str(pico_total_1987)
names(pico_total_1987)

unique(pico_total_1987$species)
# Vemos que en nuestros datos hay 2 especies de pinzones: Geospiza fortis y
# Geospiza scandens

## limpieza a tidy

# Ver si tenemos y dónde NA
print(pico_total_1987[apply(pico_total_1987, 
                            1, 
                            function(x) any(is.na(x)))
                      , ])

# NA en las filas 944 a 947

pico_1987 <- pico_total_1987 %>% 
  rename(beak_length = Beak.length..mm, 
         beak_depth = Beak.depth..mm) %>%
  slice(1:943)   # "slice()" para seleccionar filas y quitar las filas con NA

str(pico_1987)      # vemos que hay 943 observaciones de G. fortis y G. scandens

# Seguimos modificando las columnas
pico_1987 <- pico_1987 %>% 
  mutate(year = rep("1987", 943),     # asignar el año correspondiente
         id = c(1:943),               # darles un id a cada observacion
         year = as.factor(year)) %>% 
  select(-band)                       # quitar la columna "band"

str(pico_1987)     # está en formato tidy


## 1991 ####


str(pico_total_1991)
names(pico_total_1991)

unique(pico_total_1991$species)
# Vemos que en nuestros datos hay 2 especies de pinzones: Geospiza fortis y
# Geospiza scandens

## limpieza a tidy

# Ver si tenemos y dónde NA
print(pico_total_1991[apply(pico_total_1991, 
                            1, 
                            function(x) any(is.na(x)))
                      , ])

# NA en las filas 622 a 625

pico_1991 <- pico_total_1991 %>% 
  rename(beak_length = blength, 
         beak_depth = bdepth) %>%
  slice(1:621)   # "slice()" para seleccionar filas y quitar las filas con NA

str(pico_1991)      # vemos que hay 621 observaciones de G. fortis y G. scandens

# Seguimos modificando las columnas
pico_1991 <- pico_1991 %>% 
  mutate(year = rep("1991", 621),     # asignar el año correspondiente
         id = c(1:621),               # darles un id a cada observacion
         year = as.factor(year)) %>% 
  select(-band)                       # quitar la columna "band"

str(pico_1991)     # está en formato tidy


## 2012 ####

str(pico_total_2012)
names(pico_total_2012)

unique(pico_total_2012$species)
# Vemos que en nuestros datos hay 2 especies de pinzones: Geospiza fortis y
# Geospiza scandens

## limpieza a tidy

# Ver si tenemos y dónde NA
print(pico_total_2012[apply(pico_total_2012, 
                            1, 
                            function(x) any(is.na(x)))
                      , ])

# NA en las filas 249 a 265

pico_2012 <- pico_total_2012 %>% 
  rename(beak_length = blength, 
         beak_depth = bdepth) %>%
  slice(1:248)   # "slice()" para seleccionar filas y quitar las filas con NA

str(pico_2012)      # vemos que hay 248 observaciones de G. fortis y G. scandens

# Seguimos modificando las columnas
pico_2012 <- pico_2012 %>% 
  mutate(year = rep("2012", 248),     # asignar el año correspondiente
         id = c(1:248),               # darles un id a cada observacion
         year = as.factor(year)) %>% 
  select(-band)                       # quitar la columna "band"

str(pico_2012)     # está en formato tidy



## Todos ####

# Fusión de los data.frames (1975+1987+1991+2012) en un solo data.frame
# Formato tidy

pico_total <- bind_rows(pico_1975, pico_1987, pico_1991, pico_2012) 

str(pico_total)   # 2215 observaciones

# Hemos modificado el nombre del "id" para que no hayan repeticiones
pico <- pico_total %>% 
  mutate( id = c(1:2215)) 
str(pico)
levels(as.factor(pico$year))

# Así está en formato tidy de los datos de 1975, 1987, 1991 y 2012

pico

## Gráficas ####

year <- as.factor(pico$year)

### G.fortis ####
pico_fortis <- pico %>% 
  filter(species == "fortis")

gf <- ggplot(pico_fortis, 
             aes(x = beak_length, y = beak_depth, colour = year)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm, aes(colour = year)) + 
  scale_colour_manual(values = c("1975" = "red3", 
                                 "1987" = "orange2", 
                                 "1991" = "darkblue",
                                 "2012" = "darkgreen")) +
  labs(title = "Geospiza fortis") +
  xlab("Beak length") +
  ylab("Beak depth") +
  theme_light()


bf <- ggplot(pico_fortis, aes(x = year, y = beak_depth)) +
  geom_boxplot(aes(fill = year)) +
  scale_fill_manual(values = c("1975" = "red3", 
                                 "1987" = "orange2", 
                                 "1991" = "darkblue",
                                 "2012" = "darkgreen")) +
  labs(x = "Year",
       y = "Beak depth",
       fill = "Year",
       title = "Geospiza fortis") +
  theme_light()


### G. scandens ####
pico_scandens <- pico %>% 
  filter(species == "scandens")

gs <- ggplot(pico_scandens, 
             aes(x = beak_length, y = beak_depth, colour = year)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm, aes(colour = year)) +
  scale_colour_manual(values = c("1975" = "red3", 
                                 "1987" = "orange2", 
                                 "1991" = "darkblue",
                                 "2012" = "darkgreen")) +
  labs(title = "Geospiza scandens") +
  xlab("Beak length") +
  ylab("Beak depth") +
  theme_light()

bs <- ggplot(pico_scandens, aes(x = year, y = beak_depth)) +
  geom_boxplot(aes(fill = year)) +
  scale_fill_manual(values = c("1975" = "red3", 
                                 "1987" = "orange2", 
                                 "1991" = "darkblue",
                                 "2012" = "darkgreen")) +
  labs(x = "Year",
       y = "Beak depth",
       fill = "Year",
       title = "Geospiza scandens") + 
  theme_light()


### Ambos ####
# "par(mfrow) = " no funciona con "ggplot2" --> utilizamos "plot_grid()" de "cowplot"

x11()
plot_grid(gf, bf, gs, bs, 
          ncol = 2, nrow = 2)

## Conclusiones ####
# Vemos que hay una aparente correlación positiva entre la longitud del pico
# y la profundidad. Trabajaremos con el dato de la profundidad de pico.


# Clima ####

# Cargar datos 
clima_total_csv <- read.csv("climalimpio.csv")
str(clima_total_csv)
# Day (día), Tº (ºC), Precipitacion (mm), Humedad Relativa (%)


# Pasar a formato tidy

## 1970 - 2015

# Cambiado nombre columnas y eliminando los innecesarios (quedarnos solo con
# la fecha, la temperatura media, las precipitaciones y la humedad relativa media)

clima_total_sucio <- clima_total_csv %>% 
  mutate(RHMean = RelativeHumidityMean) %>% 
  select(c(Day, Tmean, Precipitation, RHMean))

str(clima_total_sucio)

# Utilizar la función "separate()" para separar año/mes/resto porque venía todo 
# junto en la variable "Day
clima_total_year <- separate(clima_total_sucio,
                             col = Day,
                             into = c("Year", "Month"),
                             sep = 4)

str(clima_total_year)

clima_total_month <- separate(clima_total_year,
                              col = Month,
                              into = c("Month", "resto"),
                              sep = 2)
str(clima_total_month)

# Quitar la columna que no nos interesa "resto"
clima_total <- clima_total_month %>% 
  select(-"resto")

str(clima_total)


# Hacer la media de los datos, agrupándolos por mes
c_total <- clima_total %>%
  group_by(Year) %>%
  summarise(Tmean = mean(Tmean, na.rm = TRUE),
            RHmean = mean(RHMean, na.rm = TRUE),
            Pmean = mean(Precipitation, na.rm = TRUE))

str(c_total)
colnames(c_total)

## GRÁFICOS ####

# Nos quedamos con la variable que represente mejor la sequía: la humedad media

### Temperatura media ####
# ggplot(data = c_total, aes(x = Year, y = Tmean)) +
#   # geom_col(fill = "steelblue", alpha = 0.6) + # Gráfico de barras
#   geom_line(aes(group = 1), color = "darkred", size = 1) + # Línea que sigue los valores
#   geom_point(color = "darkred", size = 2) + # Puntos en los valores
#   labs(title = "Temperatura media anual",
#        x = "Año",
#        y = "Temperatura media") +
#   # scale_y_continuous(
#   #   limits = c(0, 20), # Cambiar los límites del eje y
#   #   breaks = seq(0, 20, by = 5) # Cambiar los números que aparecen
#   # ) +
#   theme_minimal() # Tema limpio


### Precipitación media ####
# ggplot(data = c_total, aes(x = Year, y = Pmean)) +
#   # geom_col(fill = "#5e00c2", alpha = 0.6) + # Gráfico de barras
#   geom_line(aes(group = 1), color = "#006358", size = 1) + # Línea que sigue los valores
#   geom_point(color = "#006358", size = 2) + # Puntos en los valores
#   labs(title = "Precipitación media anual",
#        x = "Año",
#        y = "Precipitación media") +
#   theme_minimal() # Tema limpio


### Humedad media ####
ggplot(data = c_total, aes(x = Year, y = RHmean)) +
  # geom_col(fill = "#00a814", alpha = 0.6) + # Gráfico de barras
  geom_line(aes(group = 1), color = "#757d00", size = 1) + # Línea que sigue los valores
  geom_point(color = "#757d00", size = 2) + # Puntos en los valores
  labs(title = "Humedad relativa media anual",
       x = "Año",
       y = "Humedad relativa media") +
  theme_minimal() # Tema limpio

# G. fortis vive de 5 a 10 años
# G. scandens vive de 10 a 15 años


#### G. fortis ####

# Agrupamos los 7 años previos de la medición del pico para G.fortis
# Años de pico: 1975, 1987, 1991, 2012

str(c_total)
humedad <- c_total %>% 
  select(Year, RHmean)
str(humedad)

  # Agrupar de 1970 a 1974
which(humedad$Year == "1970")
which(humedad$Year == "1974")

  # Seleccionar filas y calcular la media
year70_74 <- humedad[1:5, ]
str(year70_74)

mean70_74 <- colMeans(year70_74[sapply(year70_74, is.numeric)])
  # 78.57447 % humedad relativa

  # Agrupar de 1979 a 1986
which(humedad$Year == "1979")
which(humedad$Year == "1986")

# Seleccionar filas y calcular la media
year79_86 <- humedad[10:17, ]
str(year79_86)

mean79_86 <- colMeans(year79_86[sapply(year79_86, is.numeric)])
# 77.89934 % humedad relativa

# Agrupar de 1983 a 1990
which(humedad$Year == "1983")
which(humedad$Year == "1990")

# Seleccionar filas y calcular la media
year83_90 <- humedad[14:21, ]
str(year83_90)

mean83_90 <- colMeans(year83_90[sapply(year83_90, is.numeric)])
# 76.69223 % humedad relativa

# Agrupar de 2004 a 2011
which(humedad$Year == "2004")
which(humedad$Year == "2011")

# Seleccionar filas y calcular la media
year04_11 <- humedad[35:42, ]
str(year04_11)

mean04_11 <- colMeans(year04_11[sapply(year04_11, is.numeric)])
# 76.6157 % humedad relativa

fortis_humedad <- data.frame(mean = c(mean70_74, mean79_86, mean83_90, mean04_11),
                             year = as.factor(c(1975, 1987, 1991, 2012)))
str(fortis_humedad)

bar_rh_f <- ggplot(fortis_humedad, aes(x = year, y = mean)) +
  geom_bar(stat = "identity", aes(fill = factor(year))) +
  scale_fill_manual(values = c("1975" = "red3", 
                                 "1987" = "orange2", 
                                 "1991" = "darkblue",
                                 "2012" = "darkgreen")) +
  coord_cartesian(ylim = c(75, NA)) +
  theme_light() +
  labs(x = "Year", y = "Mean Humidity (%)",
       title = "Geospiza fortis",
       fill = "Year")

x11()
plot_grid(bar_rh_f, bf, 
          ncol = 1, nrow = 2)


#### G. scandens ####

# Agrupamos los 12 años previos de la medición del pico para G. scandens
# Años de pico: 1975, 1987, 1991, 2012

# Agrupar de 1970 a 1974
which(humedad$Year == "1970")
which(humedad$Year == "1974")

# Seleccionar filas y calcular la media
year70_74 <- humedad[1:5, ]
str(year70_74)

mean70_74 <- colMeans(year70_74[sapply(year70_74, is.numeric)])
# 78.57447 % humedad relativa

# Agrupar de 1974 a 1986
which(humedad$Year == "1974")
which(humedad$Year == "1986")

# Seleccionar filas y calcular la media
year74_86 <- humedad[5:17, ]
str(year74_86)

mean74_86 <- colMeans(year74_86[sapply(year74_86, is.numeric)])
# 78.10568 % humedad relativa

# Agrupar de 1978 a 1990
which(humedad$Year == "1978")
which(humedad$Year == "1990")

# Seleccionar filas y calcular la media
year78_90 <- humedad[9:21, ]
str(year78_90)

mean78_90 <- colMeans(year78_90[sapply(year78_90, is.numeric)])
# 77.59413 % humedad relativa

# Agrupar de 1999 a 2011
which(humedad$Year == "1999")
which(humedad$Year == "2011")

# Seleccionar filas y calcular la media
year99_11 <- humedad[30:42, ]
str(year99_11)

mean99_11 <- colMeans(year99_11[sapply(year99_11, is.numeric)])
# 76.76742 % humedad relativa

scandens_humedad <- data.frame(mean = c(mean70_74, mean74_86, mean78_90, mean99_11),
                             year = as.factor(c(1975, 1987, 1991, 2012)))
str(scandens_humedad)

bar_rh_s <- ggplot(scandens_humedad, aes(x = year, y = mean)) +
  geom_bar(stat = "identity", aes(fill = factor(year))) +
  scale_fill_manual(values = c("1975" = "red3", 
                               "1987" = "orange2", 
                               "1991" = "darkblue",
                               "2012" = "darkgreen")) +
  coord_cartesian(ylim = c(75, NA)) +
  theme_light() +
  labs(x = "Year", y = "Mean Humidity (%)",
       title = "Geospiza scandens",
       fill = "Year")

x11()
plot_grid(bar_rh_s, bs, 
          ncol = 1, nrow = 2)

#### Ambos ####

plot_grid(bar_rh_f, bar_rh_s, bf, bs,
          ncol = 2, nrow = 2)

#.###########################################.#

# CONCLUSIONES ####
# Los resultados no son concluyentes. Esto puede ser posible a que el tamaño de 
# los picos está afectado por más variables aparte del clima. 
# Realizaremos un estudio macroevolutivo para comparar variable 
# filogenética y macrogeológica.


# MACROEVOLUTIVO ####

# Metabase
# https://r-packages.io/datasets/geiger
# https://CRAN.R-project.org/package=geodata


## Descarga de paquetes y carga de librerías 
install.packages("tidyr")
install.packages("geiger")
library(geiger)

data(geospiza)
geospizadata <- geospiza$geospiza.data
geospizadata


## CARGAR MÁS LIBRERÍAS
library(rgbif)
library(terra)
library(sf)
library(geodata)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

## Localizacion especies:
  # G. magnirostris
magnirostris <- occ_search(scientificName = "Geospiza magnirostris")
magnirostris <- magnirostris$data
table(magnirostris$country)

  # G. conirostris
conirostris <- occ_search(scientificName = "Geospiza conirostris")
conirostris <- conirostris$data
table(conirostris$country)

  # G. difficilis
difficilis <- occ_search(scientificName = "Geospiza difficilis")
difficilis <- difficilis$data
table(difficilis$country)

  # G. scandens
scandens <- occ_search(scientificName = "Geospiza scandens")
scandens <- scandens$data
table(scandens$country)

  # G. fortis
fortis <- occ_search(scientificName = "Geospiza fortis")
fortis <- fortis$data
table(fortis$country)

  # G. fuliginosa
fuliginosa <- occ_search(scientificName = "Geospiza fuliginosa")
fuliginosa <- fuliginosa$data
table(fuliginosa$country)

  # G. pallida
pallida <- occ_search(scientificName = "Geospiza pallida")
pallida <- pallida$data
table(pallida$country)

  # G. fusca -> "table of extent 0"
fusca <- occ_search(scientificName = "Geospiza fusca")
fusca <- fusca$data
table(fusca$country)

  # G. parvulus -> "table of extent 0"
parvulus <- occ_search(scientificName = "Geospiza parvulus")
parvulus <- parvulus$data
table(parvulus$country)

  # G. pauper
pauper <- occ_search(scientificName = "Geospiza pauper")
pauper <- pauper$data
table(pauper$country)

# Pinaroloxias
Pinaroloxias <- occ_search(scientificName = "Pinaroloxias")
Pinaroloxias <- Pinaroloxias$data
table(Pinaroloxias$country)

# Platyspiza
Platyspiza <- occ_search(scientificName = "Platyspiza")
Platyspiza <- Platyspiza$data
table(Platyspiza$country)

  # G. psittacula
psittacula <- occ_search(scientificName = "Geospiza psittacula")
psittacula <- psittacula$data
table(psittacula$country)

## MAPA CON FALTA DE ISLAS ####

## Mapa islas Galapagos - Ecuador
world <- ne_countries(scale = "medium", returnclass = "sf")

unique(world$continent)

america <- subset(world, continent == "South America")
plot(america$geometry)

ecuador <- subset(america, sovereignt %in% "Ecuador")
plot(ecuador$geometry)

# PROBLEMA: nos faltan islas. Probamos otras funciones para mapear


# para ver coordenadas
ggplot(ecuador$geometry) +
  geom_sf() +
  theme_classic()

# mapa bien hecho
ggplot(ecuador$geometry) +
  geom_sf() +
  theme_classic() +
  coord_sf( # para indicar las coordenadas que queremos que dibuje
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  )

# Mapa base de Ecuador
ggplot() +
  # Mapa de Ecuador
  geom_sf(data = ecuador$geometry, col="black", fill="darkgreen", alpha=0.5) +
  # Puntos de G. magnirostris
  geom_point(data = magnirostris, 
             aes(x = magnirostris$decimalLongitude, y = magnirostris$decimalLatitude), 
             color = "red", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
   # Puntos de G. conirostris
  geom_point(data = conirostris, 
             aes(x = conirostris$decimalLongitude, y = conirostris$decimalLatitude), 
             color = "orange", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # Puntos de G. difficilis
  geom_point(data = difficilis, 
             aes(x = difficilis$decimalLongitude, y = difficilis$decimalLatitude), 
             color = "yellow", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # Puntos de G. scandens
  geom_point(data = scandens, 
             aes(x = scandens$decimalLongitude, y = scandens$decimalLatitude), 
             color = "green", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # Puntos de G. fortis
  geom_point(data = fortis, 
             aes(x = fortis$decimalLongitude, y = fortis$decimalLatitude), 
             color = "darkgreen", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # Puntos de G. fuliginosa
  geom_point(data = fuliginosa, 
             aes(x = fuliginosa$decimalLongitude, y = fuliginosa$decimalLatitude), 
             color = "lightblue", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # Puntos de G. pallida
  geom_point(data = pallida, 
             aes(x = pallida$decimalLongitude, y = pallida$decimalLatitude), 
             color = "#06BEE1", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # Puntos de G. pauper
  # geom_point(data = pauper, 
  #           aes(x = pauper$decimalLongitude, y = pauper$decimalLatitude), 
  #           color = "#F7E733", size = 2,
  #           shape = 1) +
  # theme_classic() +
  # coord_sf(
  #  xlim = c(-92, -89),
  #  ylim = c(0.5, -1.5)
  # ) 
  # G. pauper en la tabla indica como indicio de fosil o especie preservada --> no existe,
  # por eso nos daba problemas

  # Puntos de G. psittacula
  geom_point(data = psittacula, 
             aes(x = psittacula$decimalLongitude, y = psittacula$decimalLatitude), 
             color = "darkblue", size = 1.5,
             alpha = 0.5) +
  theme_classic() +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # # Puntos de Pinaroloxias
  # geom_point(data = Pinaroloxias, 
  #            aes(x = Pinaroloxias$decimalLongitude, y = Pinaroloxias$decimalLatitude), 
  #            color = "violet", size = 1.5,
  #            alpha = 0.5) +
  # theme_classic() +
  # coord_sf(
  #   xlim = c(-92, -89),
  #   ylim = c(0.5, -1.5)
  # ) +
  # # Puntos de G. fusca
  # geom_point(data = fusca, 
  #          aes(x = fusca$decimalLongitude, y = fusca$decimalLatitude), 
  #          color = "pink", size = 1.5,
  #          alpha = 0.5) +
  # theme_classic() +
  # coord_sf(
  #   xlim = c(-92, -89),
  #   ylim = c(0.5, -1.5)
  # ) +
  # NO LO REPRESENTA. "table of extent 0"
  
  # # Puntos de G. parvulus
  # geom_point(data = parvulus, 
  #            aes(x = parvulus$decimalLongitude, y = parvulus$decimalLatitude), 
  #            color = "pink4", size = 1.5,
  #            alpha = 0.5) +
  # theme_classic() +
  # coord_sf(
  #   xlim = c(-92, -89),
  #   ylim = c(0.5, -1.5)
  # ) + 
  # NO LO REPRESENTA. "table of extent 0"
  
  # # Puntos de Platyspiza
  # geom_point(data = Platyspiza, 
  #            aes(x = Platyspiza$decimalLongitude, y = Platyspiza$decimalLatitude), 
  #            color = "gray", size = 1.5,
  #            alpha = 0.5) +
  # theme_classic() +
  # coord_sf(
  #   xlim = c(-92, -89),
  #   ylim = c(0.5, -1.5)
  # ) +
  labs(title = "Distribución especies Islas Galápagos") +
  xlab("Longitud") +
  ylab("Latitud")

# PROBLEMA: el shape_file de las islas nos sale distinto a cómo nos sale al hacer el del clima.
# Cogeremos y modificaremos las funciones para hacer el mismo shape.file

# Lo hacemos más visualmente bonito, modificando el tamaño, transparencia y formas


## Hacer mapa sin ggplot, con R base:
# plot(ecuador$geometry,    xlim = c(-92, -89),
#      ylim = c(0.5, -1.5)
# )
# points(x = fortis$decimalLongitude,
#        y = fortis$decimalLatitude,
#        col="darkred",pch=19,cex=0.7)


?terra::points


## MAPA MEJORADO CON TODAS LAS ISLAS ####
### MAPA CLIMA ####
mundo <- world(resolution=2, 
               path = "mapas/")
mapaislas <- mundo[mundo$NAME_0=="Ecuador",] 


galapagos <- crop(mapaislas, ext(-91.66193,-89,-1.4, 0.5)) # crop(mapa, 
                                        # ext(xmin, xmax, ymin, ymax))
plot(galapagos)

climagalapagos <- worldclim_global(var="prec",
                                   res=0.5, 
                                   path = "mapas/")

precgalapagos <- crop(climagalapagos, ext(-91.66193,-89,-1.5, 0.5))

# SIN GGPLOT
plot(galapagos)

  # Cojo el mes de agosto (08) porque es el mes con menos precipitaciones
plot(precgalapagos$wc2.1_30s_prec_08, add = TRUE)  
lines(galapagos, col="black", lwd=2) 

# CON GGPLOT
  # Lo convierto en un data.frame para poder meterlo en ggplot
precgalapagos_ggplot <- as.data.frame(precgalapagos[["wc2.1_30s_prec_08"]], xy = TRUE)  
  # Hemos utilizado el mes de agosto
names(precgalapagos_ggplot) <- c("long", "lat", "prec")  # Le cambio los nombres para que este mas claro

# Paso el objeto galapagos a sf para poder ponerlo en geom_sf
galapagos_sf <- st_as_sf(galapagos)

# Mapa
  # A color
ggplot() +
  geom_raster(data = precgalapagos_ggplot, aes(x = long, y = lat, fill = prec)) +
  geom_sf(data = galapagos_sf, fill = NA, color = "black", linewidth = 0.7) +
  scale_fill_viridis_c() +
  theme_classic() +
  labs(title = "Clima Islas Galápagos",
       fill = "Precipitación") +
  xlab("Longitud") +
  ylab("Latitud") +
  theme(legend.key.height = unit(1.6, "cm"),
        legend.background = element_rect(color = "black", size = 0.7))

  # En blanco y negro
ggplot() +
  geom_raster(data = precgalapagos_ggplot, aes(x = long, y = lat, fill = prec)) +
  geom_sf(data = galapagos_sf, fill = NA, color = "black", linewidth = 0.7) +
  scale_fill_gradient(low = "white", high = "gray40") +
  theme_classic() +
  labs(title = "Clima Islas Galápagos",
       fill = "Precipitación") +
  xlab("Longitud") +
  ylab("Latitud") +
  theme(legend.key.height = unit(1.6, "cm"),
        legend.background = element_rect(color = "black", size = 0.7))


 ### MAPA DISTRIBUCIÓN ####

# Paso el objeto galapagos a sf para poder ponerlo en geom_sf
# galapagos_sf <- st_as_sf(galapagos)

# SIN PUNTOS
ggplot(data = galapagos_sf) +
  geom_sf(fill = "lightgray", color = "black") +
  theme_light() +
  labs(title = "Mapa Islas Galápagos") +
  xlab("Longitud") +
  ylab("Latitud") +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  )


# CON PUNTOS
ggplot(data = galapagos_sf) +
  geom_sf(fill = "lightgray", color = "black") +
  
  # Puntos de G. magnirostris
  geom_point(data = magnirostris, 
             aes(x = magnirostris$decimalLongitude, y = magnirostris$decimalLatitude), 
             color = "red", size = 1.5,
             alpha = 1) +
  
  # Puntos de G. conirostris
  geom_point(data = conirostris, 
             aes(x = conirostris$decimalLongitude, y = conirostris$decimalLatitude), 
             color = "orange", size = 1.5,
             alpha = 1) +
  
  # Puntos de G. difficilis
  geom_point(data = difficilis, 
             aes(x = difficilis$decimalLongitude, y = difficilis$decimalLatitude), 
             color = "yellow", size = 1.5,
             alpha = 1) +
  
  # Puntos de G. scandens
  geom_point(data = scandens, 
             aes(x = scandens$decimalLongitude, y = scandens$decimalLatitude), 
             color = "green", size = 1.5,
             alpha = 1) +
  
  # Puntos de G. fortis
  geom_point(data = fortis, 
             aes(x = fortis$decimalLongitude, y = fortis$decimalLatitude), 
             color = "brown", size = 1.5,
             alpha = 1) +
  
  # Puntos de G. fuliginosa
  geom_point(data = fuliginosa, 
             aes(x = fuliginosa$decimalLongitude, y = fuliginosa$decimalLatitude), 
             color = "#06BEE1", size = 1.5,
             alpha = 1) +
  
  # Puntos de G. pallida
  geom_point(data = pallida, 
             aes(x = pallida$decimalLongitude, y = pallida$decimalLatitude), 
             color = "purple", size = 1.5,
             alpha = 1) +
  
  # Puntos de G. psittacula
  geom_point(data = psittacula, 
             aes(x = psittacula$decimalLongitude, y = psittacula$decimalLatitude), 
             color = "pink", size = 1.5,
             alpha = 1) +
  
  # # Puntos de Pinaroloxias
  # geom_point(data = Pinaroloxias,
  #            aes(x = Pinaroloxias$decimalLongitude, y = Pinaroloxias$decimalLatitude),
  #            color = "violet", size = 1.5,
  #            alpha = 0.5) +
  
  # Puntos de G. fusca
  # geom_point(data = fusca, 
  #            aes(x = fusca$decimalLongitude, y = fusca$decimalLatitude), 
  #            color = "pink", size = 1.5,
  #            alpha = 0.5) +
                                              # ¡¡FUSCA DA ERROR!!
  
  # Puntos de G. parvulus
  # geom_point(data = parvulus,
  #            aes(x = parvulus$decimalLongitude, y = parvulus$decimalLatitude),
  #            color = "pink4", size = 1.5,
  #            alpha = 0.5) +
                                             # ¡¡PARVULUS DA ERROR!!
  
  # # Puntos de Platyspiza
  # geom_point(data = Platyspiza,
  #            aes(x = Platyspiza$decimalLongitude, y = Platyspiza$decimalLatitude),
  #            color = "black", size = 1.5,
  #            alpha = 0.5) +

  labs(title = "Distribución especies Islas Galápagos") +
  xlab("Longitud") +
  ylab("Latitud") +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  theme_light()

warnings() # Creo que estos avisos no estan influyendo en nada.

# ANALIZO FUSCA Y PARVULUS
exists("fusca")
class(fusca)
nrow(fusca)
names(fusca)
head(fusca[, c("decimalLongitude", "decimalLatitude")])

exists("parvulus")
class(parvulus)
nrow(parvulus)
names(parvulus)
head(parvulus[, c("decimalLongitude", "decimalLatitude")])

# Existe el objeto, pero los datos de longitud y latitud son "NULL". El mapa
# anterior y este salen igual, entiendo que el anterior simplemente los omitía.
# Ocurre lo mismo con Pinarolaxias y Platyspiza.



# Relacionar especies por temperaturas en gráfico densidad o en boxplot
# luego comando dist() para ver distancias entre especies en valores de T, etc,
# para acabar calculando la matriz entre especies para ver la distancia de por 
# ej sp1 con todo el resto de especies y asi con todas. Se hace distancias de Ta
## y de tamaño pico para ver diferencias

# TEMPERATURAS CON TAMAÑO CORPORAL, PRECIPITACION CON PICO
# extract(precipitaciones. especie)


## Unir todas las especies en un solo data.frame

geospiza_dataframe <- rbind(magnirostris, conirostris, difficilis,
                     scandens, fortis, fuliginosa, pallida, psittacula)
  # Error pq no tienen el mismo número de columnas. Utilizamos "bind_rows" del paquete
  # dplyr porque rellena con NA directamente los espacios en blanco no coincidentes.

geospiza_dataframe <- bind_rows(magnirostris, conirostris, difficilis,
                     scandens, fortis, fuliginosa, pallida, psittacula)

  # Nos quedamos solo con las columnas que nos interesan
colnames(geospiza_dataframe)

geospiza_df <- geospiza_dataframe %>% 
  select("scientificName", "decimalLongitude", "decimalLatitude")

geospiza_df  # datos de coordenadas de geospiza
  # datos de precipitacion del mes de agosto
precgalapagos_ggplot  # formato data.frame
precgalapagos$wc2.1_30s_prec_08  # formato raster

  # Hay que extraer el valor de la precipitacion para cada observacion de Geospiza.
  # Para poder hacer eso, hay que renombrar las columnas de longitud y latitud

colnames(geospiza_df) <- c("name", "long", "lat")

geospiza_prec <- raster::extract(precgalapagos$wc2.1_30s_prec_08,
                          geospiza_df[,c(2,3)])
  # Sale error en un inicio pq me hace extract del paquete equivocado. Hay que especificar
  # que es del paquete raster.

tabla_geospiza <- geospiza_df %>% 
  mutate(prec = geospiza_prec$wc2.1_30s_prec_08,
         name = as.factor(name)) 
  # añadir la columna prec a nuestro data.frame de geospiza
  # Pasar como factor con niveles la columna de especies "name"
head(tabla_geospiza)
levels(tabla_geospiza$name)  # Hay repetidos porque hay diferentes autores y subespecies
   # hay que agruparlos de alguna manera: por Geospiza + 3 primeras letras

geospiza_con_resto <- separate(tabla_geospiza,
                              col = name,
                              into = c("name", "resto"),
                              sep = 12)
geospiza_sin_resto <- select(geospiza_con_resto, -"resto")
head(geospiza_sin_resto)

geospiza_sin_resto_f <- geospiza_sin_resto %>% 
  mutate(name = as.factor(name))
head(geospiza_sin_resto_f)
levels(geospiza_sin_resto_f$name)  # 8 Niveles porque hay 8 especies. Ahora a renombrar

levels(geospiza_sin_resto_f$name) <- c("Geospiza conirostris", 
                                       "Geospiza debilirostris", 
                                       "Geospiza difficilis",
                                       "Geospiza fortis",
                                       "Geospiza fuliginosa",
                                       "Geospiza magnirostris",
                                       "Geospiza nebulosa",
                                       "Geospiza pallida",
                                       "Geospiza psittacula",
                                       "Geospiza scandens")
levels(geospiza_sin_resto_f$name)

# Vemos que han aparecido 2 especies de la nada (G. debilirostris y G. nebulosa).
# No sé de dónde salen, quizás estaban metidos con los datos de otra especie como 
# una extensión. Pero como no tienen datos, los quitamos

geospiza_bien <- geospiza_sin_resto_f[!geospiza_sin_resto_f$name %in% 
                                        c("Geospiza debilirostris", 
                                          "Geospiza nebulosa"), ]
## BOXPLOTS ####
ggplot(geospiza_bien, aes(x = prec, y = name)) +
  geom_boxplot(fill = "lightblue") +
  theme_light() +
  labs(title = "Precipitación sufrida por especie") +
  xlab("Precipitación") +
  ylab("Especie")

  # Vemos que la especie psittacula no tiene ningún dato de precipitacion en 
  # las coordenadas en las que se le ha observado. Comprobamos sus localizaciones
  # en el mapa y sus datos.

which(geospiza_bien$name == "Geospiza psittacula") # filas 3000 a 3034
geospiza_bien[3000:3034,] 
  # Vemos que solo hay datos de la ubicación de 1 individuo. Además, ese individuo no
  # tiene datos de precipitación.
  # Comprobamos con el mapa

ggplot(data = galapagos_sf) +
  geom_sf(fill = "lightgray", color = "black") +
  theme_light() +
  labs(title = "Mapa Islas Galápagos") +
  xlab("Longitud") +
  ylab("Latitud") +
  coord_sf(
    xlim = c(-92, -89),
    ylim = c(0.5, -1.5)
  ) +
  # Puntos de Geospiza psittacula
  geom_point(data = psittacula, 
             aes(x = psittacula$decimalLongitude, y = psittacula$decimalLatitude), 
             color = "pink", size = 1.5,
             alpha = 1) 
  # Correcto, vemos que solo hay 1 observación de esta especie. Por ello, la quitamos
# de nuestros datos porque se nos imposibilita trabajar con ella

geospiza_final <- geospiza_bien[!geospiza_bien$name %in% "Geospiza psittacula",]

ggplot(geospiza_final, aes(x = prec, y = name)) +
  geom_boxplot(fill = "lightblue") +
  theme_light() +
  labs(title = "Precipitación sufrida por especie") +
  xlab("Precipitación") +
  ylab("Especie")

# Queremos hacer una matriz comparando la precipitación especie por especie.
# Lo que compararemos será la precipitación media del mes más seco (agosto)

geospiza_final
  # Vemos que nuestros datos tienen NA. Habrá que quitarlos
geospiza_sin_na <- na.omit(geospiza_final)

  # Hacemos la media de precipitaciones por especie
media_geospiza <- geospiza_sin_na %>% 
  group_by(name) %>% 
  summarise(media = mean(prec))
media_geospiza
  # Ordenamos las especies según el orden que tiene la base de datos geo_data (la que
  # usaremos después)para que luego se puedan trabajar conjuntamente
media_geospiza_ordenada <- media_geospiza[c(5,1,2,7,3,4,6),]

  # Realizamos la matriz comparando las diferencias entre los valores especie por 
  # especie
distprec <- dist(media_geospiza_ordenada[,2])
distprec


# Realizamos también una matriz comparando especie a especie con respecto al tamaño
# de sus picos.

geospizadata  # los datos que tenemos
  # Hay que quitar las especies que no tenemos en la matriz de precipitaciones
geo_data <- geospizadata[-c(8,9,10,11,12,13), ]
geo_data

  # Queremos hacer la matriz sobre la diferencia de profundidad de los picos
distgeospiza <- dist(geo_data[,4])
distgeospiza

# Nos hemos encargado antes de que las especies se encuentren en el mismo orden
# para poder correlacionar los resultados obtenidos
distprec
distgeospiza

  
# Los pasamos a data.frame para trabajar mejor con los datos
distprec_df <- as.data.frame(distprec)
distprec_df

distgeospiza_df <- as.data.frame(distgeospiza)
distgeospiza_df
colnames(distgeospiza_df)[1] <- "y"

  # Hacemos otro data.frame que reúnan los datos:
dist_total_df <- cbind(distprec_df, distgeospiza_df)
dist_total_df
  # Renombramos columnas para no confundirnos
colnames(dist_total_df) <- c("distprec", "distbeak")
dist_total_df


## Realizamos un gráfico de densidad que nos permita comprobar los datos
dist_ggplot <- ggplot(dist_total_df, aes(x = distprec, y = distbeak)) +
                 geom_point() +
                 geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs (title = "Relación entre pico y precipitación") +
        xlab("Diferencia de precipitación") +
        ylab("Diferencia en la profundidad del pico")

dist_ggplot

## RESULTADOS ####
# Vemos que no se muestra una diferencia entre la diferencia de picos y la diferencia
# de precipitación. La hipótesis inicial (a mayor diferencia de precipitación que
# sufren dos especies, más se notará en su diferencia de caracteres) no se cumple.




