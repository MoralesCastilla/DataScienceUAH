# QUE HICIMOS PARA LIMPIAR LOS DATOS Y LAS COLUMNAS 
# antes de filtrar con GBIF
# primero saber meter una tabla de datos al R
#voy a probar meter una database en rstudio
rm(list=ls())
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(terra)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(googleway)
library(ncdf4)
library(ggplot2)
library(geodata)
library(mapSpain)
theme_set(theme_bw())

#he creado un proyecto y creado una carpeta como vimos en clase para que R vaya siempre ha buscar a esa carpeta 
#como sabemos que va a buscar asi esto no se lo decimos 
datos_libelulas <- read_tsv("1_data/data_odonata.csv")
#no se por que me funciona con tsv y no con csv creo que es algo de tabulador pero no se que significa 
head(datos_libelulas)

#view(datos_libelulas)
names(datos_libelulas)               
datos_lib_sel <- datos_libelulas |> #solo quiero estas columnas 
  dplyr::select("species", "countryCode", "decimalLatitude" , "decimalLongitude" ,  "elevation"  ,  "year" ) |>  arrange(year)  #para ordenar por año 
head(datos_lib_sel)
#quiero saber cuantas especies distintas tengo
unique(datos_lib_sel$species)
#quiero eliminar las especies que no estan bien identificadas 
#creo que se hace con la exclamacion  
datos_lib_sel <- filter(datos_lib_sel, !is.na(species), !is.na(year), 
                      !is.na(decimalLatitude), !is.na(decimalLongitude) )
#asi se filtra eliminando las filas con NA en esas columnas solo 
#view(datos_lib_sel)
#lo vuelvo a comprobar
unique(datos_lib_sel$species)
#con esto me quedo

##2 COMO HICIMOS LOS MAPAS DE LAS ESPECIES
#es mejor lo de poner las coordenadas de la peninsula porque queda mas exacto
mapa_libelulas <- ggplot(data = map_data("world")) +
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "grey" #color del pais 
               , color = "white" #color del borde 
  ) +
  geom_point(data = datos_lib_sel,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 color = species 
                 #esto significa que asigna colores a cada especie
             ),
             size = 0.7) +
  coord_fixed(xlim = c(-12, 5),
              ylim = c(35, 45),
              ratio = 0.9) +
  ggtitle("Mapa de distribución libélulas en España") +
  theme_classic() +
  theme(  panel.background = element_rect(colour = "black" 
                                          #color del borde del panel
                                          , linewidth = 1)  ) 

##sale un mapa gigante con 96 especies
ggsave(filename = "mapas/mapa_especies_libelulas.jpg", 
       plot = mapa_libelulas,
       dpi = 300, 
       height = 30,
       width = 30,
       units = "cm")
##guardar el mapa

##3 OTRA MANERA DE HACER MAPAS
###RIQUEZA Y VARIABLES AMBIENTALES
##primer intento
#he creado un proyecto y creado una carpeta como vimos en clase para que R 
#vaya siempre ha buscar a esa carpeta 
#como sabemos que va a buscar asi esto no se lo decimos 
##Separamos por tiempos para usarlo en los mapas 
libelulas_old <- subset(datos_lib_sel, year %in% 2008:2014 )
libelulas_new <- subset(datos_lib_sel, year %in% 2015:2021 )


###PRECIPITACIÓN
##1-PENINSULA
## cargar datos de precipitación media
precip = rast("1_data/RR.nc")
years=format(time(precip), "%Y")

#Mapa suma de precipitaciones anuales 2008-2014
precip0814_data = subset(precip, years %in% 2008:2014)
precip0814 = app(precip0814_data, sum, na.rm = TRUE)


#Mapa de suma de precipitacioens anuales 2015-2021
precip1521_data = subset(precip, years %in% 2015:2021)
precip1521 = app(precip1521_data, sum, na.rm = TRUE)

#Hacer los mapas 
niveles_PP <- seq(0, 12100, by = 2000)
plot(precip0814, main="Precipitaciones 2008-2014", breaks=niveles_PP)
plot(precip1521, main="Precipitaciones 2015-2021", breaks=niveles_PP)

##MAPAS DE TEMPERATURA
Tmax_data = rast("1_data/Tmax.nc")
class(time(Tmax_data))
years=format(time(Tmax_data), "%Y")
#Primero NEW 
Tmax_data_new = subset(Tmax_data, years %in% 2015:2021)
Tmax_data_new <- mask(Tmax_data_new, mapa_peninsula)
##elegimos las temperaturas maximas de agosto y hacemos la media 
meses = format(time(Tmax_data_new), "%m")
Tmax_data_new_ag = subset(Tmax_data_new, meses=="08")
Tmax_mean_new_ag = app(Tmax_data_new_ag, mean, na.rm = TRUE)

##añadir al mapa
mapa_peninsula <- mapamundo[mapamundo$NAME_0 %in% c("Spain","Portugal"),]
lines(mapa_peninsula)
Tmax_mean_new_ag <- mask(Tmax_mean_new_ag, mapa_peninsula)


#Ahora old=2008-2014
Tmax_data_old = subset(Tmax_data, years %in% 2008:2014)
Tmax_data_old <- mask(Tmax_data_old, mapa_peninsula)

meses = format(time(Tmax_data_old), "%m")
Tmax_data_old_ag = subset(Tmax_data_old, meses=="08")
Tmax_mean_old_ag = app(Tmax_data_old_ag, mean, na.rm = TRUE)

Tmax_mean_old_ag <- mask(Tmax_mean_old_ag, mapa_peninsula)


##hacer los mapas
niveles_Tmax <- seq(10, 40, by=5)
plot(Tmax_mean_new_ag, main="Tmax media en Agosto 2015-2021", breaks=niveles_Tmax)
plot(Tmax_mean_old_ag , main="Tmax media en Agosto 2008-2014", breaks=niveles_Tmax)

##cargar los mapas de base
mapamundo <- world(resolution = 2,
                   path="~/4_sandbox")
mapa_peninsula <- mapamundo[mapamundo$NAME_0 %in% c("Spain","Portugal"),]
mapo <- crop(mapa_peninsula, ext(-10,5,35,44))
mapo <- mask(precip0814, mapa_peninsula)
##Hemos dejado solita a la peninsula
#Ponerle líneas al mapa
lines(mapa_peninsula)
## crear raster de referencia
rastercito = mapo
nsites=length(values(rastercito)[!is.na(values(rastercito))])
values(rastercito)[!is.na(values(rastercito))] <-1:nsites
plot(rastercito)
#Sale un mapa degradado
#Ponerle líneas al mapa
lines(mapa_peninsula)

##TEMPERATURA MÍNIMA
Tmin_data = rast("1_data/Tmin.nc")
class(time(Tmin_data))
years = format(time(Tmin_data), "%Y")

#NEW (2015–2021)
Tmin_data_new = subset(Tmin_data, years %in% 2015:2021)
Tmin_data_new <- mask(Tmin_data_new, mapa_peninsula)

#Elegimos temperaturas mínimas de enero y hacemos la media
meses = format(time(Tmin_data_new), "%m")
Tmin_data_new_jan = subset(Tmin_data_new, meses == "01")
Tmin_mean_new_jan = app(Tmin_data_new_jan, mean, na.rm=TRUE)
Tmin_mean_new_jan <- mask(Tmin_mean_new_jan, mapa_peninsula)

#OLD (2008–2014)
Tmin_data_old = subset(Tmin_data, years %in% 2008:2014)
Tmin_data_old <- mask(Tmin_data_old, mapa_peninsula)

meses = format(time(Tmin_data_old), "%m")
Tmin_data_old_jan = subset(Tmin_data_old, meses=="01")
Tmin_mean_old_jan = app(Tmin_data_old_jan, mean, na.rm=TRUE)
Tmin_mean_old_jan <- mask(Tmin_mean_old_jan, mapa_peninsula)

##Hacer los mapas
niveles_Tmin <- seq(-10, 10, by=2.5)

plot(Tmin_mean_new_jan,
     main="Tmin media en Enero 2015-2021",
     breaks=niveles_Tmin)

plot(Tmin_mean_old_jan,
     main="Tmin media en Enero 2008-2014",
     breaks=niveles_Tmin)

##MAPAS DE RIQUEZA 
#Raster península
#RIQUEZA OLD
spsnames_old = unique(libelulas_old$species)
datasps1 = subset(libelulas_old, species == spsnames_old[1])
points(datasps1$decimalLongitude,datasps1$decimalLatitude)
coordssps1 = data.frame(x=datasps1$decimalLongitude,y=datasps1$decimalLatitude)

#Esto es la matriz vacía
sort(unique(extract(rastercito, coordssps1)[,2]))
matrizguardar = array(0, dim=c(nsites,length(spsnames_old)))
colnames(matrizguardar) = spsnames_old

#Bucle para llenar la matriz vacía con presencia/ausencia de todas las especies
#Lo hace noventa y mazo veces y lo guarda en la matriz
for(i in 1:length(spsnames_old)){
  spsi = spsnames_old[i]
  print(spsi)
  datasps1 = subset(libelulas_old, species == spsi)
  coordssps1 = data.frame(x=datasps1$decimalLongitude,y=datasps1$decimalLatitude)
  celdas = sort(unique(extract(rastercito, coordssps1)[,2]))
  matrizguardar[celdas,i]=1 
}

riqueza_old=rowSums(matrizguardar)

#Convertir rastercito al nuevo raster de riqueza
rasterriqueza_old = rastercito
values(rasterriqueza_old)[!is.na(values(rasterriqueza_old))] <- riqueza_old

niveles_riqueza <- seq(0,50, by=10)
plot(rasterriqueza_old, main="Riqueza Odonatos 2008-2014", breaks= niveles_riqueza)
  
  
##RIQUEZA NEW
## RIQUEZA Odonatos 2015-2021
spsnames_new = unique(libelulas_new$species)
datasps1 = subset(libelulas_new, species == spsnames_new[1])
coordssps1 = data.frame(
  x = datasps1$decimalLongitude,
  y = datasps1$decimalLatitude)

# Crear matriz vacía presencia/ausencia
sort(unique(extract(rastercito, coordssps1)[,2]))
matrizguardar = array( 0,
  dim = c(nsites, length(spsnames_new)))
colnames(matrizguardar) = spsnames_new

# Bucle para rellenar matriz
for(i in 1:length(spsnames_new)){
  spsi = spsnames_new[i]
  print(spsi)
  datasps1 = subset(libelulas_new, species == spsi)
  coordssps1 = data.frame(
    x = datasps1$decimalLongitude,
    y = datasps1$decimalLatitude
  )
  celdas = sort(unique(extract(rastercito, coordssps1)[,2]))
  matrizguardar[celdas, i] = 1
}

# Calcular riqueza por celda
riqueza_new = rowSums(matrizguardar)
# Crear raster de riqueza
rasterriqueza_new = rastercito
values(rasterriqueza_new)[!is.na(values(rasterriqueza_new))] <- riqueza_new
##mapa
plot(rasterriqueza_new,
  main = "Riqueza Odonatos 2015-2021",
  breaks = niveles_riqueza)

###MAPAS COMUNIDADES AUTONOMAS 
##CATALUÑA Y VALENCIA##
#install.packages("mapSpain", dependencies = TRUE)
##Extraer las CCAA
codelist <- mapSpain::esp_codelist |>
  dplyr::select(cpro, codauto) |>
  distinct()
names(codelist)

# Extraer CCAA
CatVal <- esp_get_ccaa() |>
  filter(ine.ccaa.name %in% c("Cataluña", "Comunitat Valenciana"))
#hay que poner el %in% cuando es mas de una

#ahora como pongo los datos
#Primero sacamos los mapas de precipitaciones 
#elegimos el mes de abril
##PP old
meses = format(time(precip0814_data), "%m")
PP_data_0814april = subset(precip0814_data, meses=="04")
PP_sum_0814 = app(PP_data_0814april, sum, na.rm = TRUE)
##new
PP_data_1521april = subset(precip1521_data, meses=="04")
PP_sum_1521 = app(PP_data_1521april, sum, na.rm = TRUE)

#OLD
PP_sum_old_catval <- crop(
  PP_sum_0814,
  CatVal
)
PP_old_catval <- mask(
  PP_sum_old_catval,
  CatVal
)

#NEW
PP_sum_new_catval <- crop(
  PP_sum_1521,
  CatVal
)
PP_new_catval <- mask(
  PP_sum_new_catval,
  CatVal
)


breaks <- seq(0, 1200, by = 200) #quiero igualar las leyendas 
plot(PP_old_catval, main= "Precipitación Abril 2008-2014", breaks=breaks )
plot(PP_new_catval, main= "Precipitación Abril 2015-2021", breaks=breaks)

##Ahora los mapas de Tmax
#OLD
Tmax_total_old_catval <- crop(Tmax_mean_old_ag,  CatVal)
Tmax_old_catval <- mask(
  Tmax_total_old_catval, CatVal)
#NEW
Tmax_total_new_catval <- crop(Tmax_mean_new_ag,  CatVal)
Tmax_new_catval <- mask(
  Tmax_total_new_catval, CatVal)

niveles_Tmax <- seq(10, 40, by=5)
plot(Tmax_old_catval, main= "Tmax Agosto 2008-2014", breaks=niveles_Tmax,
     plg=list(title= "ºC"))
plot(Tmax_new_catval, main= "Tmax Agosto 2015-2021", breaks=niveles_Tmax,
     plg=list(title= "ºC"))

##AHORA LA RIQUEZA
raster_riqueza_catval_old = PP_old_catval
nsites_old=length(values(raster_riqueza_catval_old)
                  [!is.na(values(raster_riqueza_catval_old))])
values(raster_riqueza_catval_old)[!is.na(values(raster_riqueza_catval_old))] <-1:nsites_old
spsnames_catval_old <- unique(libelulas_old$species)

#Crear matriz presencia-ausencia
matriz_catval_old <- array(0,
                           dim = c(nsites_old, length(spsnames_catval_old)))
colnames(matriz_catval_old) <- spsnames_catval_old

# Bucle de riqueza
for(i in 1:length(spsnames_catval_old)){
  spsi = spsnames_catval_old[i]
  print(spsi)
  datasps1 = subset(libelulas_old, species == spsi)
  coordssps1 = data.frame(
    x=datasps1$decimalLongitude,
    y=datasps1$decimalLatitude )
  celdas = sort(unique(
    terra::extract(raster_riqueza_catval_old, coordssps1)[,2]))
  matriz_catval_old[celdas,i]=1 
}

#Calcular riqueza
riqueza_catval_old <- rowSums(matriz_catval_old)

values(raster_riqueza_catval_old)[
  !is.na(values(raster_riqueza_catval_old))] <- riqueza_catval_old


##NEW
raster_riqueza_catval_new = PP_new_catval
nsites_new=length(values(raster_riqueza_catval_new)
                  [!is.na(values(raster_riqueza_catval_new))])

values(raster_riqueza_catval_new)[!is.na(values(raster_riqueza_catval_new))] <-1:nsites_new

spsnames_catval_new <- unique(libelulas_new$species)

#Crear matriz presencia-ausencia
matriz_catval_new <- array(0,
                           dim = c(nsites_new, length(spsnames_catval_new)))

colnames(matriz_catval_new) <- spsnames_catval_new

# Bucle de riqueza
for(i in 1:length(spsnames_catval_new)){
  spsi = spsnames_catval_new[i]
  print(spsi)
  datasps1 = subset(libelulas_new, species == spsi)
  coordssps1 = data.frame(
    x=datasps1$decimalLongitude,
    y=datasps1$decimalLatitude)
  celdas = sort(unique(
    terra::extract(raster_riqueza_catval_new, coordssps1)[,2]))
  matriz_catval_new[celdas,i]=1 
}

#Calcular riqueza
riqueza_catval_new <- rowSums(matriz_catval_new)
values(raster_riqueza_catval_new)[
  !is.na(values(raster_riqueza_catval_new))] <- riqueza_catval_new


#ahora sacar los mapas
niveles_riqueza <- seq(0, 50, by = 10)
plot(raster_riqueza_catval_old,
     main= "Riqueza 2008-2014",
     breaks=niveles_riqueza)
lines(CatVal)

plot(raster_riqueza_catval_new,
     main= "Riqueza 2015-2021",
     breaks=niveles_riqueza)


####ANALISIS DE PLOTS
###RELACIÓN PRECIPITACIÓN Y RIQUEZA 2008-2014 (OLD)
#Con esto ves si son rasters. Si no es raster no se van a alinear
class(precip0814) #Suma de las recipitaciones aniales 2008-2014
class(rasterriqueza_old) #Riqueza de 2008-2014

#Crear un dataframe para hacer el gráfico.
#Hay más datos de precipitación que de riqueza
#así que hay que hacer resample para ajustarlo.
#Hago el resample sobre la precipitación y después mask 
#en la riqueza porque tiene más
#datos (supongo que porque incluye Francia)
precip0814r <- resample(precip0814, rasterriqueza_old)
precip0814rm <- mask(precip0814r, rasterriqueza_old)

#Dataframe
ppriq0814 <- c(rasterriqueza_old, precip0814rm)
ppriq0814 <- as.data.frame(ppriq0814, xy = TRUE, na.rm = TRUE)
names(ppriq0814)
ncol(ppriq0814) #4 Columnas. importante que no se hayan perdido columnas
#view(ppriq0814) es una tabla con coordenadas, un valor de
#riqueza y un valor de precipitación

#Le cambio los nombres a las columnas. x y son coordenadas
# y en este caso tanto la riqueza como la precipitción son sumas
names(ppriq0814) <- c("x", "y", "riqueza", "precipitacion")


#Hexbin con colorinchis
#Mola pero igual es un poco redundante 
ggplot(ppriq0814, aes(x = precipitacion, y = riqueza)) +
  stat_summary_hex(aes(z = riqueza), fun = mean, bins = 30) +
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Riqueza por precipitación anual 2008-2014",
       fill = "Número de especies") +
  theme_classic()

#Creo que con este es como mejor se ve
ggplot(ppriq0814, aes(x = precipitacion, y = riqueza)) +
  geom_point(alpha = 0.4, color = "darkblue") + 
  geom_smooth(method = "loess", color = "red") +  
  labs(title = "Riqueza-Precipitación 2008-2014",
       x = "Precipitación (mm)",
       y = "Riqueza de especies") +
  theme_minimal()


###RELACIÓN PRECIPITACIÓN Y RIQUEZA 2015-2021 (NEW)

#Con esto ves sis son rasters. Si no es raster no se van a alinear
class(precip1521)
class(rasterriqueza_new)

#Crear un dataframe para hacer el gráfico.
precip1521r <- resample(precip1521, rasterriqueza_new)
precip1521rm <- mask(precip1521r, rasterriqueza_new)

#Dataframe
ppriq1521 <- c(rasterriqueza_new, precip1521rm)
ppriq1521 <- as.data.frame(ppriq1521, xy = TRUE, na.rm = TRUE)

#Le cambio los nombres a las columnas 
names(ppriq1521) <- c("x", "y", "riqueza", "precipitacion")

#Dispersión de puntos
ggplot(ppriq1521, aes(x = precipitacion, y = riqueza)) +
  geom_point(alpha = 0.4, color = "darkblue") + 
  geom_smooth(method = "loess", color = "red") +  
  labs(title = "Riqueza-Precipitación 2015-2021",
       x = "Precipitación (mm)",
       y = "Riqueza de especies") +
  theme_minimal()

#Hexbin con colorinchis
#Mola pero igual es un poco redundante 
ggplot(ppriq1521, aes(x = precipitacion, y = riqueza)) +
  stat_summary_hex(aes(z = riqueza), fun = mean, bins = 30) +
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Riqueza por precipitación anual 2015-2021",
       fill = "Número de especies") +
  theme_classic()

#Juntar ambos rasters en un dataframe
temax0814r  <- resample(Tmax_data_old, rasterriqueza_old, method = "bilinear")
temax0814rm <- mask(temax0814r, rasterriqueza_old)

#Combinamos los dos rasters en un objeto intermedio (un stack)
stack_0814 <- c(rasterriqueza_old, temax0814rm)

#Pasamos al dataframe el stack_0814, NO el temax0814
temaxriq0814 <- as.data.frame(stack_0814, xy = TRUE, na.rm = TRUE)

#Le cambiamos los nombres 
names(temaxriq0814) <- c("x", "y", "riqueza", "temperatura")

#Verificación
ncol(temaxriq0814) 
names(temaxriq0814)
view(temaxriq0814)


###RELACIÓN TEMPERATURA MEDIA MÁXIMA Y RIQUEZA 2015-2021
#Crear un dataframe para hacer el gráfico.
temax1521r  <- resample(Tmax_data_new, rasterriqueza_new, method = "bilinear")
temax1521rm <- mask(temax1521r, rasterriqueza_new)

#Combinamos los dos rasters en su objeto intermedio
stack_1521 <- c(rasterriqueza_new, temax1521rm)

#Pasamos al dataframe el 'stack_1521'
temaxriq1521 <- as.data.frame(stack_1521, xy = TRUE, na.rm = TRUE)

#Le cambiamos los nombres de forma segura
names(temaxriq1521) <- c("x", "y", "riqueza", "temperatura")

#Verificación
ncol(temaxriq1521) 
#view(temaxriq1521)

###GRÁFICOS DE TEMPERATURA MÁXIMA MEDIA Y RIQUEZA
#dev.off() para reiniciar lo que genera los plots (mi portatil embeces
# opina que no quiere trabajar más y tengo que ejecutar eso y vuelve a funcionar)

#2008-2014 riqueza y temperatura máxima
#Hay que ajustar los ejes para que queden más bonitos
ggplot(subset(temaxriq0814, riqueza > 0), aes(x = temperatura, y = riqueza)) +
  geom_point(alpha = 0.4, color = "darkblue") + 
  geom_smooth(method = "loess", color = "red") +  
  labs(title = "Riqueza-Temperatura máxima 2008-2014",
       x = "Temperatura máxima media (ºC)",
       y = "Riqueza de especies") +
  theme_light() #Mirar bien que theme nos mola más


#2014-2021 riqueza y temperatra máxima
ggplot(subset(temaxriq1521, riqueza > 0), aes(x = temperatura, y = riqueza)) +
  geom_point(alpha = 0.4, color = "darkblue") + 
  geom_smooth(method = "loess", color = "red") +  
  labs(title = "Riqueza-Temperatura máxima 2008-2014",
       x = "Temperatura máxima media ºC",
       y = "Riqueza de especies") +
  theme_light()

##6.1 RIQUEZA OLD, 2008-2014
#Qué rasters estoy utilizando
class(precip0814rm)
class(temax0814rm)
class(rasterriqueza_old)

#Dataframe
raster_combinado <- c(rasterriqueza_old, temax0814rm, precip0814rm)
temax_precip_riq0814 <- as.data.frame(raster_combinado, na.rm = TRUE)
names(temax_precip_riq0814)
ncol(temax_precip_riq0814)
names(temax_precip_riq0814) <- c("riqueza", "temax", "precip") #Que columna es cada
#view(temax_precip_riq0814) #es una tabla con riqueza/temp/precip

#Hexbin precip/riqueza0814/temp
#quitar cuando la riqueza es 0 para que no salgan hexágonos de más (subset)
ggplot(subset(temax_precip_riq0814, riqueza > 0), aes(y = temax, x = precip)) +
  stat_summary_hex(aes(z = riqueza), fun = mean, bins = 30,) +
  scale_fill_viridis_c(option = "viridis", name = "Riqueza") +
  labs(title = "Riqueza por temperatura y precipitación 2008-2014",
       y = "Temperatura máxima media (ºC)",
       x = "Precipitación total (mm)")

#Bubbleplot con las tres cosas 
ggplot(subset(temax_precip_riq0814, riqueza > 0), aes(x = precip, y = temax,)) +
  labs(title = "Riqueza por temperatura y precipitación 2008-2014",
       x = "Suma de la precipitación (mm)",
       y = "Temperatura máxima media (ºC)") +
  geom_point(
    aes(color = riqueza),
    size = 4,
    alpha = 0.7,
  ) +
  scale_color_viridis_b(option = "viridis", name = "Riqueza") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
  ) 

##6.2 RIQUEZA NEW, 2014-2021
#Qué rasters estoy utilizando
class(precip1521rm)
class(temax1521rm)
class(rasterriqueza_old)

#Dataframe 
raster_combinado <- c(rasterriqueza_new, temax1521rm, precip1521rm)
temax_precip_riq1521 <- as.data.frame(raster_combinado, na.rm = TRUE)
names(temax_precip_riq1521)
ncol(temax_precip_riq1521)
names(temax_precip_riq1521) <- c("riqueza", "temax", "precip") #Que columna es cada
view(temax_precip_riq1521) #es una tabla con riqueza/temp/precip

#Bubbleplot temperatura/precipitación y riqueza para 2015-2021
ggplot(subset(temax_precip_riq1521, riqueza_new > 0), aes(x = precip, y = temax,)) +
  labs(title = "Riqueza por temperatura y precipitación 2014-2021",
       x = "Suma de la precipitación (mm)",
       y = "Temperatura máxima media (ºC)") +
  geom_point(
    aes(color = riqueza_new),
    size = 4,
    alpha = 0.7,
  ) +
  scale_color_viridis_b(option = "viridis", name = "Riqueza") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
  ) 


##plotear random
plot(mapa_Tmax_new, riqueza_new)


#GGPLOT
datos_new_ggplot = data.frame(mapa_Tmax_new, riqueza_new)
ggplot(datos_new_ggplot,
       aes(x = mapa_Tmax_new, y = riqueza_new)) +
  geom_point() +
  geom_smooth()

ggplot(data.frame(mapa_Tmax_old, riqueza_old),
       aes(x = mapa_Tmax_old, y = riqueza_old)) +
  geom_point() +
  geom_smooth()


ggplot(datos_new_ggplot,
       aes(x = mapa_Tmax_new, y = riqueza_new)) +
  geom_bin2d() +
  scale_fill_continuous()

ggplot(data.frame(mapa_Tmax_old, riqueza_old),
       aes(x = mapa_Tmax_old, y = riqueza_old)) +
  geom_bin2d() +
  scale_fill_continuous()




#Violines

# Para que no salga un violín gigante hay que dividir la precipitación
#en trozos más pequeños
ppriq0814$rango_lluvia <- cut_number(ppriq0814$precipitacion, n = 5, 
                                     labels = c("Muy Baja", "Baja", "Media", "Alta", "Muy Alta"))


ggplot(ppriq0814, aes(x = rango_lluvia, y = riqueza, fill = rango_lluvia)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "Riqueza por rangos de Precipitación 2008-2014",
       x = "Intervalos de precipitación", y = "Riqueza")






