################################################################# #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'       alumno: Juan Julián Martínez
##'
################################################################# #


## Limpia el ambiente de trabajo de variables de otros trabajos.
rm(list=ls())
options(stringsAsFactors = FALSE)


## Establece el directorio de trabajo (wd)
getwd()  
setwd("C:/Users/julia/OneDrive/Documentos/GitHub/DataScienceUAH/hackathon2025/juan_julian/")


## Carga los paquetes
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
#install.packages("rgbif")
library(rgbif)
theme_set(theme_bw())
#install.packages("raster")
library(raster)
## 1. Carga de datos espaciales ##
############################### ##

##' Busca, descarga y lee datos de temperaturas máximas diarias para 
##' toda Europa, correspondientes al periodo 1980-1994.
##' https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles 

  # Para leer los datos usamos la función rast. 

  # Le asignamos un nombre para guardarlos como una variable.

datos80_94 <- rast("tx_ens_mean_0.25deg_reg_1980-1994_v31.0e.nc")

datos80_94



##' Lee los datos de Parques Nacionales de España (en github).

  # Para leer los datos utilizamos la función st_read().

  # Asignamos el nombre datos_parq_nac a la variable con los datos de los 
    # parques nacionales.

datos_parq_nac <- st_read("~/GitHub/DataScienceUAH/hackathon2025/data/ENP.shp")

datos_parq_nac



##' Lee los datos de temperaturas mensuales de España, para enero y julio (en github).

  # Para abrir los datos utilizamos la función "rast()", la misma que hemos
    # usado para abrir los datos de las temperaturas.

media_temp_esp_enero <- raster("temp.Spain.jan07.tif")

media_temp_esp_enero

media_temp_esp_julio <- raster("temp.Spain.jul07.tif")

media_temp_esp_julio



##' Descarga datos para el lince ibérico ("Lynx pardinus"), con el paquete rgbif.

  # occ_search() es una función para buscar si en gbif se han encontrado registros.

  # Buscamos el lince ibérico ("Lynx pardinus").

occ_search(scientificName = "Lynx pardinus")

  # occ_data() es una función que obtiene los datos de las observaciones.
    # Como se han encontrado 2009 registros, ponemos de límite 2009, 
    # si pusiéramos un límite más alto no serviría de nada pues solo hay 
    # en la base de datos 2009 registros.
  
l_pardinus <- occ_data(scientificName = "Lynx pardinus",
                    limit = 2009)



## 2. Manipulación de datos ##
############################### ##


##' Crea un objeto con los datos de temperaturas máximas de España del 
##' mes de enero de 1981.
##'  Repite para el mes de julio de 1981. 
##'  Pista: tendras que recortar y enmascarar los datos de E-OBS.

  # Con la función time obtenemos un vector con el nombre de las capas.
    # Nos sirve para ver la posición en la que están las fechas.

fechas_datos_80_94 <- time(datos80_94)

   # Nos aseguramos que la variable es tipo fecha.

fechas_datos_80_94 <- as.Date(fechas_datos_80_94)

  # Hacemos un objeto  que obtenga las posiciones de las fechas de enero del
    # año 1981 y otro para todas las posiciones de julio de 1981.

    # Utilizamos la función format() con la que podemos elegir el día, mes,
      # año o calendario juliano, pero solo lo utilizaremos para mes y año.

    # Para ello usamos el operador lógico & que requiere de que dos condiciones
      # sean verdaderas para elegir el valor.

fechas_datos_81_enero <- which(format(fechas_datos_80_94, "%m") == "01" &
                               
                               format(fechas_datos_80_94, "%Y") == "1981")


fechas_datos_81_julio <- which(format(fechas_datos_80_94, "%m") == "07" &
                                 
                                 format(fechas_datos_80_94, "%Y") == "1981")

  # Hacemos 2 objetos, mediante un subset y utilizamos las posiciones que hemos 
    # obtenido de las fechas para quedarnos solo con los datos que nos interesan.

temp_max_81_enero <- subset(datos80_94, fechas_datos_81_enero)

temp_max_81_julio <- subset(datos80_94, fechas_datos_81_julio)

plot(temp_max_81_enero)

  # Tenemos mapas con datos de la temperatura de toda Europa, queremos quedarnos
    # solo con España.
    
    # Utilizamos la función crop() para recortar y quedarnos solo con España

temp_max_81_enero_spain <- crop(temp_max_81_enero, ext(-10, 4, 35, 44))

plot(temp_max_81_enero_spain)

temp_max_81_julio_spain <- crop(temp_max_81_julio, ext(-10, 4, 35, 44))

plot(temp_max_81_julio_spain)

  # Generamos un mapa de España para enmascarar.

mapamundo <- world (resolution = 2, level = 0, 
                      path = "C:\\Users\\julia\\OneDrive\\Documentos\\GitHub\\DataScienceUAH\\hackathon2025\\juan_julian", 
                    version = "latest")

  # Usamos la columna NAME_0 para quedarnos solo con España.

mapa_spain <- mapamundo[mapamundo$NAME_0 == "Spain"]

plot(mapa_spain)

  # Quitamos las Canarias porque no hay linces allí y ajustamos la extensión
    # para que quede centrado.

mapa_spain <- crop(mapa_spain, ext (-10, 4, 35, 44))

plot(mapa_spain)


  # Enmascaramos el mapa con las temperaturas de enero y julio.

temp_max_81_enero_spain_mask <- mask(temp_max_81_enero_spain, mapa_spain)

plot(temp_max_81_enero_spain_mask)

temp_max_81_julio_spain_mask <- mask(temp_max_81_julio_spain, mapa_spain)

plot(temp_max_81_julio_spain_mask)



##' Crea un subset con los datos de ocurrencia del lince, solo para España.

  # Usamos la función str() para ver la estructura de l_pardinus.

    # Como hay un exceso de información, utilizamos el símbolo $ para ver 
      # las variables que tiene y buscamos la variable país (country).
      # Con la función table podemos ver el nombre de la variable country y 
      # podemos buscar España.

str(l_pardinus)

table(l_pardinus$data$country)

  # Creamos un subset solo con los datos de España.

l_pardinus_data <- l_pardinus$data

l_pardinus_spain <- subset(l_pardinus_data, country == "Spain")

  # Podemos comprobar que en la tabla obtenida con la función table() nos 
    # muestra que hay 1113 observaciones de España y en el objeto l_pardinus_spain
    # que es el objeto en el que hemos guardado los avistamientos en España 
    # nos indica que efectivamente se han seleccionado 1113 observaciones.



##' Extrae los datos de temperaturas mensuales de junio y enero para 
##' cada ocurrencia del lince en España. Repite para las temperaturas máximas de los
##'  días del mes de enero y del mes de julio de 1981

  # Queremos obtener las coordenadas de los avistamientos de los linces en España
    
    # Para ello obtenemos un dataframe con los valores de longitud y latitud.

coords_lince <- data.frame (x = l_pardinus_spain$decimalLongitude,
                            y = l_pardinus_spain$decimalLatitude)

  # Nos aseguramos de que no tenga NAs.

coords_lince <- na.omit(coords_lince)
  
  # Extraemos los datos de las temperaturas mensuales y máximas de enero y junio para
    # cada ocurrencia del lince

  # Utilizaremos la función extract()

temp_mean_enero_lince <- extract(media_temp_esp_enero, coords_lince)

temp_mean_julio_lince <- extract(media_temp_esp_julio, coords_lince)

temp_max_enero_lince <- extract(temp_max_81_enero_spain_mask, coords_lince)

temp_max_junio_lince <- extract(temp_max_81_julio_spain_mask, coords_lince)



##' Extrae las ocurrencias del lince que tienen lugar dentro de los 
##' Parques Nacionales de España.

  # Observamos con la función "str()" que en la variable figura_lp 
    # aparece la categoría de protección (estamos buscando Parque Nacional)

str(datos_parq_nac)

  # Hacemos un subset para quedarnos solo con la categoría de Parque Nacional.

parques_nacionales <- subset(datos_parq_nac, figura_lp == "Parque Nacional")

plot(parques_nacionales)

  # Adecuamos el sistema de coordenadas de los Parques Nacionales transformándolo
    # a objeto st y utilizando la función "crs()"

coords_parques <- st_transform(datos_parq_nac, crs(datos_parq_nac))

# Para poder utilizar 2 archivos diferentes usando sistemas de coordenadas 
  # los sistemas de coordenadas utilizados deben ser los mismos.

  # Hay que mirar qué sistema de coordenadas utiliza cada archivo
    # Para ello usaremos la función str

str(coords_parques)
str(coords_lince)    

  # La función str no nos indica qué sistema de coordenadas tienen los Parques Naturales
    # pero sí que nos indica que el sistema de coordenadas de los avistamientos
    # de lince tiene un sistema de coordenadas basado en un eje "x" y un eje "y".

  # Para obtener el sistema de coordenadas de los parques necesitamos la
    # la función st_crs(), que nos devuelve información sobre el sistema de 
    # referencia de coordenadas que tienen unos datos georreferenciados.
  
st_crs(coords_parques)  

  # La función st_crs nos indica que el sistema de coordenadas que utilizan 
    # los Parques Naturales es el 4258.

  # Al tener sistemas de coordenadas diferentes, es necesario transformar uno 
    # de ellos para que pasen a tener el mismo sistema de coordenadas.

    # Cambiaremos el de los linces ya que no tienen un sistema georreferenciado.
      # Para ello utilizaremos la función st_as_sf(), transformando coords_lince
      # en un objeto sf (como lo es coords_parques)

coords_lince <- st_as_sf(coords_lince, coords = c("x", "y"), crs = 4258)

  # Extraemos los datos de ambos para observar dónde coinciden mediante la función
    # st_intersection (análoga a extract).

lince_parques <- st_intersection(coords_lince, coords_parques)



##' Calcula un mapa raster con la media de las temperaturas máximas 
##' de enero de 1981
##' y de julio de 1981

  # Usamos la función "app()" para realizar operaciones en un archivo raster.

temp_media_81_enero <- app(temp_max_81_enero_spain_mask, mean)

plot(temp_media_81_enero)

temp_media_81_julio <- app(temp_max_81_julio_spain_mask, mean)

plot(temp_media_81_julio)



## Transforma los mapas con esas medias correspondientes a 1981 a la resolución, extent
##'   y proyección utilizada por los mapas mensuales que descargaste de github.

temp_media_81_enero
temp_media_81_julio

  # Tienen una resolución de 0.25, se extiende desde 35º a 44º norte y 
    # -10º a 4º de latitud y unas coordenadas de referencia en WGS 84.

media_temp_esp_enero
media_temp_esp_julio

  # Tienen una resolución de 0,1, se extiende desde 39,95º a 43,85º norte y 
    # -9,35º a 4,45º norte y unas coordenadas de referencia en WGS 84.

  # Para ajustar la resolución usamos la función "resample()", que toma las
    # características de uno y las implementa en el otro.

  # Para ello, ambos deben estar en el mismo formato.
    # La función "rast()" convierte a SpatRaster.

media_temp_esp_enero_spat <- rast(media_temp_esp_enero)  

temp_media_81_enero_reescalada <- resample(temp_media_81_enero,
                                           media_temp_esp_enero_spat, method = "bilinear")

media_temp_esp_julio_spat <- rast(media_temp_esp_julio)

temp_media_81_julio_reescalada <- resample(temp_media_81_julio,
                                           media_temp_esp_julio_spat, method = "bilinear")

  # Si ejecutamos los objetos observamos que efectivamente la resolución y 
    # la extensión han cambiado (el sistema de coordenadas era ya igual)

  # Elaboramos el mapa con la función "plot()".

plot(temp_media_81_enero_reescalada)

plot(temp_media_81_julio_reescalada)



## 3. Análisis: Realiza los análisis necesarios para resolver los siguientes problemas ##
###################################################################################################### ##

##' 3.a. Compara (p.ej. correlaciones, diagrama de dispersión) las temperaturas
##' experimentadas por el lince dentro del Parque Nacional de Doñana en enero y julio 
##' de 1981. 

  # Algunos gráficos que se podrían hacer serían:
    # Un diagrama de dispersión en el que para cada observación haya 2 puntos,
     # uno para junio y otro para enero.
   # Un gráfico también de dispersión en el que la variable x sea las temperaturas
     # de enero y la y las de julio. Dependiendo de la posición y de la
     # pendiente de la línea de tendencia podríamos determinar algún patrón.
    # Un gráfico de boxplot con las temperaturas de enero y otro con las de julio
     # en la misma escala para comparar ambos meses.



  # Elaboramos un subset con los datos de Doñana usando la función "subset()".

coords_don <- subset(coords_parques, 
                             coords_parques$sitename == "Doñana")

  # Usamos únicamente los avistamientos de linces ocurridos en Doñana.
    #Utilizamos la función "st_intersection()"

coords_lince_don<- st_intersection(coords_lince, coords_don)

  # Obtenemos las temperaturas de enero y de junio para cada avistamiento de 1981.
    # Utilizamos la función "extract()".

temp_avist_don_enero <- extract(temp_max_81_enero_spain_mask,coords_lince_don)

temp_avist_don_julio <- extract(temp_max_81_julio_spain_mask,coords_lince_don)

  # Usamos la función "colmeans()" para obtener la media de cada día.

temp_avist_don_enero_media <- colMeans(temp_avist_don_enero, na.rm = TRUE)

temp_avist_don_julio_media <- colMeans(temp_avist_don_julio, na.rm = TRUE)

  # Elaboramos un dataframe con las medias calculadas.

dataframe_temp_avist_81_don_media <- data.frame(t_enero = temp_avist_don_enero_media,
                                                t_julio = temp_avist_don_julio_media,
                                                dia = c(1:32))

rownames(dataframe_temp_avist_81_don_media)

dataframe_temp_avist_81_don_media <- dataframe_temp_avist_81_don_media[-1,]
  # Elaboramos un mapa de dispersión para ver cómo se relacionan ambas temperaturas.

ggplot(dataframe_temp_avist_81_don_media)+
  
  geom_point(aes(x = dia, y = t_enero), col = "turquoise") +
  geom_line(aes(x = dia, y = t_enero), col = "turquoise") +
  
  geom_point(aes(x = dia, y = t_julio), col = "darkred") +
  geom_line(aes(x = dia, y = t_julio), col = "darkred") 

  # Podemos observar que las temperaturas en julio son mayores por motivos obvios
    # y también que no siguen un mismo patrón.

library(tidyr)
dataframe_temp_avist_81_don_media_long <- pivot_longer(dataframe_temp_avist_81_don_media, cols = c(t_enero, t_julio))

ggplot(dataframe_temp_avist_81_don_media_long, aes (x = as.character(name), y = value, col = name)) +
  geom_boxplot()

  # Con el boxplot se confirma lo que se había visto en el gráfico de dispersión
    # que es que las temperaturas de julio son más altas que las de enero.


  # Podemos observar que las temperaturas en julio son mayores por motivos obvios
    # y también que no siguen un mismo patrón.

##' 3.b. Haz un mapa que compare la media de las temperaturas máximas del mes de 
##' enero de 1981 con las temperaturas medias del mes de enero. ¿en qué parte de 
##' España las diferencias entre máximas del 81 y medias son más pequeñas? ¿Dónde
##' son más grandes?   
##' Pista: usa operaciones aritméticas

  # Para comparar las temperaturas podemos restar a las del año 1981 las 
    # temperaturas medias.

comparacion_enero <- temp_media_81_enero_reescalada - media_temp_esp_enero_spat

plot(comparacion_enero)

comparacion_julio <- temp_media_81_julio_reescalada - media_temp_esp_julio_spat

plot(comparacion_julio)

  # Todos los que estén por encima de 0ºC habrán experimentado un mes (tanto de
    # enero como de junio dependiendo del mapa que se trate) más cálido que 
    # la media, mientras que si se halla por debajo de 0ºC habrá experimentado
    # un mes más frío que la media.

  # En enero las diferencias más grandes con respecto a la media se dividen en 
    # 2 grupos, los que tienen temperaturas más altas que la media (algunos puntos
    # de Extremadura, Navarra, Valencia, Albacete y Murcia) y los que tienen 
    # temperaturas inferiores a la media como lo es la provincia de Gerona y 
    # algunos puntos en la Cordillera Bética.

##' 3.c. ¿En qué parque nacional fueron mayores las diferencias entre la media de las
##' temperaturas máximas de julio de 1981 y las temperaturas medias de enero?   

par(mfrow = (c(1,2)))

plot(comparacion_enero)

plot(comparacion_julio)

dev.off()
  # Poniendo los gráficos uno al lado del otro parece que enero ha sido más 
    # caluroso respecto a la media, pero al no poseer las mismas escalas, no
    # podemos aclararlo con este método.

comparación_meses <- plot(comparacion_enero - comparacion_julio)

  # Podemos observar de esta forma que hay lugares donde claramente enero fue 
    # más caluroso respecto a su media como el centro norte de la península, 
    # pero otros sitios donde julio fue más caluroso respecto a la media, 
    # como por ejemplo Galicia o Cataluña.

  # Para comprobar realmente cuál fue el mes más caluroso respecto a su media
    # habría que hacer una media de las observaciones de los días de cada mes, 
    # una media con todos los días y a eso restarle la media de temperatura 
    # mensual que nos hemos descargado de Git.

  # (por desgracia, no he conseguido realizar el primer paso por lo que me 
    # ha resultado imposible continuar con el resto).


  # Tras darle muchas vueltas creo que he llegado a algo, aún así, dejo lo 
    # anterior para que se vea en las posibilidades en las que había pensado)
  # Otra opción sería hacer una media con todos los valores de temperatura
    # de cada mes y compararla con la media de Git (mediante una resta como antes)

media_julio <- global(comparacion_enero, fun = mean, na.rm = TRUE)

media_julio <- global(comparacion_julio, fun = mean, na.rm = TRUE)

plot_meses <- data.frame(id = c("dif_enero", "dif_julio"),
                         value = c(-0.02219256, -1.333526))


ggplot(data = plot_meses, aes(x = id, y = value)) +
  geom_bar(stat = "identity")

  # Se puede ver claramente cómo la diferencia entre julio y su media fue mucho 
    # mayor que la diferencia de enerco con su media, siendo las dos negativas,
    # lo que significa que fueron meses más fríos que la media.

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



