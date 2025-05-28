#PROYECTO DE INES CORRECHER Y ARIADNA SÁNCHEZ####

# Nuestra primera idea es tratar los incendios ya que es una problemática medio ambiental muy presente en los
# últimos años. Comenzamos 

# Nos surgieron varias ideas, evolución de los incendios de los últimos year en Europa, en España, en las diferentes CCAA de España
# como podría esta afectando las condiciones climatológicas de cada zona a que hubiera incendios, la existencia de siniestros (incendios 
# que ocupan menos de 1 ha)...
# - Evolución de los incendios forestales en las CCAA de España desde 2007 hasta 2023
# - Evolución de los incendios en España

### evolución y comparación en los siniestros 



#Pasar de excel a R####
library(readxl)

siniestro_database_2007 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "3", range = "B6:H27", na = "-")
siniestro_database_2008 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "4", range = "B6:G27", na = "-")
siniestro_database_2009 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "5", range = "B6:G27", na = "-")
siniestro_database_2010 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "6", range = "B6:G27", na = "-")
siniestro_database_2011 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "7", range = "B6:G27", na = "-")
siniestro_database_2012 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "8", range = "B6:G27", na = "-")
siniestro_database_2013 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "9", range = "B6:G27", na = "-")
siniestro_database_2014 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "10", range = "B6:G27", na = "-")
siniestro_database_2015 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "11", range = "B6:G27", na = "-")
siniestro_database_2016 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "12", range = "B6:G27", na = "-")
siniestro_database_2017 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "13", range = "B6:G27", na = "-")
siniestro_database_2018 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "14", range = "B6:G27", na = "-")
siniestro_database_2019 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "15", range = "B6:G27", na = "-")
siniestro_database_2020 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "16", range = "B6:G27", na = "-")
siniestro_database_2021 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "17", range = "B6:G27", na = "-")
siniestro_database_2022 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "18", range = "B6:G27", na = "-")
siniestro_database_2023 <- read_excel("Incendios_database_GAR.xlsx", 
                                      sheet = "19", range = "B6:G27", na = "-")

####Datos prevencion de incendios
#prevencion <- read.csv(file = "prevencion-de-incendios.csv", header = TRUE, sep = ";")

library(tidyverse)
library(dplyr)

siniestro_database_2007_nrow <- siniestro_database_2007 %>% 
  select(!Reproducciones)
#Este es especifico del 2007, porque tenia la columna esa extra, entonces la he borrado primero

#Este hay que hacerlo en todos. 
siniestro_database_2007_tidy <- siniestro_database_2007_nrow %>%  
  filter(!row_number() %in% (1)) %>% 
  #Elimino la primera fila porque esta vacia
  rename(ccaa = 1) %>% 
  #Le cambio el nombre a la primera columna, porque el nombre "1" no es intuitivo
  replace(is.na(.), 0) %>% 
  #Cambio todos los NA por 0 para poder calcular con los siniestros, ademas, se cambia de ch a dbl
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...7") %>% 
  #cambios de nombre para facilitar procesado de datos en el futuro
  mutate(year = 2007)
#Creo una columna con el año para poder separar los distintos años en la df final
rm(siniestro_database_2007_nrow)
rm(siniestro_database_2007)

#Hora de crear las otras tablas :p
siniestro_database_2008_tidy <- siniestro_database_2008 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>%   mutate(year = 2008)
rm(siniestro_database_2008)

siniestro_database_2009_tidy <- siniestro_database_2009 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2009)
rm(siniestro_database_2009)

siniestro_database_2010_tidy <- siniestro_database_2010 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2010)
rm(siniestro_database_2010)

siniestro_database_2011_tidy <- siniestro_database_2011 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2011)
rm(siniestro_database_2011)

siniestro_database_2012_tidy <- siniestro_database_2012 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2012)
rm(siniestro_database_2012)

siniestro_database_2013_tidy <- siniestro_database_2013 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2013)
rm(siniestro_database_2013)

siniestro_database_2014_tidy <- siniestro_database_2014 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2014)
rm(siniestro_database_2014)

siniestro_database_2015_tidy <- siniestro_database_2015 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>%  
  mutate(year = 2015)
rm(siniestro_database_2015)

siniestro_database_2016_tidy <- siniestro_database_2016 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2016)
rm(siniestro_database_2016)

siniestro_database_2017_tidy <- siniestro_database_2017 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>%  
  mutate(year = 2017)
rm(siniestro_database_2017)

siniestro_database_2018_tidy <- siniestro_database_2018 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2018)
rm(siniestro_database_2018)

siniestro_database_2019_tidy <- siniestro_database_2019 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>%  
  mutate(year = 2019)
rm(siniestro_database_2019)

siniestro_database_2020_tidy <- siniestro_database_2020 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>%  
  mutate(year = 2020)
rm(siniestro_database_2020)

siniestro_database_2021_tidy <- siniestro_database_2021 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>% 
  mutate(year = 2021)
rm(siniestro_database_2021)

siniestro_database_2022_tidy <- siniestro_database_2022 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>%  
  mutate(year = 2022)
rm(siniestro_database_2022)

siniestro_database_2023_tidy <- siniestro_database_2023 %>%  
  filter(!row_number() %in% (1)) %>% 
  rename(ccaa = 1) %>% 
  replace(is.na(.), 0) %>% 
  rename(siniestros = "Siniestros que llegan a incendios (%)", incendios = "Incendios(2)", conatos = "Conatos(1)", suma = "Total...2", ha_forestal = "Total...6") %>%  
  mutate(year = 2023)
rm(siniestro_database_2023)

#Ahora voy a unir todas las tablas en una sola


list_siniestro = list(siniestro_database_2007_tidy, siniestro_database_2008_tidy, siniestro_database_2009_tidy, siniestro_database_2010_tidy, siniestro_database_2011_tidy,
                      siniestro_database_2012_tidy, siniestro_database_2013_tidy, siniestro_database_2014_tidy, siniestro_database_2015_tidy, siniestro_database_2016_tidy,
                      siniestro_database_2017_tidy, siniestro_database_2018_tidy, siniestro_database_2019_tidy, siniestro_database_2020_tidy, siniestro_database_2021_tidy,
                      siniestro_database_2022_tidy,siniestro_database_2023_tidy) 
df_siniestro_year_dbl <- list_siniestro %>% 
  bind_rows() 

#Primero, he creado una lista con todas las df
#Luego, he usado el comando bind_rows para unir todas las df de la lista por fila. Esto quiere decir que se me quedan 6 columnas
#en la df, pero se me acoplan todas las filas en una sola df.

df_siniestro <- df_siniestro_year_dbl %>% 
  mutate (year = as.integer(year))

rm(df_siniestro_year_dbl, list_siniestro)        
#He pasado los años a int por evitar errores. He borrado la columna año, que esta en dbl


#Me he dado cuenta de unos errores en el excel, en los que algunos datos de siniestros de ceuta que deberian ser 100 son 0. (Filas 239, 259 y 319)


df_siniestro <- df_siniestro %>% 
  mutate(suma*100/ha_forestal)  %>% 
  rename(siniestro_ha = "suma * 100/ha_forestal")

#Creamos una columna de siniestros por hectarea forestal

#La parte entre [] es la condicion, y la <- lo sustituye
#Sinceramente, es un poco chapuzas, porque no esta realmente automatizado, pero me di cuenta de que solo eran 3 filas y que todas
#tenian los mismos valores de conatos, incendios y total por lo que he hecho unas pequeñas trampas.

df_siniestro1 <- df_siniestro %>% 
  filter(ccaa != c("Ceuta"))

df_siniestro <- df_siniestro1 %>% 
  filter(ccaa != c("Melilla"))

df_siniestro <- df_siniestro %>% 
  filter(ccaa != c("Total", "Canarias"))

#Hemos eliminado canarias porque en los datos climáticos no aparecía

#Eliminamos los valores de Ceuta y Melilla porque no son representativos y creamos una versión con el total 
#de las ccaa y otra sin para hacer posibles representaciones graficas.

write.csv(df_siniestro, "df_siniestro.csv")
write.csv(df_siniestro, "df_siniestro_Total.csv")

#Al fin tenemos el .csv de todos los datos tidy y en una sola df!!!!!



#Esteticas del proyecto####

ccaa_palette <- c("chartreuse1", "#0000FF", "darkorange", "deeppink2", "red",
                  "#ADD8E6", "magenta4", "darkolivegreen", "cornsilk3", "yellow",
                  "#D8BFD8", "#A0522D", "purple1", "#2F4F4F", "mediumturquoise",
                  "#FFFAF0", "dimgray")

paint <- scale_fill_manual(values=Barbie_palette)

#Proyecto####

rm(list=ls())
options(stringsAsFactors = FALSE)

library(terra)
library(mapSpain)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Introducimos bases de datos

df_siniestros <- read.csv("df_siniestros.csv")

df_prep <- read.csv("df_prep.csv")

df_temp <- read.csv("df_temp.csv")

df_tmax <- read.csv("df_tmax.csv")

df_siniestros_total <- read.csv("df_siniestro_Total.csv")

##Temperatura####

#ayuda chatgpt

# Gracias a esta información pudimos comenzar a analizar nuestros datos de clima
#al ser un trabajo repetitivo vamos a intentar hacer un loop
# Primero extraemos los primeros datos de nuestro periodo

###Tramo de 2007 hasta 2010####

tmedia1 <- rast("Tmedia1.nc")
tmedia1

años <- 2007:2010

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
tmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  tmedia_por_año[[as.character(años[i])]] <- subset(tmedia1, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)


listamapas1 <- list()
listadatos1 <- list()
# Seleccionar año; aqui debemos hacer nuestro loop para conseguir automatizar este proceso de obtención de datos 



for(i in 1:length(tmedia_por_año)){
  
  tmedia_200i <- tmedia_por_año[[i]]
  
  # Pasar a formato date
  fechas <- as.Date(time(tmedia_200i))
  
  # ahora queremos hacer las temperaturas medias por comunidad autonoma
  tmedia_ccaa <- terra::extract(tmedia_200i, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = tmedia_ccaa, mean, na.rm=T)
  medias_ccaa <- rowMeans(medias_ccaa_dia[,2:366])
  #ccaa_medias = data.frame(tempmediaccaa = medias_ccaa, ID = medias_ccaa_dia$ID)
  
  # columna que se llame año 2007; formato tidy
  
  medias_200i_ccaa <- data.frame (nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
                                  temp = as.numeric(medias_ccaa))#paste0 va a poner 0 como un caracter paraque aparezca 01, 02...
  
  medias_200i_ccaa$year <- as.numeric(names(tmedia_por_año)[i])
  
  #ahora le incluimos las dos ciudades autonómicas
  #esto lo podríamos omitir en el proyecto ya que nosotras quitamos estas dos ciudades 
  
  
  CCAAtemps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  #CCAAtemps_sf
  #str(medias_julio_ccaa)
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Ceuta"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Melilla"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    rename(ccaa = nuts2.name)
  
  listadatos1[[i]] <- medias_200i_ccaa
  
  #ahora haremos un pama ggplot
  listamapas1[[i]] <- ggplot(CCAAtemps_sf)+
    geom_sf(aes(fill = temp),
            color = "grey70",
            lwd = .3
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, "Reds", rev = TRUE),
      n.breaks = 10,
      guide = guide_legend(title = "temp")
    ) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),        # Mueve la leyenda a la esquina inferior derecha
      legend.justification = c(1, 0)
    )
  
  
  
  
}

listamapas1[[1]]
listadatos1[[2]]



###Tramo de 2011 hasta 2023####

tmedia2 <- rast("Tmedia2.nc")
tmedia2

años <- 2011:2023

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
tmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  tmedia_por_año[[as.character(años[i])]] <- subset(tmedia2, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)


listamapas2 <- list()
listadatos2 <- list()
# Seleccionar año; aqui debemos hacer nuestro loop para conseguir automatizar este proceso de obtención de datos 



for(i in 1:length(tmedia_por_año)){
  
  tmedia_200i <- tmedia_por_año[[i]]
  
  # Pasar a formato date
  fechas <- as.Date(time(tmedia_200i))
  
  # ahora queremos hacer las temperaturas medias por comunidad autonoma
  tmedia_ccaa <- extract(tmedia_200i, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = tmedia_ccaa, mean, na.rm=T)
  medias_ccaa <- rowMeans(medias_ccaa_dia[,2:366])
  # columna que se llame año 2007; formato tidy
  
  medias_200i_ccaa <- data.frame (nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
                                  temp = as.numeric(medias_ccaa))#paste0 va a poner 0 como un caracter paraque aparezca 01, 02...
  
  medias_200i_ccaa$year <- as.numeric(names(tmedia_por_año)[i])
  
  #ahora le incluimos las dos ciudades autonómicas
  #esto lo podríamos omitir en el proyecto ya que nosotras quitamos estas dos ciudades 
  
  
  CCAAtemps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  #CCAAtemps_sf
  #str(medias_julio_ccaa)
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Ceuta"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Melilla"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    rename(ccaa = nuts2.name)
  
  listadatos2[[i]] <- medias_200i_ccaa
  
  #ahora haremos un pama ggplot
  listamapas2[[i]] <- ggplot(CCAAtemps_sf)+
    geom_sf(aes(fill = temp),
            color = "grey70",
            lwd = .3
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, "Reds", rev = TRUE),
      n.breaks = 10,
      guide = guide_legend(title = "temp")
    ) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),        # Mueve la leyenda a la esquina inferior derecha
      legend.justification = c(1, 0)
    )
  
  
  
  
}

listamapas2[[1]]
listadatos2[[1]]

##Precipitacion con las medias diarias####

###Tramo de 2007 hasta 2010####

pmedia1 <- rast("Pmedia1.nc")
pmedia1

años <- 2007:2010

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
pmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  pmedia_por_año[[as.character(años[i])]] <- subset(pmedia1, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)


listamapas3 <- list()
listadatos3 <- list()
# Seleccionar año; aqui debemos hacer nuestro loop para conseguir automatizar este proceso de obtención de datos 



for(i in 1:length(pmedia_por_año)){
  
  pmedia_200i <- pmedia_por_año[[i]]
  
  # Pasar a formato date
  fechas <- as.Date(time(pmedia_200i))
  
  # ahora queremos hacer las temperaturas medias por comunidad autonoma
  pmedia_ccaa <- terra::extract(pmedia_200i, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = pmedia_ccaa, mean, na.rm=T)
  medias_ccaa <- rowMeans(medias_ccaa_dia[,2:366])
  # columna que se llame año 2007; formato tidy
  
  medias_200i_ccaa <- data.frame (nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
                                  prep = as.numeric(medias_ccaa))#paste0 va a poner 0 como un caracter paraque aparezca 01, 02...
  
  medias_200i_ccaa$year <- as.numeric(names(pmedia_por_año)[i])
  
  #ahora le incluimos las dos ciudades autonómicas
  #esto lo podríamos omitir en el proyecto ya que nosotras quitamos estas dos ciudades 
  
  
  CCAApreps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  #CCAAtemps_sf
  #str(medias_julio_ccaa)
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Ceuta"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Melilla"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    rename(ccaa = nuts2.name)
  
  listadatos3[[i]] <- medias_200i_ccaa
  
  #ahora haremos un pama ggplot
  listamapas3[[i]] <- ggplot(CCAApreps_sf)+
    geom_sf(aes(fill = prep),
            color = "grey70",
            lwd = .3
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, "Blues", rev = TRUE),
      n.breaks = 10,
      guide = guide_legend(title = "prep")
    ) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),        # Mueve la leyenda a la esquina inferior derecha
      legend.justification = c(1, 0)
    )
  
  
  
  print(i)  
}

listamapas3[[1]]
listadatos3[[2]]



###Tramo de 2011 hasta 2023####

pmedia2 <- rast("Pmedia2.nc")
pmedia2

años <- 2011:2023

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
pmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  pmedia_por_año[[as.character(años[i])]] <- subset(pmedia2, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)


listamapas4 <- list()
listadatos4 <- list()
# Seleccionar año; aqui debemos hacer nuestro loop para conseguir automatizar este proceso de obtención de datos 



for(i in 1:length(pmedia_por_año)){
  
  pmedia_200i <- pmedia_por_año[[i]]
  
  # Pasar a formato date
  fechas <- as.Date(time(pmedia_200i))
  
  # ahora queremos hacer las temperaturas medias por comunidad autonoma
  pmedia_ccaa <- terra::extract(pmedia_200i, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = pmedia_ccaa, mean, na.rm=T)
  medias_ccaa <- rowMeans(medias_ccaa_dia[,2:366])
  # columna que se llame año 2007; formato tidy
  
  medias_200i_ccaa <- data.frame (nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
                                  prep = as.numeric(medias_ccaa))#paste0 va a poner 0 como un caracter paraque aparezca 01, 02...
  
  medias_200i_ccaa$year <- as.numeric(names(pmedia_por_año)[i])
  
  #ahora le incluimos las dos ciudades autonómicas
  #esto lo podríamos omitir en el proyecto ya que nosotras quitamos estas dos ciudades 
  
  
  CCAApreps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  #CCAAtemps_sf
  #str(medias_julio_ccaa)
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Ceuta"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Melilla"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    rename(ccaa = nuts2.name)
  
  listadatos4[[i]] <- medias_200i_ccaa
  
  #ahora haremos un pama ggplot
  listamapas4[[i]] <- ggplot(CCAApreps_sf)+
    geom_sf(aes(fill = prep),
            color = "grey70",
            lwd = .3
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, "Blues", rev = TRUE),
      n.breaks = 10,
      guide = guide_legend(title = "prep")
    ) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),        # Mueve la leyenda a la esquina inferior derecha
      legend.justification = c(1, 0)
    )
  
  
  
  
}

listamapas4[[1]]
listadatos4[[6]]




##Precipitacion con las medias anuales (ayuda de Cris)####

###Tramo de 2007 hasta 2010####

pmedia1 <- rast("Pmedia1.nc")
pmedia1

años <- 2007:2010

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
pmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  pmedia_por_año[[as.character(años[i])]] <- subset(pmedia1, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)


listamapas3_year <- list()
listadatos3_year <- list()
# Seleccionar año; aqui debemos hacer nuestro loop para conseguir automatizar este proceso de obtención de datos 



for(i in 1:length(pmedia_por_año)){
  
  pmedia_200i <- pmedia_por_año[[i]]
  
  # Pasar a formato date
  fechas <- as.Date(time(pmedia_200i))
  
  # ahora queremos hacer las temperaturas medias por comunidad autonoma
  pmedia_ccaa <- terra::extract(pmedia_200i, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = pmedia_ccaa, sum, na.rm=T)
  medias_ccaa <- rowMeans(medias_ccaa_dia[,2:366])
  # columna que se llame año 2007; formato tidy
  
  medias_200i_ccaa <- data.frame (nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
                                  prep = as.numeric(medias_ccaa))#paste0 va a poner 0 como un caracter paraque aparezca 01, 02...
  
  medias_200i_ccaa$year <- as.numeric(names(pmedia_por_año)[i])
  
  #ahora le incluimos las dos ciudades autonómicas
  #esto lo podríamos omitir en el proyecto ya que nosotras quitamos estas dos ciudades 
  
  
  CCAApreps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  #CCAAtemps_sf
  #str(medias_julio_ccaa)
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Ceuta"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Melilla"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    rename(ccaa = nuts2.name)
  
  listadatos3_year[[i]] <- medias_200i_ccaa
  
  #ahora haremos un pama ggplot
  listamapas3_year[[i]] <- ggplot(CCAApreps_sf)+
    geom_sf(aes(fill = prep),
            color = "grey70",
            lwd = .3
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, "Blues", rev = TRUE),
      n.breaks = 10,
      guide = guide_legend(title = "prep")
    ) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),        # Mueve la leyenda a la esquina inferior derecha
      legend.justification = c(1, 0)
    )
  
  
  
  print(i)  
}

listamapas3_year[[1]]
listadatos3_year[[2]]



###Tramo de 2011 hasta 2023####

pmedia2 <- rast("Pmedia2.nc")
pmedia2

años <- 2011:2023

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
pmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  pmedia_por_año[[as.character(años[i])]] <- subset(pmedia2, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)


listamapas4_year <- list()
listadatos4_year <- list()
# Seleccionar año; aqui debemos hacer nuestro loop para conseguir automatizar este proceso de obtención de datos 



for(i in 1:length(pmedia_por_año)){
  
  pmedia_200i <- pmedia_por_año[[i]]
  
  # Pasar a formato date
  fechas <- as.Date(time(pmedia_200i))
  
  # ahora queremos hacer las temperaturas medias por comunidad autonoma
  pmedia_ccaa <- terra::extract(pmedia_200i, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = pmedia_ccaa, sum, na.rm=T)
  medias_ccaa <- rowMeans(medias_ccaa_dia[,2:366])
  # columna que se llame año 2007; formato tidy
  
  medias_200i_ccaa <- data.frame (nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
                                  prep = as.numeric(medias_ccaa))#paste0 va a poner 0 como un caracter paraque aparezca 01, 02...
  
  medias_200i_ccaa$year <- as.numeric(names(pmedia_por_año)[i])
  
  #ahora le incluimos las dos ciudades autonómicas
  #esto lo podríamos omitir en el proyecto ya que nosotras quitamos estas dos ciudades 
  
  
  CCAApreps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  #CCAAtemps_sf
  #str(medias_julio_ccaa)
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Ceuta"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Ciudad Autónoma de Melilla"))
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    filter(nuts2.name != c("Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% 
    rename(ccaa = nuts2.name)
  
  listadatos4_year[[i]] <- medias_200i_ccaa
  
  #ahora haremos un pama ggplot
  listamapas4_year[[i]] <- ggplot(CCAApreps_sf)+
    geom_sf(aes(fill = prep),
            color = "grey70",
            lwd = .3
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, "Blues", rev = TRUE),
      n.breaks = 10,
      guide = guide_legend(title = "prep")
    ) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),        # Mueve la leyenda a la esquina inferior derecha
      legend.justification = c(1, 0)
    )
  
  
  
  
}

listamapas4_year[[1]]
listadatos4_year[[6]]



##Dataframes####
#Juntar los dos loops para tener desde el 2007 hasta el 2023
temp_ccaa_1 <- listadatos1 %>% 
  bind_rows() 

temp_ccaa_2 <- listadatos2 %>% 
  bind_rows() 

df_temp <- rbind(temp_ccaa_1,temp_ccaa_2)

#Juntar los dos loops para tener desde el 2007 hasta el 2023 diarios
prep_ccaa_1 <- listadatos3 %>% 
  bind_rows() 

prep_ccaa_2 <- listadatos4 %>% 
  bind_rows() 

df_prep <- rbind(prep_ccaa_1,prep_ccaa_2)

write.csv(df_temp, "df_temp.csv")
write.csv(df_prep, "df_prep.csv")

#Juntar los dos loops para tener desde el 2007 hasta el 2023 anuales
prep_ccaa_1_year <- listadatos3_year %>% 
  bind_rows() 

prep_ccaa_2_year <- listadatos4_year %>% 
  bind_rows() 

df_prep_year <- rbind(prep_ccaa_1_year,prep_ccaa_2_year)


write.csv(df_prep_year, "df_prep_year.csv")





#Temperaturas de verano#
write.csv(df_temp_summer, "temperaturas_medias_verano_CCAA_2007_2023.csv", row.names = FALSE)

df_tmedia_summer <- read.csv("temperaturas_medias_verano_CCAA_2007_2023.csv")

write.csv(df_tmax_summer, "tmax_verano_CCAA_2007_2023.csv", row.names = FALSE)

df_tmax_summer <- read.csv("tmax_verano_CCAA_2007_2023.csv")


#Separacion de los datos por ccaa####
comunidades <- unique(df_siniestros$ccaa)

listaprep <- list()
listatemp <- list()
listainc <- list()

for(i in comunidades){
  listainc[[i]] <- df_siniestros %>% filter(ccaa == i) 
  listaprep[[i]] <- df_prep %>% filter(ccaa == i)
  listatemp[[i]] <- df_temp %>% filter(ccaa == i)
}

listagraph <- list()

for(i in comunidades){
  df <- data.frame(year = as.numeric(listainc[[i]][["year"]]), 
                   sin = listainc[[i]][["siniestro_ha"]],
                   prep = listaprep[[i]][["prep"]],
                   temp = listatemp[[i]][["temp"]])
  
  df <- df %>% filter(year != c(2020))  
  
  
  df_long <- df %>%
    gather(key = "variable", value = "value", sin, prep, temp)
  
  listagraph[[i]] <- ggplot(df_long, aes(x = year, y = value)) + 
    geom_bar(stat = "identity", aes(fill = variable), alpha = 0.6) + 
    geom_line(aes(color = variable), linewidth = 1) +
    geom_point(aes(color = variable)) + 
    facet_wrap(~ variable, scales = "free_y") +
    xlab("Años") + 
    ylab("") + 
    labs(title = i) + 
    scale_fill_manual(name = "Variable", values = c("sin" = "darkgreen", "prep" = "blue", 
                                                    "temp" = "red")) + 
    scale_color_manual(name = "Variable", values = c("sin" = "darkgreen", "prep" = "blue", 
                                                     "temp" = "red"))
}

listagraph[[13]]




#EMPIEZAN CAMBIOS TRAS LA PRESENTACIÓN DE MITAD DE SEMESTRE####

#Separación por 3 zonas climas####
df_siniestros3 <- df_siniestros %>% 
  mutate(clima = c("rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul")
  )

df_prep3 <- df_prep_year %>% 
  mutate(clima = c("rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul")
  )

df_temp3 <- df_tmedia_summer %>% 
  mutate(clima = c("rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul")
  )

df_tmax3 <- df_tmax %>% 
  mutate(clima = c("rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul","rojo", "naranja", "azul", 
                   "naranja", "azul", "naranja", 
                   "naranja", "rojo", "naranja", 
                   "rojo", "azul", "naranja",
                   "naranja", "azul", "azul", 
                   "azul")
  )
#write.csv(df_prep, "df_prep.csv")
#write.csv(df_temp, "df_temp.csv")
#write.csv(df_siniestros, "df_siniestros.csv")
#write.csv(df_tmax, "df_tmax.csv")

climas <- unique(df_siniestros3$clima)

listaprep <- list()
listatemp <- list()
listainc <- list()

for(i in climas){
  listainc[[i]] <- df_siniestros3 %>% filter(clima == i) 
  listaprep[[i]] <- df_prep3 %>% filter(clima == i)
  listatemp[[i]] <- df_temp3 %>% filter(clima == i)
}

listagraph <- list()

for(i in climas){
  df <- data.frame(year = as.numeric(listainc[[i]][["year"]]), 
                   sin = listainc[[i]][["siniestro_ha"]],
                   prep = listaprep[[i]][["prep"]],
                   temp = listatemp[[i]][["temp"]])
  
  df <- df %>% filter(year != c(2020))  
  
  
  df_long <- df %>%
    gather(key = "variable", value = "value", sin, prep, temp)
  
  listagraph[[i]] <- ggplot(df_long, aes(x = year, y = value))+ 
    geom_line(aes(color = variable), linewidth = 1) +
    geom_point(aes(color = variable)) + 
    facet_wrap(~ variable, scales = "free_y") +
    xlab("Años") + 
    ylab("") + 
    labs(title = i) + 
    scale_fill_manual(name = "Variable", values = c("sin" = "darkgreen", "prep" = "blue", "temp" = "red")) + 
    scale_color_manual(name = "Variable", values = c("sin" = "darkgreen", "prep" = "blue", "temp" = "red"))
}

listagraph[[2]]



#Separación por 2 zonas climas####
library(dplyr)
library(ggplot2)

# 1. Crear el vector con los climas asociados a cada CCAA
climas_ccaa <- c(
  "Andalucía" = "naranja", 
  "Aragón" = "naranja", 
  "Asturias (Principado de)" = "azul", 
  "Cantabria" = "azul", 
  "Castilla y León" = "naranja", 
  "Castilla-La Mancha" = "naranja", 
  "Cataluña" = "naranja", 
  "Comunitat Valenciana" = "naranja", 
  "Extremadura" = "naranja", 
  "Galicia" = "azul", 
  "Madrid (Comunidad de)" = "naranja", 
  "Murcia (Región de)" = "naranja", 
  "Navarra (Comunidad Foral de)" = "azul", 
  "País Vasco" = "azul", 
  "Rioja (La)" = "azul", 
  "Balears (Illes)" = "naranja"
)

# 2. Añadir la columna clima directamente al dataframe
df_siniestros2 <- df_siniestros %>%
  mutate(clima = recode(ccaa, !!!climas_ccaa))
df_prep2 <- df_prep %>% 
  mutate(clima = recode(ccaa, !!!climas_ccaa))
df_temp2 <- df_temp %>% 
  mutate(clima = recode(ccaa, !!!climas_ccaa))
df_tmax2 <- df_tmax %>% 
  mutate(clima = recode(ccaa, !!!climas_ccaa))



#Sacar las temperaturas correspondientes al verano####
#aprovechando el código ya escrito para las temperaturas en el periodo elegido, se seleccionaran
#aquellas correspondientes a los meses de verano 

##Tramo de 2007 hasta 2010####

tmedia1 <- rast("Tmedia1.nc")
tmedia1

años <- 2007:2010

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
tmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  tmedia_por_año[[as.character(años[i])]] <- subset(tmedia1, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)


# Inicializar listas
listamapas1_summer <- list()
listadatos1_summer <- list()

for(i in 1:length(tmedia_por_año)){
  
  tmedia_200i <- tmedia_por_año[[i]]
  
  # Extraer fechas asociadas a cada capa
  fechas <- as.Date(time(tmedia_200i))
  
  # Filtrar fechas de verano (junio, julio, agosto)
  meses <- as.integer(format(fechas, "%m"))
  indices_summer <- which(meses %in% c(6, 7, 8))
  
  # Subconjunto del raster solo con capas de verano
  tmedia_summer <- tmedia_200i[[indices_summer]]
  
  # Extraer medias por comunidad autónoma
  tmedia_ccaa <- terra::extract(tmedia_summer, ccaa_vect)
  
  medias_ccaa_dia <- aggregate(.~ID, data = tmedia_ccaa, mean, na.rm=TRUE)
  medias_ccaa <- rowMeans(medias_ccaa_dia[, 2:ncol(medias_ccaa_dia)])
  
  medias_200i_ccaa <- data.frame(
    nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
    temp = as.numeric(medias_ccaa)
  )
  
  medias_200i_ccaa$year <- as.numeric(names(tmedia_por_año)[i])
  
  # Eliminar Ceuta, Melilla y Canarias
  medias_200i_ccaa <- medias_200i_ccaa %>%
    filter(!nuts2.name %in% c("Ciudad Autónoma de Ceuta", "Ciudad Autónoma de Melilla", "Canarias"))
  
  # Renombrar las CCAA
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>%
    rename(ccaa = nuts2.name)
  
  # Guardar datos en la lista
  listadatos1_summer[[i]] <- medias_200i_ccaa
  
  # Crear mapa y guardarlo
  CCAAtemps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  
  listamapas1_summer[[i]] <- ggplot(CCAAtemps_sf) +
    geom_sf(aes(fill = temp), color = "grey70", lwd = .3) +
    scale_fill_gradientn(colors = hcl.colors(10, "Reds", rev = TRUE),
                         n.breaks = 10,
                         guide = guide_legend(title = "temp")) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),
      legend.justification = c(1, 0)
    )
}

listamapas1_summer[[1]]
listadatos1_summer[[2]]

##Tramo de 2011 hasta 2023####

tmedia2 <- rast("Tmedia2.nc")
tmedia2

años <- 2011:2023

# Definir la duración de cada año (bisiesto o no)
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)

# Crear una lista donde almacenaremos los datos por año
tmedia_por_año <- list()

# Extraer datos año por año
inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1  # Determinar el rango de días
  tmedia_por_año[[as.character(años[i])]] <- subset(tmedia2, inicio:fin)
  inicio <- fin + 1  # Actualizar el inicio para el siguiente año
}

CCAA_sf <- esp_get_ccaa()
#extraemos las ccaa
ccaa_vect <- vect(CCAA_sf$geometry)

listamapas2_summer <- list()
listadatos2_summer <- list()

for(i in 1:length(tmedia_por_año)){
  
  tmedia_200i <- tmedia_por_año[[i]]
  
  # Fechas
  fechas <- as.Date(time(tmedia_200i))
  meses <- as.integer(format(fechas, "%m"))
  indices_summer <- which(meses %in% c(6, 7, 8))
  tmedia_summer <- tmedia_200i[[indices_summer]]
  
  # Extracción espacial
  tmedia_ccaa <- terra::extract(tmedia_summer, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = tmedia_ccaa, mean, na.rm=TRUE)
  medias_ccaa <- rowMeans(medias_ccaa_dia[, 2:ncol(medias_ccaa_dia)])
  
  medias_200i_ccaa <- data.frame(
    nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
    temp = as.numeric(medias_ccaa)
  )
  
  medias_200i_ccaa$year <- as.numeric(names(tmedia_por_año)[i])
  
  # Filtrar Ceuta, Melilla y Canarias
  medias_200i_ccaa <- medias_200i_ccaa %>%
    filter(!nuts2.name %in% c("Ciudad Autónoma de Ceuta", "Ciudad Autónoma de Melilla", "Canarias"))
  
  # Renombrar CCAA
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>%
    rename(ccaa = nuts2.name)
  
  listadatos2_summer[[i]] <- medias_200i_ccaa
  
  # Mapa
  CCAAtemps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  
  listamapas2_summer[[i]] <- ggplot(CCAAtemps_sf) +
    geom_sf(aes(fill = temp), color = "grey70", lwd = .3) +
    scale_fill_gradientn(colors = hcl.colors(10, "Reds", rev = TRUE),
                         n.breaks = 10,
                         guide = guide_legend(title = "temp")) +
    theme_void() +
    theme(
      legend.position = c(0.9, 0.1),
      legend.justification = c(1, 0)
    )
}

listamapas2_summer[[1]]
listadatos2_summer[[2]]

df_temp_summer <- do.call(rbind, c(listadatos1_summer, listadatos2_summer))
head(df_temp_summer)
str(df_temp_summer)

ggplot(df_temp_summer, aes(x = year, y = temp, group = ccaa, color = ccaa)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución de la temperatura media de verano por CCAA (2007–2023)",
    x = "Año",
    y = "Temp. media (°C)",
    color = "CCAA"
  ) +
  theme_minimal()

write.csv(df_temp_summer, "temperaturas_medias_verano_CCAA_2007_2023.csv", row.names = FALSE)

df_tmedia_summer <- read.csv("temperaturas_medias_verano_CCAA_2007_2023.csv")

#Sacar las temperaturas correspondientes al verano pero con las temperaturas máximas ####
##Tramo de 2007 hasta 2010####
tmax1 <- rast("Tmax1.nc")
tmax1

años <- 2007:2010
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)
tmax_por_año <- list()

inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1
  tmax_por_año[[as.character(años[i])]] <- subset(tmax1, inicio:fin)
  inicio <- fin + 1
}

CCAA_sf <- esp_get_ccaa()
ccaa_vect <- vect(CCAA_sf$geometry)

listamapas1_summer <- list()
listadatos1_summer <- list()

for(i in 1:length(tmax_por_año)){
  tmax_200i <- tmax_por_año[[i]]
  fechas <- as.Date(time(tmax_200i))
  meses <- as.integer(format(fechas, "%m"))
  indices_summer <- which(meses %in% c(6, 7, 8))
  tmax_summer <- tmax_200i[[indices_summer]]
  tmax_ccaa <- terra::extract(tmax_summer, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = tmax_ccaa, mean, na.rm=TRUE)
  medias_ccaa <- rowMeans(medias_ccaa_dia[, 2:ncol(medias_ccaa_dia)])
  
  medias_200i_ccaa <- data.frame(
    nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
    temp = as.numeric(medias_ccaa),
    year = as.numeric(names(tmax_por_año)[i])
  )
  
  medias_200i_ccaa <- medias_200i_ccaa %>%
    filter(!nuts2.name %in% c("Ciudad Autónoma de Ceuta", "Ciudad Autónoma de Melilla", "Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% rename(ccaa = nuts2.name)
  listadatos1_summer[[i]] <- medias_200i_ccaa
  
  CCAAtemps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  listamapas1_summer[[i]] <- ggplot(CCAAtemps_sf) +
    geom_sf(aes(fill = temp), color = "grey70", lwd = .3) +
    scale_fill_gradientn(colors = hcl.colors(10, "Reds", rev = TRUE),
                         n.breaks = 10,
                         guide = guide_legend(title = "temp")) +
    theme_void() +
    theme(legend.position = c(0.9, 0.1), legend.justification = c(1, 0))
}
##Tramo de 2011 hasta 2023####
tmax2 <- rast("Tmax2.nc")
tmax2

años <- 2011:2023
dias_por_año <- ifelse(años %% 4 == 0 & (años %% 100 != 0 | años %% 400 == 0), 366, 365)
tmax_por_año <- list()

inicio <- 1
for (i in seq_along(años)) {
  fin <- inicio + dias_por_año[i] - 1
  tmax_por_año[[as.character(años[i])]] <- subset(tmax2, inicio:fin)
  inicio <- fin + 1
}

listamapas2_summer <- list()
listadatos2_summer <- list()

for(i in 1:length(tmax_por_año)){
  tmax_200i <- tmax_por_año[[i]]
  fechas <- as.Date(time(tmax_200i))
  meses <- as.integer(format(fechas, "%m"))
  indices_summer <- which(meses %in% c(6, 7, 8))
  tmax_summer <- tmax_200i[[indices_summer]]
  tmax_ccaa <- terra::extract(tmax_summer, ccaa_vect)
  medias_ccaa_dia <- aggregate(.~ID, data = tmax_ccaa, mean, na.rm=TRUE)
  medias_ccaa <- rowMeans(medias_ccaa_dia[, 2:ncol(medias_ccaa_dia)])
  
  medias_200i_ccaa <- data.frame(
    nuts2.name = CCAA_sf$nuts2.name[medias_ccaa_dia$ID],
    temp = as.numeric(medias_ccaa),
    year = as.numeric(names(tmax_por_año)[i])
  )
  
  medias_200i_ccaa <- medias_200i_ccaa %>%
    filter(!nuts2.name %in% c("Ciudad Autónoma de Ceuta", "Ciudad Autónoma de Melilla", "Canarias"))
  
  medias_200i_ccaa[,1] <- c("Andalucía", "Aragón", "Asturias (Principado de)", "Balears (Illes)", "Cantabria", "Castilla y León",
                            "Castilla-La Mancha", "Cataluña", "Comunitat Valenciana", "Extremadura", "Galicia", "Madrid (Comunidad de)",
                            "Murcia (Región de)", "Navarra (Comunidad Foral de)", "País Vasco", "Rioja (La)")
  
  medias_200i_ccaa <- medias_200i_ccaa %>% rename(ccaa = nuts2.name)
  listadatos2_summer[[i]] <- medias_200i_ccaa
  
  CCAAtemps_sf <- merge(CCAA_sf, medias_200i_ccaa)
  listamapas2_summer[[i]] <- ggplot(CCAAtemps_sf) +
    geom_sf(aes(fill = temp), color = "grey70", lwd = .3) +
    scale_fill_gradientn(colors = hcl.colors(10, "Reds", rev = TRUE),
                         n.breaks = 10,
                         guide = guide_legend(title = "temp")) +
    theme_void() +
    theme(legend.position = c(0.9, 0.1), legend.justification = c(1, 0))
}
df_tmax_summer <- do.call(rbind, c(listadatos1_summer, listadatos2_summer))

ggplot(df_tmax_summer, aes(x = year, y = temp, group = ccaa, color = ccaa)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución de la temperatura máxima de verano por CCAA (2007–2023)",
    x = "Año",
    y = "Temp. máxima (°C)",
    color = "CCAA"
  ) +
  theme_minimal()

write.csv(df_tmax_summer, "tmax_verano_CCAA_2007_2023.csv", row.names = FALSE)
df_tmax_summer <- read.csv("tmax_verano_CCAA_2007_2023.csv")


#Analisis####
#Tmax y siniestros####

#Creamos datasets con solo las columnas que queremos
df_siniestros1 <- df_siniestros %>%
  select(ccaa, siniestros, year)
df_tmax1 <- df_tmedia_summer %>% 
  select(ccaa, temp, year)

# Unimos las df por ccaa y year
df_merged <- merge(df_tmax1, df_siniestros1, by = c("ccaa", "year"))

#Analizamos la correlación para cada ccaa mediante el método Pearson
df_cor <- df_merged %>%
  group_by(ccaa) %>%
  summarize(correlacion = cor(temp, siniestros, method = "pearson"))
df_cor

# + :relación de proporcionalidad directa.
# 0 :sin relación lineal.
# − :relación de proporcionalidad inversa.

modelo <- lm(siniestros ~ temp, data = df_merged)
summary(modelo)

#Lo que queremos representar
df_merged$pred <- modelo$fitted.values

#Modelo lineal para observar visualmente
ggplot(df_merged, aes(x = temp, y = siniestros)) +
  geom_point(alpha = 0.6) +  # puntos reales
  geom_line(aes(y = pred), color = "red", linewidth = 0.8) +  # líneas de residuos
  labs(title = "Modelo lineal: Temperatura vs Siniestros",
       x = "Temperatura (ºC)",
       y = "Siniestros")

#Prep y siniestros####

#Creamos datasets con solo las columnas que queremos
df_siniestros1 <- df_siniestros %>%
  select(ccaa, siniestros, year)
df_prep1 <- df_prep %>% 
  select(ccaa, prep, year)

# Unimos las df por ccaa y year
df_merged <- merge(df_prep1, df_siniestros1, by = c("ccaa", "year"))

#Analizamos la correlación para cada ccaa mediante el método Pearson
df_cor <- df_merged %>%
  group_by(ccaa) %>%
  summarize(correlacion = cor(prep, siniestros, method = "pearson"))
df_cor

# + :relación de proporcionalidad directa.
# 0 :sin relación lineal.
# − :relación de proporcionalidad inversa

modelo <- lm(siniestros ~ prep, data = df_merged)
summary(modelo)

#Lo que queremos representar
df_merged$pred <- modelo$fitted.values

#Modelo lineal para observar visualmente
ggplot(df_merged, aes(x = prep, y = siniestros)) +
  geom_point(alpha = 0.6) +  # puntos reales
  geom_line(aes(y = pred), color = "red", linewidth = 0.8) +  # líneas de residuos
  labs(title = "Modelo lineal: Precipitación vs Siniestros",
       x = "Precipitación (mm)",
       y = "Siniestros")

##Por clima (lm) ####

# Cargamos librerías necesarias
library(dplyr)
library(ggplot2)

# Aseguramos que 'clima' sea un factor (por si aún no lo es)
df_siniestros$clima <- as.factor(df_siniestros3$clima)

# Seleccionamos columnas necesarias (incluyendo clima)
df_siniestros1 <- df_siniestros3 %>%
  select(ccaa, siniestro_ha, year, clima)

df_tmax1 <- df_tmax_summer %>% 
  select(ccaa, temp, year,clima)

# Unimos los data frames por ccaa y year
df_merged <- merge(df_tmax1, df_siniestros1, by = c("ccaa", "year"))

# Creamos el modelo lineal con temperatura y clima
modelo <- lm(siniestro_ha ~ temp + clima, data = df_merged)

# Resumen del modelo
summary(modelo)

# Añadimos las predicciones al data frame
df_merged$pred <- modelo$fitted.values

# Visualizamos los datos y el modelo
ggplot(df_merged, aes(x = temp, y = siniestro_ha)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = pred), color = "red", linewidth = 0.8) +
  labs(title = "Modelo lineal: Temperatura vs Incendios (con clima)",
       x = "Temperatura (ºC)",
       y = "Número de incendios")

# Visualizamos por cada clima
ggplot(df_merged, aes(x = temp, y = siniestro_ha)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  facet_wrap(~ clima) +
  labs(title = "Relación temperatura-incendios por tipo de clima",
       x = "Temperatura (ºC)", y = "Número de incendios")



##Modelo cuadrático####

modelo_quad <- lm(siniestros ~ temp + I(temp^2), data = df_merged)
summary(modelo_quad)

# Predicción
df_merged$pred_quad <- predict(modelo_quad)

# Visualización
ggplot(df_merged, aes(x = temp, y = siniestros)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = pred_quad), color = "blue", linewidth = 1) +
  labs(title = "Relación cuadrática: Temperatura vs Incendios",
       x = "Temperatura (ºC)", y = "Número de incendios")


##Gráfico de dispersión por comunidad####
ggplot(df_merged, aes(x = temp, y = siniestros)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ ccaa) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre temperatura e incendios por CCAA",
       x = "Temperatura (ºC)", y = "Número de incendios")

ggplot(df_merged, aes(x = temp, y = siniestros)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  facet_wrap(~ clima) +
  labs(title = "Temperatura vs Incendios por tipo de clima",
       x = "Temperatura (ºC)", y = "Número de incendios")


##Modelo con interacción entre temperatura media y siniestros por clima ####
# Librerías
library(dplyr)
library(ggplot2)

# Asegúrate de que 'clima' es factor
df_siniestros$clima <- as.factor(df_siniestros3$clima)

# Crear datasets seleccionando las columnas necesarias
df_siniestros1 <- df_siniestros %>%
  select(ccaa, siniestros, year, clima)

df_tmed1 <- df_tmax_summer %>% 
  select(ccaa, temp, year)

# Unir por ccaa y year
df_merged <- merge(df_tmed1, df_siniestros1, by = c("ccaa", "year"))

# Comprobar columnas disponibles
print(colnames(df_merged))

modelo_interaccion <- lm(siniestros ~ temp * clima, data = df_merged)

# Mostramos el resumen del modelo
summary(modelo_interaccion)

# Guardamos las predicciones del nuevo modelo
df_merged$pred_interaccion <- modelo_interaccion$fitted.values

# Visualización del modelo con interacción
ggplot(df_merged, aes(x = temp, y = siniestros)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_interaccion), color = "red", linewidth = 0.8) +
  facet_wrap(~ clima) +
  labs(title = "Modelo lineal con interacción: Temperatura x Clima",
       x = "Temperatura (ºC)",
       y = "Siniestros")

#visualización por clima
ggplot(df_merged, aes(x = temp, y = siniestros, color = clima)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(
    "azul" = "blue",
    "naranja" = "orange",
    "rojo" = "red"
  )) +
  labs(title = "Relación temperatura-incendios según clima (modelo con interacción)",
       x = "Temperatura (ºC)",
       y = "Siniestros")


##Modelo con interacción entre temperatura media y siniestros por clima (dos climas)####
# Librerías
library(dplyr)
library(ggplot2)

# Asegúrate de que 'clima' es factor
df_siniestros2$clima <- as.factor(df_siniestros2$clima)

# Crear datasets con las columnas necesarias y clima ya asignado
df_siniestros1 <- df_siniestros2 %>%
  select(ccaa, siniestros, year, clima)

df_tmed1 <- df_tmedia_summer %>% 
  select(ccaa, temp, year)

# Unir los datos por ccaa y year
df_merged <- merge(df_tmed1, df_siniestros1, by = c("ccaa", "year"))

# Modelo con interacción temperatura x clima
modelo_interaccion <- lm(siniestros ~ temp * clima, data = df_merged)

# Añadir predicciones
df_merged$pred_interaccion <- modelo_interaccion$fitted.values

#Analizamos la correlación para cada ccaa mediante el método Pearson
df_cor <- df_merged %>%
  group_by(clima) %>%
  summarize(correlacion = cor(temp, siniestros, method = "pearson"))
df_cor

# + :relación de proporcionalidad directa.
# 0 :sin relación lineal.
# − :relación de proporcionalidad inversa.

modelo <- lm(siniestros ~ temp, data = df_merged)
summary(modelo)

# Gráfico por zonas climáticas
ggplot(df_merged, aes(x = temp, y = siniestros)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_interaccion), color = "red", linewidth = 0.8) +
  facet_wrap(~ clima) +
  labs(title = "Modelo lineal con interacción: Temperatura x Clima",
       x = "Temperatura media (ºC)",
       y = "Número de siniestros")

#visualización por clima
ggplot(df_merged, aes(x = temp, y = siniestros, color = clima)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(
    "azul" = "blue",
    "naranja" = "orange",
    "rojo" = "red"
  )) +
  labs(title = "Relación temperatura-incendios según clima (modelo con interacción)",
       x = "Temperatura (ºC)",
       y = "Siniestros")


##Modelo con interacción entre temperatura máximas y siniestros por clima ####
# Librerías
library(dplyr)
library(ggplot2)

# Asegúrate de que 'clima' es factor
df_siniestros$clima <- as.factor(df_siniestros$clima)

# Crear datasets seleccionando las columnas necesarias
df_siniestros1 <- df_siniestros %>%
  select(ccaa, siniestros, year, clima)

df_tmax1 <- df_tmax_summer %>% 
  select(ccaa, temp, year)

# Unir por ccaa y year
df_merged <- merge(df_tmax1, df_siniestros1, by = c("ccaa", "year"))

# Comprobar columnas disponibles
print(colnames(df_merged))

modelo_interaccion <- lm(siniestros ~ temp * clima, data = df_merged)

# Mostramos el resumen del modelo
summary(modelo_interaccion)

# Guardamos las predicciones del nuevo modelo
df_merged$pred_interaccion <- modelo_interaccion$fitted.values

# Visualización del modelo con interacción
ggplot(df_merged, aes(x = temp, y = siniestros)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_interaccion), color = "red", linewidth = 0.8) +
  facet_wrap(~ clima) +
  labs(title = "Modelo lineal con interacción: Temperatura x Clima",
       x = "Temperatura (ºC)",
       y = "Número de incendios")

#visualización por clima
ggplot(df_merged, aes(x = temp, y = siniestros, color = clima)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(
    "azul" = "blue",
    "naranja" = "orange",
    "rojo" = "red"
  )) +
  labs(title = "Relación temperatura-incendios según clima (modelo con interacción)",
       x = "Temperatura (ºC)",
       y = "Número de incendios")



##Modelo con interacción entre precipitaciones y siniestros por clima ####
# Librerías
library(dplyr)
library(ggplot2)

# Asegúrate de que 'clima' es factor
df_siniestros$clima <- as.factor(df_siniestros$clima)

# Crear datasets seleccionando las columnas necesarias
df_siniestros1 <- df_siniestros %>%
  select(ccaa, siniestros, year, clima)

df_tmax1 <- df_prep %>% 
  select(ccaa, prep, year)

# Unir por ccaa y year
df_merged <- merge(df_tmax1, df_siniestros1, by = c("ccaa", "year"))

# Comprobar columnas disponibles
print(colnames(df_merged))

modelo_interaccion <- lm(siniestros ~ prep * clima, data = df_merged)

# Mostramos el resumen del modelo
summary(modelo_interaccion)

# Guardamos las predicciones del nuevo modelo
df_merged$pred_interaccion <- modelo_interaccion$fitted.values

# Visualización del modelo con interacción
ggplot(df_merged, aes(x = prep, y = siniestros)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_interaccion), color = "red", linewidth = 0.8) +
  facet_wrap(~ clima) +
  labs(title = "Modelo lineal con interacción: Precipitación x Clima",
       x = "Precipitación (mm)",
       y = "Siniestros")

#visualización por clima
ggplot(df_merged, aes(x = prep, y = siniestros, color = clima)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(
    "azul" = "blue",
    "naranja" = "orange",
    "rojo" = "red"
  )) +
  labs(title = "Relación precipitación-incendios según clima (modelo con interacción)",
       x = "Precipitación (mm)",
       y = "Siniestros")


##Modelo con interacción entre precipitaciones y siniestros por clima (para 2 climas) ####
# Librerías
library(dplyr)
library(ggplot2)

# Asegúrate de que 'clima' es factor
df_siniestros$clima <- as.factor(df_siniestros2$clima)

# Crear datasets seleccionando las columnas necesarias
df_siniestros1 <- df_siniestros %>%
  select(ccaa, siniestros, year, clima)

df_tmax1 <- df_prep %>% 
  select(ccaa, prep, year)

# Unir por ccaa y year
df_merged <- merge(df_tmax1, df_siniestros1, by = c("ccaa", "year"))

# Comprobar columnas disponibles
print(colnames(df_merged))

modelo_interaccion <- lm(siniestros ~ prep * clima, data = df_merged)

# Mostramos el resumen del modelo
summary(modelo_interaccion)

# Guardamos las predicciones del nuevo modelo
df_merged$pred_interaccion <- modelo_interaccion$fitted.values

#Analizamos la correlación para cada ccaa mediante el método Pearson
df_cor <- df_merged %>%
  group_by(clima) %>%
  summarize(correlacion = cor(prep, siniestros, method = "pearson"))
df_cor


# Visualización del modelo con interacción
ggplot(df_merged, aes(x = prep, y = siniestros)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_interaccion), color = "red", linewidth = 0.8) +
  facet_wrap(~ clima) +
  labs(title = "Modelo lineal con interacción: Precipitación x Clima",
       x = "Precipitación (mm)",
       y = "Siniestros")

#visualización por clima
ggplot(df_merged, aes(x = prep, y = siniestros, color = clima)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c(
    "azul" = "blue",
    "naranja" = "orange",
    "rojo" = "red"
  )) +
  labs(title = "Relación precipitación-incendios según clima (modelo con interacción)",
       x = "Precipitación (mm)",
       y = "Siniestros")











#Graficas####


ggplot(df_siniestros, aes(x=year, y=siniestros, color = ccaa)) + geom_line(linewidth = 1) 


ggplot(df_siniestros, aes(x=year, y=incendios, color = ccaa)) + geom_line(linewidth = 1) 
ggplot(df_siniestros, aes(x = year, y = incendios, color = ccaa)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Año",
    y = "Número de incendios",
    color = "Comunidad Autónoma"
  )



ggplot(df_siniestros, aes(x= year, y= ha_forestal, fill = ccaa))+ geom_bar(stat = "identity", position = "dodge") 


ggplot(df_siniestros, aes(x= year, y= siniestro_ha, fill = ccaa))+ geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.05) + paint

ggplot(df_siniestros, aes(x= year, y= siniestro_ha, fill = ccaa))+ geom_bar(stat = "identity", position = "dodge") 

ggplot(df_prep, aes(x=year, y=prep, color = ccaa)) + geom_line(linewidth = 1)+
  labs(
    x = "Año",
    y = "Precipitación media diaria (mm)",
    color = "Comunidad Autónoma"
  )
ggplot(df_prep_year, aes(x=year, y=prep, color = ccaa)) + geom_line(linewidth = 1)+
  labs(
    x = "Año",
    y = "Precipitación media anual (mm)",
    color = "Comunidad Autónoma"
  )

ggplot(df_temp, aes(x=year, y=temp, color = ccaa)) + geom_line(linewidth = 1) +
  labs(
    x = "Año",
    y = "Temperatura media (ºC) ",
    color = "Comunidad Autónoma"
  )




#Mapas####
##Mapa siniestros####

df_siniestros_filtrado <- df_siniestros %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, suma, year)

df_siniestros_filtrado <-  data.frame(df_siniestros_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                             "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                             "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                             "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_siniestros_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = suma), color = "black") +
  scale_fill_gradient(low = "darkseagreen1", high = "darkgreen", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Siniestros por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Siniestros"
  ) +
  facet_wrap(~year)

##Mapa sins/ha####

df_siniestros_filtrado <- df_siniestros %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, siniestro_ha, year)

df_siniestros_filtrado <-  data.frame(df_siniestros_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                             "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                             "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                             "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_siniestros_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = siniestro_ha), color = "black") +
  scale_fill_gradient(low = "darkseagreen1", high = "darkgreen", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Siniestros por hectárea por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Siniestros por Hectárea"
  ) +
  facet_wrap(~year)

##Mapa incendios####

df_siniestros_filtrado <- df_siniestros %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, incendios, year)

df_siniestros_filtrado <-  data.frame(df_siniestros_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                             "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                             "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                             "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_siniestros_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = incendios), color = "black") +
  scale_fill_gradient(low = "darkseagreen1", high = "darkgreen", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Incendios por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Incendios"
  ) +
  facet_wrap(~year)



##Mapa conatos ( incendios de < 1ha ) ####

df_siniestros_filtrado <- df_siniestros %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, conatos, year)

df_siniestros_filtrado <-  data.frame(df_siniestros_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                             "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                             "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                             "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_siniestros_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = conatos), color = "black") +
  scale_fill_gradient(low = "darkseagreen1", high = "darkgreen", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Conatos por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Conatos"
  ) +
  facet_wrap(~year)


##Mapa prep diaria ####
df_prep_filtrado <- df_prep %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, prep, year)

df_prep_filtrado <-  data.frame(df_prep_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                 "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                 "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                 "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_prep_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = prep), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Precipitación media diaria por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Precipitación (mm)"
  ) +
  facet_wrap(~year)


##Mapa prep anual ####
df_prep_filtrado <- df_prep_year %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, prep, year)

df_prep_filtrado <-  data.frame(df_prep_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                 "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                 "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                 "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_prep_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = prep), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Precipitación media anual por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Precipitación (mm)"
  ) +
  facet_wrap(~year)


##Mapa temp####
df_temp_filtrado <- df_temp %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, temp, year)

df_temp_filtrado <-  data.frame(df_temp_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                 "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                 "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                 "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_temp_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = temp), color = "black") +
  scale_fill_gradient(low = "coral1", high = "darkred", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Temperatura Media por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Temperatura (ºC)"
  ) +
  facet_wrap(~year)




##Mapa temperaturas medias de verano####
df_temp_filtrado <- df_tmedia_summer %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, temp, year)

df_temp_filtrado <-  data.frame(df_temp_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                 "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                 "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                 "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_temp_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = temp), color = "black") +
  scale_fill_gradient(low = "coral1", high = "darkred", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Temperatura Medias de Verano por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Temperatura (ºC)"
  ) +
  facet_wrap(~year)

##Mapa temperaturas máximas de verano####
df_temp_filtrado <- df_tmax_summer %>%
  filter(year %in% c("2007", "2015", "2023")) %>%
  select(ccaa, temp, year)

df_temp_filtrado <-  data.frame(df_temp_filtrado, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                 "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                 "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                 "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_temp_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = temp), color = "black") +
  scale_fill_gradient(low = "coral1", high = "darkred", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Temperatura máximas de verano por Comunidad Autónoma",
    subtitle = "Años 2007, 2015 y 2023",
    fill = "Temperatura (ºC)"
  ) +
  facet_wrap(~year)


##Mapa clim para 3 zonas climáticas####
df_siniestros_filtrado3 <- df_siniestros3 %>%
  filter(year %in% c("2007")) %>%
  select(ccaa, suma, year, clima)
df_siniestros_filtrado3 <- df_siniestros_filtrado3 %>% mutate(clima = ifelse(clima == "rojo", 3, clima))
df_siniestros_filtrado3 <- df_siniestros_filtrado3 %>% mutate(clima = ifelse(clima == "naranja", 2, clima))
df_siniestros_filtrado3 <- df_siniestros_filtrado3 %>% mutate(clima = ifelse(clima == "azul", 1, clima))


df_siniestros_filtrado <-  data.frame(df_siniestros_filtrado3, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                              "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                              "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                              "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_siniestros_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = clima), color = "black")+
  labs(
    title = "Zonas climáticas"
  ) + scale_fill_manual(values=c("blue3","orange2","red3"), name = "Tipo de clima",
                        labels = c("Atlántico", "Mediterráneo Interior", "Mediterráneo"))


##Mapa clim para 2 zonas climáticas####
df_siniestros_filtrado2 <- df_siniestros2 %>%
  filter(year %in% c("2007")) %>%
  select(ccaa, suma, year, clima)
df_siniestros_filtrado2 <- df_siniestros_filtrado2 %>% mutate(clima = ifelse(clima == "rojo", 3, clima))
df_siniestros_filtrado2 <- df_siniestros_filtrado2 %>% mutate(clima = ifelse(clima == "naranja", 2, clima))
df_siniestros_filtrado2 <- df_siniestros_filtrado2 %>% mutate(clima = ifelse(clima == "azul", 1, clima))


df_siniestros_filtrado <-  data.frame(df_siniestros_filtrado2, nuts2.name = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears",
                                                                              "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                                                              "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
                                                                              "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "La Rioja"))

spain_map <- esp_get_ccaa()

mapa_completo <- spain_map %>%
  left_join(df_siniestros_filtrado, by = )

ggplot(mapa_completo) +
  geom_sf(aes(fill = clima), color = "black")+
  labs(
    title = "Zonas climáticas"
  ) + scale_fill_manual(values=c("blue3","orange2","red3"),name = "Tipo de clima",
                        labels = c("Atlántico", "Mediterráneo", "NA"))





