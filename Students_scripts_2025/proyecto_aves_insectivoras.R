


rm(list=ls()) #Limpia la memoria
setwd('C:\\Users\\samue\\Documents\\GAR') #Pone el directorio donde tengo los datos
library(tidyverse)
library(dplyr) #cargan los paquetes

library(sf)


library(patchwork)



#Dataframe de Aves ####

basedatos <- read.table("BD_IEET_2015.txt", header = TRUE, sep = ";", na.strings = " ") #lee los datos
str(basedatos)
basedatos_aves <- basedatos[basedatos$Clase == "Aves",] #selecciono solo la clase = aves

unique(basedatos$Clase)

## Riqueza de aves por cada parcela ####

riq_aves_par <- basedatos_aves |>  #Creo otro data frame donde calculo la riqueza de aves por cada 
  group_by(CUTM10x10) |>           #parcela
  summarise(riqueza_aves = n_distinct(Especie)) 


#Dataframe de insectos ####

basedatos_insectos <- basedatos[basedatos$Clase == "Insecta",] #selecciono solo las clase = insectos

## Riqueza de insectos por cada parcela ####

riq_insect_par <- basedatos_insectos |> #riqueza de insectos por parcela 
  group_by(CUTM10x10) |>
  summarise(riqueza_insect= n_distinct((Nombre)))

# Mezclo ambos data frame ####

##Parcelas en común, para comparar las riquezas ####

filasquequieroaves = which(riq_aves_par$CUTM10x10 %in% riq_insect_par$CUTM10x10) #Selecciono las parcelas del data frame de aves que aparecen en el data frame de insectos
filasquequeiroinsectos = which(riq_insect_par$CUTM10x10 %in% riq_aves_par$CUTM10x10) #Selecciono las parcelas del data frame de insectos que aparecen en el data frame de insectos

riq_aves_par_sub = riq_aves_par[filasquequieroaves,]     #Creo mis dos data frame con las parcelas comunes
riq_insect_par_sub = riq_insect_par[filasquequeiroinsectos,]
summary(riq_aves_par_sub)
summary(riq_insect_par_sub)

# Uno mis data frame por las parcelas
riqueza_total <- merge(riq_aves_par_sub,riq_insect_par_sub, by = "CUTM10x10") #Uno ambos data frame en función de las parcela, obteniendo un unico data frame con la riqueza de ambas especies por parcela

riqueza_total

#Visualización de ambas riquezas####
ggplot(riqueza_total, aes(x = riqueza_insect, y = riqueza_aves)) + #Represento graficamente ambas riquezas
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.5)+
  xlab("Riqueza de insectos") +
  ylab("Riqueza de aves") +
  theme_bw()

# No se ve muy bien, se puede deber a que en el dataframe de aves no tuvimos en cuenta la alimentación de las aves  

#Correción Dataframe aves####

#Cargo otra base de datos, donde tengo el tipo de alimentación de las especies de aves

datosalimentacion <- read.csv("Avonet1_BirdLife.csv", header = TRUE, sep = ",", na.strings = c("NA","NO"))
unique(datosalimentacion$Trophic.Niche)

#Añado alimentación a mi data frame de aves

avesyalimentacion <- merge(basedatos_aves, datosalimentacion[,c("Species1",
                                                                "Trophic.Niche")],
                           by.x = "Nombre", 
                           by.y = "Species1",
                           all.x = TRUE)
avesyalimentacion
#Me quedo solo con las insectivoras

avesinsectivoras <- avesyalimentacion[avesyalimentacion$Trophic.Niche == "Invertivore",]

avesinsectivoras

##Riqueza de aves corregida por parcelas ####

riq_aves_par2 <- avesinsectivoras |>  #Creo otro data frame donde calculo la riqueza                                       de aves por cada 
  group_by(CUTM10x10) |>              #parcela
  summarise(riqueza_aves = n_distinct(Nombre)) 

#Riqueza de insectos corregida por parcelas (en realidad es la misma, pero le cambio nombre para no confundirme)

riq_insect_par2 <- basedatos_insectos |> #riqueza de insectos por parcela 
  group_by(CUTM10x10) |>
  summarise(riqueza_insect= n_distinct((Especie)))


#Mezclo mis data frame corregidos

filasquequieroaves2 = which(riq_aves_par2$CUTM10x10 %in% riq_insect_par2$CUTM10x10) #Selecciono las parcelas del data frame de aves que aparecen en el data frame de insectos
filasquequeiroinsectos2 = which(riq_insect_par2$CUTM10x10 %in% riq_aves_par2$CUTM10x10) #Selecciono las parcelas del data frame de insectos que aparecen en el data frame de insectos



riq_aves_par2_sub = riq_aves_par2[filasquequieroaves2,]     #Creo mis dos data frame con las parcelas comunes
riq_insect_par2_sub = riq_insect_par2[filasquequeiroinsectos2,]


riqueza_total2 <- merge(riq_aves_par2_sub,riq_insect_par2_sub, by = "CUTM10x10") #Uno ambos data frame en función de las parcela, obteniendo un unico data frame con la riqueza de ambas especies por parcela

riqueza_total2

ggplot(riqueza_total2, aes(x = riqueza_insect, y = riqueza_aves)) + #Represento graficamente ambas riquezas
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +
  xlab("Riqueza de insectos") +
  ylab("Riqueza de aves insectivoras") +
  theme_bw()



  
#Representacion espacial####

#Cargo la base de datos con las cuadriculas para representarlas 

mapa_cuadriculas <- st_read("Malla10x10_Ter_p.shp")
plot(mapa_cuadriculas$geometry, col="darkred",lwd=0.5)
mapa_cuadriculas

##Uno mi base de riqueza con el las cuadriculas####

basedatosfinal <- merge(mapa_cuadriculas,
                        riqueza_total2, 
                        by.x ="COD_INB", 
                        by.y =  "CUTM10x10", 
                        all.x = TRUE) #Uno mi base de datos siguiendo el codigo de parcelas

basedatosfinal
str(basedatosfinal)

mapa_riq_aves_insecti <- ggplot(basedatosfinal) +
  geom_sf(aes(fill = riqueza_aves),
          color ="grey70",
          lwd = .3)  +
  scale_fill_gradientn(colors = hcl.colors(10,"Greens", rev = TRUE),
    n.breaks = 10,
    guide = guide_legend(title = "Riqueza de aves insectivoras")) +
  labs(
    title = "Mapa de riqueza de aves insectivoras por cuadriculas UTM",
    subtitle = "Datos extraídos de MITECO"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

mapa_riq_aves_insecti  #Dibujo el mapa de riqueza de especies
ggsave("mapa_riq_aves_insecti.png", width=10, height=10, unit="cm", dpi=500)



mapa_riq_insect <- ggplot(basedatosfinal) +
  geom_sf(aes(fill = riqueza_insect),
          color ="grey70",
          lwd = .3) +
  scale_fill_gradientn(colors = hcl.colors(10,"Reds", rev = TRUE),
    n.breaks = 10,
    guide = guide_legend(title = "Riqueza de insectos")) +
  labs(
    title = "Mapa de riqueza de insectos por cuadriculas UTM",
    subtitle = "Datos extraídos de MITECO"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

mapa_riq_insect


#Para los mapas sin eliminar parcelas: 

basedatos <- read.table("BD_IEET_2015.txt", header = TRUE, sep = ";", na.strings = " ") #lee los datos
str(basedatos)
basedatos_aves <- basedatos[basedatos$Clase == "Aves",] #selecciono solo la clase = aves
View(basedatos_aves)
unique(basedatos$Clase)

## Riqueza de aves por cada parcela ####

datosalimentacion <- read.csv("Avonet1_BirdLife.csv", header = TRUE, sep = ",", na.strings = c("NA","NO"))
unique(datosalimentacion$Trophic.Niche)

#Añado alimentación a mi data frame de aves

avesyalimentacion <- merge(basedatos_aves, datosalimentacion[,c("Species1",
                                                                "Trophic.Niche")],
                           by.x = "Nombre", 
                           by.y = "Species1",
                           all.x = TRUE)
avesyalimentacion
#Me quedo solo con las insectivoras

avesinsectivoras <- avesyalimentacion[avesyalimentacion$Trophic.Niche == "Invertivore",]

riq_aves_par <- avesinsectivoras |>  #Creo otro data frame donde calculo la riqueza                                       de aves por cada 
  group_by(CUTM10x10) |>              #parcela
  summarise(riqueza_aves = n_distinct(Nombre)) 


#Dataframe de insectos ####

basedatos_insectos <- basedatos[basedatos$Clase == "Insecta",] #selecciono solo las clase = insectos



## Riqueza de insectos por cada parcela ####

riq_insect_par <- basedatos_insectos |> #riqueza de insectos por parcela 
  group_by(CUTM10x10) |>
  summarise(riqueza_insect= n_distinct((Nombre)))


avesyparcelas <- merge(mapa_cuadriculas,
                        riq_aves_par, 
                        by.x ="COD_INB", 
                        by.y =  "CUTM10x10", 
                        all.x = TRUE) 
insectosyparcelas <- merge(mapa_cuadriculas,
                           riq_insect_par,
                           by.x = "COD_INB",
                           by.y = "CUTM10x10",
                           all.x = TRUE)

mapa_riq_aves2 <- ggplot(avesyparcelas) +
  geom_sf(aes(fill = riqueza_aves),
          color ="grey70",
          lwd = .3)  +
  
  scale_fill_gradientn(colors = hcl.colors(10,"Greens", rev = TRUE),
    n.breaks = 10,
    guide = guide_legend(title = "Riqueza")) +
  labs(
    title = "Mapa de riqueza de aves insectivoras por cuadriculas UTM",
    subtitle = "Datos extraídos de MITECO"
  ) +
  
  theme_minimal() +
  
  theme(legend.position = "right")

mapa_riq_aves2


mapa_riq_insect2 <- ggplot(insectosyparcelas) + 
  geom_sf(aes(fill= riqueza_insect),
          color = "grey70", 
          lwd = .3) +
  scale_fill_gradientn(colors = hcl.colors(10, "Reds", rev = TRUE), 
                       n.breaks = 10,
                       guide = guide_legend(title = "Riqueza de insectos")) +
  labs(
    title = "Mapa de riqueza de insectos por cuadriculas UTM",
    subtitle = "Datos extraídos de MITECO"
  ) +
  theme_minimal() +
  theme(legend.position = "right") 

mapa_riq_insect2



#Temp vs Riqueza de especies
unique(basedatos_aves$FechaCUTM)  #Nuestros datos de aves van desde 2004 hata 2012 
#por tanto ese es el margen de tiempo que tenemos que comparar las temperaturas
library(terra)
tempprimerosaños <- rast("tg_ens_mean_0.1deg_reg_1995-2010_v31.0e.nc")
tempsegundosaños <- rast("tg_ens_mean_0.1deg_reg_2011-2024_v31.0e.nc") 


#Filtro años que me interesan y hago un subset de los spatraster inicales

fechas1 <- as.Date(time(tempprimerosaños))
primerosaños <- as.character(2004:2010)
diasprimerosaños <- which(format(fechas1, "%Y") %in% primerosaños)
tempsdiariasprimeros <- subset(tempprimerosaños, diasprimerosaños)

fechas2 <- as.Date(time(tempsegundosaños))
segundosaños <- as.character(2011:2012)
diassegundosaños <- which(format(fechas2, "%Y") %in% segundosaños)
tempsdiariassegundos <- subset(tempsegundosaños, diassegundosaños)

tempsdiariastotales <- c(tempsdiariasprimeros, tempsdiariassegundos)
media_tempdiarias <- app(tempsdiariastotales, fun = mean, na.rm = TRUE)



mapa_cuadriculas <- st_read("Malla10x10_Ter_p.shp")
cuadriculas_vect <- vect(mapa_cuadriculas)

#Comprobamos sistema de coordenadas
crs(media_tempdiarias)  
crs(mapa_cuadriculas)   

crs_utm <- crs(mapa_cuadriculas)
media_temp_utm <- project(media_tempdiarias, crs_utm, method = "bilinear")

media_por_cuadricula <- terra::extract(media_temp_utm, cuadriculas_vect, fun = mean, na.rm = TRUE)
mapa_cuadriculas$media_temp <- media_por_cuadricula[, 2]

riquezasytemps <- merge(mapa_cuadriculas,
                        riq_aves_par, 
                        by.x ="COD_INB", 
                        by.y =  "CUTM10x10", 
                        all.x = TRUE) 


ggplot(riquezasytemps, aes(x = media_temp, y = riqueza_aves)) + 
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, span = 0.5)+
  xlab("Temperaturas medias") +
  ylab("Riqueza de aves insectivoras") +
  theme_bw()

mapa1 <- ggplot(data = mapa_cuadriculas) +
  geom_sf(aes(fill = media_temp), color = "black") +   
  scale_fill_viridis_c(name = "Temp. media (°C)", option = "plasma") +
  theme_minimal()  +
  labs(title = "Temperatura media por cuadrícula UTM (2004-2012)", 
       subtitle = "Datos extraídos de E-OBS") 
(mapa1 + mapa_riq_aves2)




















