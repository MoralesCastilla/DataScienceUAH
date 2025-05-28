
# Establecemos el directorio de trabajo

setwd("~/GitHub/prac_reptiles/2_distrib")



# Llamamos a las librerías que vamos a usar

library(tidyverse)
library(rgbif)
library(terra)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(googleway)
library(ncdf4)
library(geodata)
library(stringr)

# Leemos y visualizamos los datos de temperatura

datos_temp_clean <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/datos_temp_clean.csv", sep = ",", dec = ".")[,-1]

glimpse(datos_temp_clean)


#Obtenemos una media de latitud para la distrib de la spp
spp_names <- datos_temp_clean$species
spp_2000 <- occ_data(scientificName = c(as.character(spp_names)),
                    limit = 2000)

spp_2000$`Acanthophis antarcticus`$data$decimalLatitude
colnames(spp_2000$`Acanthophis antarcticus`$data)
latsmediasreptile_2000 = as.data.frame(array(NA,dim=c(71,2)))
colnames(latsmediasreptile_2000) = c("species","latmedia")
for(i in 1:71){
  
print(i)
  latsmediasreptile_2000[i,1] <- spp_names[i]
    latsmediasreptile_2000[i,2] <- mean(spp_2000[[i]]$data$decimalLatitude,na.rm=T)
  
}

latsmediasreptile_2000
write.csv(latsmediasreptile_2000, "~/GitHub/prac_reptiles/1_temp/1_data/mean_lat_rep2000.csv")

coords_rep_2000<-list() #Como hay datos con distintos valores usamos una lista

for(i in 1:length(spp_names)){
  print(i)
  coords_rep_2000[[i]] <- spp_2000[[i]]$data[,c(2,4,3)]
}       

cords_rep_join_2000 <- do.call(rbind, coords_rep_2000)

write.csv(cords_rep_join_2000, "~/GitHub/prac_reptiles/1_temp/1_data/long_lat_rep2000.csv")

#Lo hacemos con 5000 para comparar

spp_5000 <- occ_data(scientificName = c(as.character(spp_names)),
                     limit = 5000)

spp_names <- datos_temp_clean$species
latsmediasreptile_5000 = as.data.frame(array(NA,dim=c(71,2)))
colnames(latsmediasreptile_5000) = c("species","latmedia")

for(i in 1:71){
  
  print(i)
  latsmediasreptile_5000[i,1] <- spp_names[i]
  latsmediasreptile_5000[i,2] <- mean(spp_5000[[i]]$data$decimalLatitude,na.rm=T)
  
}

latsmediasreptile_5000
write.csv(latsmediasreptile_5000, "~/GitHub/prac_reptiles/1_temp/1_data/mean_lat_rep5000.csv")

coords_rep_5000<-list() #Como hay datos con distintos valores usamos una lista

for(i in 1:length(spp_names)){
  print(i)
  coords_rep_5000[[i]] <- spp_5000[[i]]$data[,c(2,4,3)]
}       

cords_rep_join_5000 <- do.call(rbind, coords_rep_5000)

write.csv(cords_rep_join_5000, "~/GitHub/prac_reptiles/1_temp/1_data/long_lat_rep5000.csv")

#comprobamos si los datos funcionan

long_lat_rep2000 <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/long_lat_rep2000.csv", sep = ",", dec = ".")[,-1]
long_lat_rep5000 <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/long_lat_rep5000.csv", sep = ",", dec = ".")[,-1]
mean_lat_rep2000 <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/mean_lat_rep2000.csv", sep = ",", dec = ".")[,-1]
mean_lat_rep5000 <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/mean_lat_rep5000.csv", sep = ",", dec = ".")[,-1]

# vemos si hay diferencia de precisión

precis_diff <- data.frame(spp_names, 
                          diferencia = abs(mean_lat_rep2000$latmedia - mean_lat_rep5000$latmedia))


#Para especies donde habia más datos si hay diferencias, para las que no, no

# Vemos la distribucion de las especies 
mapa_mundo <- world(resolution = 2 , path ="~/Sandbox" )
plot(mapa_mundo)
points(x = long_lat_rep5000$decimalLongitude,
       y = long_lat_rep5000$decimalLatitude,
       col="darkred",pch=19,cex=0.7)


#Sacamos los datos del pais de ocurrencia de cada especie

country_rep = list()

for(i in 1:length(spp_names)){
  print(i)
  country_rep[[i]] <- spp_5000[[i]]$data[,c( "scientificName" , "country")]
}       

country_rep_join <- do.call(rbind, country_rep)

#Como salen subespecies y nombres de autores los quitamos
country_rep_clean <- country_rep_join %>%
  mutate(
    scientificName = str_c(word(scientificName, 1), 
                           word(scientificName, 2), sep = " ")
    #con esto extraes las 2 primeras palabras
  )

#Ahora lo resumimos
country_rep_summ <- country_rep_clean %>%
  filter(!is.na(country)) %>%
  group_by(scientificName) %>%
  summarise(paises = paste(unique(country), collapse = ", "))

#Y lo pasamos a formato tidy
country_rep_summ_tidy <- country_rep_summ %>%
  separate_rows(paises, sep = ",")

write.csv(country_rep_summ_tidy, "~/GitHub/prac_reptiles/2_distrib/1_data/spp_country.csv")

#Sacamos los datos del pais de ocurrencia de cada familia 

country_rep_fam = list()

for(i in 1:length(spp_names)){
  print(i)
  country_rep_fam[[i]] <- spp_5000[[i]]$data[,c( "family" , "country")]
}       

country_rep_fam_join <- do.call(rbind, country_rep_fam)

#Ahora lo resumimos
country_rep_fam_summ <- country_rep_fam_join %>%
  filter(!is.na(country)) %>%
  group_by(family) %>%
  summarise(paises = paste(unique(country), collapse = ", "))

#Y lo pasamos a formato tidy
country_rep_fam_summ_tidy <- country_rep_fam_summ %>%
  separate_rows(paises, sep = ",")

#Y seleccionamos las familias que nos interesan
country_rep_fam_summ_filter <- country_rep_fam_summ[c(3, 4 , 7),]

#Y lo pasamos a formato tidy
country_rep_fam_summ_filter_tidy <- country_rep_fam_summ_filter %>%
  separate_rows(paises, sep = ",")

write.csv(country_rep_fam_summ_tidy, "~/GitHub/prac_reptiles/2_distrib/1_data/family_country_full.csv")
write.csv(country_rep_fam_summ_filter_tidy, "~/GitHub/prac_reptiles/2_distrib/1_data/family_country_filter.csv")

