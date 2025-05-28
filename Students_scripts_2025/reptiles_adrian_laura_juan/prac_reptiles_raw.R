
# Establecemos el directorio de trabajo

setwd("~/GitHub/prac_reptiles/0_data_exp/1_data")



# Llamamos a las librerías que vamos a usar

library(tidyverse)



# Leemos los datos y los vemos

datos_reptiles <- read.csv("datos_reptiles_full.csv", sep = ";", dec = ",")

glimpse(datos_reptiles)



# Nos quedamos solo con las columnas que necesitamos (también les cambiamos los nombres)

colnames(datos_reptiles) # Para ver los nombres de las variables/columnas

datos_temp <- datos_reptiles %>% 
  select(c(Species, Family, Order, Main.biogeographic.region, Minimal.elevation..m., Maximum.elevation..m., Mean.Annual.Temperature...C.,
           Temperature.Seasonality..standard.deviation.100., Mean.Tb, Minimum.Tb, Maximum.Tb)) %>% 
  rename(species = Species, order = Order, family = Family, bio_region = Main.biogeographic.region, min_elevation = Minimal.elevation..m.,
         max_elevation = Maximum.elevation..m.,
         mean_temp = Mean.Annual.Temperature...C., temp_seasonality = Temperature.Seasonality..standard.deviation.100.,
         mean_tb = Mean.Tb, min_tb = Minimum.Tb, max_tb = Maximum.Tb)

# write.csv(datos_temp, "~/GitHub/prac_reptiles/0_data_exp/1_data/datos_temp.csv") # Guardamos el dataframe



# Quitamos los NA's

datos_temp_clean <- na.omit(datos_temp)

glimpse(datos_temp_clean)

which(is.na(datos_temp_clean), arr.ind=TRUE) # Buscamos NA's para asegurarnos de que ha funcionado :)

write.csv(datos_temp_clean, "~/GitHub/prac_reptiles/1_temp/1_data/datos_temp_clean.csv")

# Guardamos el dataframe



# Leemos y visualizamos los nuevos datos

datos_temp_clean <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/datos_temp_clean.csv", sep = ",", dec = ".")[-1]

glimpse(datos_temp_clean)

