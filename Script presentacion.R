
# AVES: mi cacao mental ---------------------------------------------------

aves <- read.csv("1-datos/d_aves.csv")
library(tidyverse)

# probamos con `pivot_wider` de `tidy`:------------

aves_2 <- pivot_wider(aves,
                   names_from = Nombre,
                   values_from = EstadoCUTM)


# También podemos usar`spread` de `tidy` para pasar a formato ancho:------

aves_2 <- spread(aves, Nombre, EstadoCUTM)


# Eliminar los repetidos--> ¿¿con `group_by` y `filter`?? desppués de spread o pivot_wider:----

aves_2 <- aves_2 %>% 
  group_by(CUADRICULA) %>% ## hago un filter del CUADRICULA a la vez seleccionando
  filter(! duplicated (CUADRICULA)) ## los NO duplicados

# ¡¡¡FUNCIONA!!!



# SUELOS: la que has liado pollito ----------------------------------------

suelos_2 <- read.csv("1-datos/suelos.csv")

# Todo el problema sobrevino de que pensaba que solo tenía los datos del suelo en 
# formato csv. Cuando empece a trabajar con suelos guardé la tabla df del shapefile 
# como un csv, no recuerdo porqué...

# Llegue a pensar que no tenía datos espaciales y por eso intente descargarmelos usando
# el paquete `mapSpain` por municipios:

library(mapSpain)

municipios <- mapSpain::esp_get_munic(year= "2019", epsg = "3857")


suelos_geo <- merge(municipios, suelos_2 [, c("antro", "label")])

# Exploración de los datos de municipio para ver si se repiten valores:

length(unique(suelos$label))

which.max(table(suelos$label))

suelos[which(suelos$label=="Albal"),]

# Luego me di cuenta que tenía los datos espaciales y aquí me quedé, que me lío me hice yo
# y que lío le hice a Nacho.










