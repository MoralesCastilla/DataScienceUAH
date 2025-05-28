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

# Leemos los datos

datos_temp_clean <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/datos_temp_clean.csv", sep = ",", dec = ".")[,-1]

fam_country <- read.csv("~/GitHub/prac_reptiles/2_distrib/1_data/family_country_filter.csv", sep = ",", dec = ".")[,-1]

spp_country <- read.csv("~/GitHub/prac_reptiles/2_distrib/1_data/spp_country.csv", sep = ",", dec = ".")[,-1]
  
#Quitamos unois valores que estaban mal
fam_country$paises <- trimws(fam_country$paises)
unique(fam_country$paises)
fam_country <- fam_country %>%
  filter(!paises %in% c("unknown or invalid", "State of", "Democratic Republic of the"))

mapa_mundo <- world(resolution = 2 , path ="~/Sandbox" )
country_name <- mapa_mundo$NAME_0

#Ahora corregimos los nombres para que estén igual que en mapa_mundo

country_name <- data.frame(pais_mal = c(
  "United States of America", "Mexico", "Canada", "French Guiana", "Japan",
  "Russian Federation", "France", "China", "Colombia", "Panama",
  "Venezuela (Bolivarian Republic of)", "Nicaragua", "Benin", "Guatemala", 
  "Costa Rica", "Philippines", "Belize", "Honduras", "Belgium", "Australia", 
  "Papua New Guinea", "Indonesia", "South Africa", "Germany", "Spain", 
  "Kenya", "Brazil", "Argentina", "Paraguay", "Bolivia (Plurinational State of)",
  "Guyana", "Peru", "Suriname", "El Salvador", "Aruba", "Congo", "Denmark", 
  "Martinique", "Trinidad and Tobago", "Palestine", "Israel", "Saudi Arabia",
  "Jordan", "Oman", "Egypt", "Yemen", "Iran (Islamic Republic of)", 
  "United Arab Emirates"
),
pais_bien = c(
  "United States", "Mexico", "Canada", "French Guiana", "Japan",
  "Russia", "France", "China", "Colombia", "Panama",
  "Venezuela", "Nicaragua", "Benin", "Guatemala", 
  "Costa Rica", "Philippines", "Belize", "Honduras", "Belgium", "Australia", 
  "Papua New Guinea", "Indonesia", "South Africa", "Germany", "Spain", 
  "Kenya", "Brazil", "Argentina", "Paraguay", "Bolivia",
  "Guyana", "Peru", "Suriname", "El Salvador", "Aruba", "Congo", "Denmark", 
  "Martinique", "Trinidad and Tobago", "Palestine", "Israel", "Saudi Arabia",
  "Jordan", "Oman", "Egypt", "Yemen", "Iran", 
  "United Arab Emirates"
))

fam_country <- fam_country %>%
  left_join(country_name, by = c("paises" = "pais_mal")) %>%
  mutate(paises = if_else(!is.na(pais_bien), pais_bien, paises)) %>%
  select(-pais_bien)

mapa_familias <- merge(
  mapa_mundo,
  fam_country,
  by.x = "NAME_0", 
  by.y = "paises",     
  all.x = TRUE 
)

mapa_sf <- st_as_sf(mapa_familias)

ggplot(mapa_sf) +
  geom_sf(aes(fill = family)) +
  scale_fill_manual(values = c(Colubridae = "#ffa76b", Elapidae = "#ff96cb", Viperidae = "#b279c9"),
                    labels = c("Colubridae", "Elapidae", "Viperidae", "Sin datos")) +
  theme_minimal() +
  labs(title = "Distribucion global",
       fill = "Familia")

#Como hay algunas que se solapan hacemos um mapa por cada familia

mapa_distrib_fam <- ggplot(mapa_sf) +
  geom_sf(aes(fill = family)) +
  facet_wrap(~family) +
  scale_fill_manual(values = c("Colubridae" = "#ffa76b", 
                               "Elapidae" = "#ff96cb", 
                               "Viperidae" = "#b279c9")) +
  theme_minimal() +
  labs(title = "Distribución por familia",
       fill = "Familia")

# Lista de familias únicas

familias_unicas <- unique(mapa_sf$family)

# Loop para crear un gráfico por cada familia
for (fam in familias_unicas[!is.na(familias_unicas)]) {
  
  mapa_filtrado <- mapa_sf[mapa_sf$family == fam, ]
  
  p <- ggplot(mapa_sf) +
    geom_sf(fill = "gray90") +  
    geom_sf(data = mapa_filtrado, aes(fill = family), color = "white") +
    scale_fill_manual(values = c("Colubridae" = "#ffa76b", 
                                 "Elapidae" = "#ff96cb", 
                                 "Viperidae" = "#b279c9")) +
    theme_minimal() +
    labs(title = paste("Distribución de", fam),
         fill = "Familia")
  
  ggsave(
    filename = paste0("mapa_familia_", fam, ".tiff"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
}

#Hacemos otra forma de visualizar la riqueza con las especies y el pais
#Repetimos lo mismo que para las familias
#Quitamos unois valores que estaban mal

spp_country$paises <- trimws(spp_country$paises)
unique(spp_country$paises)
spp_country <- spp_country %>%
  filter(!paises %in% c("unknown or invalid", "State of", "Democratic Republic of the"))

mapa_mundo <- world(resolution = 2 , path ="~/Sandbox" )
country_name <- mapa_mundo$NAME_0

#Ahora corregimos los nombres para que estén igual que en mapa_mundo

country_name <- data.frame(pais_mal = c(
  "United States of America", "Mexico", "Canada", "French Guiana", "Japan",
  "Russian Federation", "France", "China", "Colombia", "Panama",
  "Venezuela (Bolivarian Republic of)", "Nicaragua", "Benin", "Guatemala", 
  "Costa Rica", "Philippines", "Belize", "Honduras", "Belgium", "Australia", 
  "Papua New Guinea", "Indonesia", "South Africa", "Germany", "Spain", 
  "Kenya", "Brazil", "Argentina", "Paraguay", "Bolivia (Plurinational State of)",
  "Guyana", "Peru", "Suriname", "El Salvador", "Aruba", "Congo", "Denmark", 
  "Martinique", "Trinidad and Tobago", "Palestine", "Israel", "Saudi Arabia",
  "Jordan", "Oman", "Egypt", "Yemen", "Iran (Islamic Republic of)", 
  "United Arab Emirates"
),
pais_bien = c(
  "United States", "Mexico", "Canada", "French Guiana", "Japan",
  "Russia", "France", "China", "Colombia", "Panama",
  "Venezuela", "Nicaragua", "Benin", "Guatemala", 
  "Costa Rica", "Philippines", "Belize", "Honduras", "Belgium", "Australia", 
  "Papua New Guinea", "Indonesia", "South Africa", "Germany", "Spain", 
  "Kenya", "Brazil", "Argentina", "Paraguay", "Bolivia",
  "Guyana", "Peru", "Suriname", "El Salvador", "Aruba", "Congo", "Denmark", 
  "Martinique", "Trinidad and Tobago", "Palestine", "Israel", "Saudi Arabia",
  "Jordan", "Oman", "Egypt", "Yemen", "Iran", 
  "United Arab Emirates"
))

spp_country <- spp_country %>%
  left_join(country_name, by = c("paises" = "pais_mal")) %>%
  mutate(paises = if_else(!is.na(pais_bien), pais_bien, paises)) %>%
  select(-pais_bien) %>% 
  na.omit()

spp_country_num <- spp_country %>%
  group_by(paises) %>%
  summarise(num_especies = n_distinct(scientificName)) %>%
  arrange(desc(num_especies))

mapa_spp <- merge(
  mapa_mundo,
  spp_country_num,
  by.x = "NAME_0", 
  by.y = "paises",     
  all.x = TRUE)

mapa_sf_spp <- st_as_sf(mapa_spp)

#hacemos un plot

mapa_num_spp <- ggplot(mapa_sf_spp) +
  geom_sf(aes(fill = num_especies), color = "gray") +
  scale_fill_gradientn(colors = c("#f7fcb9", "#addd8e","#31a354")) +
  labs(title = "Número de especies por país") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

#Lo guardamos

ggsave("mapa_especies.tiff", plot = mapa_num_spp, width = 10, height = 6)
