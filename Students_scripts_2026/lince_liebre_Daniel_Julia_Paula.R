#################################################
##
##      Trabajo dinámica población Lince
##
#################################################

library(rgbif)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(mapSpain)
library(rnaturalearth)
library(sf)
library(units)

setwd("1_data")

#Obtenemos los datos ####
Sys.setenv(GBIF_USER = "tu usuario",
           GBIF_PWD = "tu contraseña",
           GBIF_EMAIL = "tu correo")

##datos liebre ####
liebre <- occ_search(scientificName = "Lepus granatensis")

data_liebre <- occ_download(pred_and(pred("taxonKey", 2436790),
                                     pred("country","ES"),
                                     pred_gte("year", 1990),
                                     pred_lte("year", 2024),
                                     pred("hasCoordinate",TRUE)),
                            format = "SIMPLE_CSV")

occ_download_wait(data_liebre)

data_liebre <- occ_download_get(data_liebre) |>
  occ_download_import()

#metadatos liebre
metadatos_liebre <- occ_download_meta("0009923-260221153910048")
#Download key: 0009923-260221153910048
#DOI: 10.15468/dl.2hp4hw

##datos lince ####
lince <- occ_search(scientificName = "Lynx pardinus")

data_linces <- occ_download(pred_and(pred("taxonKey", 2435261),
                                     pred("country","ES"),
                                     pred_gte("year", 1990),
                                     pred_lte("year", 2024),
                                     pred("hasCoordinate",TRUE)),
                            format = "SIMPLE_CSV")

occ_download_wait(data_linces)

data_linces <- occ_download_get(data_linces) |>
  occ_download_import()

#metadatos lince
metadatos_lince <- occ_download_meta("0000084-260225131425191")
#Download key: 0000084-260225131425191
#DOI: 10.15468/dl.rn7kba

##datos conejo ####
conejo <- occ_search(scientificName = "Oryctolagus cuniculus")

data_conejo <- occ_download(pred_and(pred("taxonKey", 2436940),
                                     pred("country","ES"),
                                     pred_gte("year", 1990),
                                     pred_lte("year", 2024),
                                     pred("hasCoordinate",TRUE)),
                            format = "SIMPLE_CSV")

occ_download_wait(data_conejo)

data_conejo <- occ_download_get(data_conejo) |>
  occ_download_import()

#metadatos conejo
metadatos_conejo <- occ_download_meta("0029506-260226173443078")
#DOI: 10.15468/dl.perdd7
#Download key: 0029506-260226173443078

#citas y referencias
lepus <- read.csv("1_data/avistamientos_lepus.csv", sep = "\t")
lynx <- read.csv("1_data/avistamientos_lynx.csv", sep = "\t")
oryctolagus <- read.csv("1_data/avistamientos_oryctolagus.csv", sep = "\t")
gbif_citation("0009923-260221153910048") #liebre
gbif_citation("0000084-260225131425191") #lince
gbif_citation("0029506-260226173443078") #conejo

# Leemos los datos ####
censo_lince <- read.csv("avistamientos_lynx.csv", sep = "\t")
censo_liebre <- read.csv("avistamientos_lepus.csv", sep = "\t")
censo_conejo <- read.csv("avistamientos_oryctolagus.csv", sep = "\t")


# Limpiamos los datos ####
## Lince ####
lince_avistamientos <- censo_lince |>
                        select(year, decimalLongitude, decimalLatitude)|>
                        count(year, decimalLatitude, decimalLongitude)|>
                        rename(n_lince = n) |>
                        subset(year != 2007)

lista_lince <- split(lince_avistamientos, lince_avistamientos$year)

#Quitamos los años redundantes
lista_lince <- lapply(lista_lince, function(año){
  año$year <- NULL
  return(año)
})

## Conejo ####
conejo_avistamientos <- censo_conejo |>
                        select(year, decimalLongitude, decimalLatitude)|>
                        count(year, decimalLatitude, decimalLongitude)|>
                        rename(n_conejo = n)

lista_conejo <- split(conejo_avistamientos, conejo_avistamientos$year)

lista_conejo <- lapply(lista_conejo, function(año){
  año$year <- NULL
  return(año)
})

## Liebre ####
liebre_avistamientos <- censo_liebre |>
                        select(year, decimalLongitude, decimalLatitude)|>
                        count(year, decimalLatitude, decimalLongitude)|>
                        rename(n_liebre = n)

lista_liebre <- split(liebre_avistamientos, liebre_avistamientos$year)

lista_liebre <- lapply(lista_liebre, function(año){
  año$year <- NULL
  return(año)
})


# Coordenadas en sf sin año ####
coordlince = st_as_sf(lince_avistamientos[,2:3], coords=c("decimalLongitude","decimalLatitude"),
                      crs=4258)


# Coordenadas con años ####
## Lince ####
lista_sf_lince <- lapply(lista_lince, 
                         function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                                           crs=4258)})
## Conejo ####
lista_sf_conejo <- lapply(lista_conejo, 
                          function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                               crs=4258)})

## Liebre ####
lista_sf_liebre <- lapply(lista_liebre, 
                         function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                              crs=4258)})


#Distancias####
## Distancias entre linces ####
distancias_año_linces <- lapply(lista_sf_lince ,function(x)
                          {st_distance(x)})

# Linces a menos de 10 Km
matriz_distancias_año = lapply(distancias_año_linces, function(x)
                        {rowSums(x< set_units(10000, "m"))-1
                        })

## Distancias entre linces y liebres ####
años_comunes_liebre <- intersect(names(lista_sf_lince), names(lista_sf_liebre))

distancias_año_liebres <- lapply(años_comunes_liebre, function(año){
                          linces <- lista_sf_lince[[año]]
                          liebres <- lista_sf_liebre[[año]]
                          st_distance(linces, liebres)
                          })

# Liebres a menos de 10 Km
matriz_distancias_liebre = lapply(distancias_año_liebres, function(x)
                            {rowSums(x< set_units(10000, "m"))
                            })
names(matriz_distancias_liebre) <- años_comunes_liebre

## Distancias entre linces y conejos ####
años_comunes_conejo <- intersect(names(lista_sf_lince), names(lista_sf_conejo))

distancias_año_conejo <- lapply(años_comunes_conejo, function(año){
                          linces <- lista_sf_lince[[año]]
                          conejos <- lista_sf_conejo[[año]]
                          st_distance(linces, conejos)
                          })

# Conejos a menos de 10 Km
matriz_distancias_conejo = lapply(distancias_año_conejo, function(x)
                            {rowSums(x< set_units(10000, "m"))
                            })
names(matriz_distancias_conejo) <- años_comunes_conejo

## Unión de linces, liebres y conejos a menos de 10 Km ####
años_con_presas <- union(names(lista_sf_conejo), names(lista_sf_liebre))
años_presas_lince <- intersect(names(lista_sf_lince), años_con_presas)

distancias_finales <- lapply(años_presas_lince, function(año){
                      n_año <- length(matriz_distancias_año[[año]])
                      cercanos <- data.frame(
                      linces = matriz_distancias_año[[año]],
                      
                      liebres = if(año %in% names(matriz_distancias_liebre)) {
                      matriz_distancias_liebre[[año]] 
                      } else { rep(NA, n_año) },
  
                      conejos = if(año %in% names(matriz_distancias_conejo)) {
                      matriz_distancias_conejo[[año]] 
                      } else { rep(NA, n_año) }
                      )
                      return(cercanos)
                      })
names(distancias_finales) <- names(lista_sf_lince)

# Lista con coordenadas e individuos a menos de 10 Km
lista_final_años <- setNames(lapply(names(lista_sf_lince), function(año){
  lista_sf_lince[[año]]$linces_cerca <- distancias_finales[[año]]$linces
  lista_sf_lince[[año]]$liebres_cerca <- distancias_finales[[año]]$liebres
  lista_sf_lince[[año]]$conejos_cerca <- distancias_finales[[año]]$conejos
  return(lista_sf_lince[[año]])
}), names(lista_sf_lince))

##Agrupando por años ####
intervalos <- list("2008-2013" = as.character(c(2008:2013)),
                   "2014-2019" = as.character(c(2014:2019)),
                   "2020-2024" = as.character(c(2020:2024)))

años_20 <- as.character(c(2020:2024)) #Último intervalo para posteriores análisis

lista_final_intervalos <- lapply(intervalos, function(años){
  datos <- do.call(rbind, lista_final_años[años])
  return(datos)
})

lista_liebre_intervalos <- lapply(intervalos, function(año){
                            datos <- do.call(rbind, lista_sf_liebre[año])
                            return(datos)
                            })

lista_conejo_intervalos <- lapply(intervalos, function(año){
                            datos <- do.call(rbind, lista_sf_conejo[año])
                            return(datos)
                            })


# Mapas ####
## Mapa de España
españa <- esp_get_ccaa()

## Mapa de Portugal
portugal <- ne_countries(scale = "medium",
                         country = "Portugal",
                         returnclass = "sf") |>
  st_crop(xmin = -10, xmax = -6, ymin = 36, ymax =43)

## Distribución linces ####
mapas_linces <- lapply(names(lista_final_intervalos), function(año){
                ggplot() +
                  geom_sf(data = españa, fill = "#FFFAFA") + 
                  geom_sf(data = portugal, fill = "#FFFAFA") +
                  geom_sf(data = lista_final_intervalos[[año]], size = 0.5, color = "red") +
                  theme_minimal() + 
                  labs(title = "Linces") +
                  theme(plot.title = element_text(hjust = 0.5, size = 15))
                })

## Distribución liebre ####
mapas_liebres <- lapply(names(lista_liebre_intervalos), function(año){
                ggplot() +
                  geom_sf(data = españa, fill = "#FFFAFA") + 
                  geom_sf(data = portugal, fill = "#FFFAFA") +
                  geom_sf(data = lista_liebre_intervalos[[año]], size = 0.5, color = "red") +
                  theme_minimal() + 
                  labs(title = "Liebres") +
                  theme(plot.title = element_text(hjust = 0.5, size = 15))
                })

## Distribución conejos ####
mapas_conejos <- lapply(names(lista_conejo_intervalos), function(año){
                  ggplot() +
                    geom_sf(data = españa, fill = "#FFFAFA") + 
                    geom_sf(data = portugal, fill = "#FFFAFA") +
                    geom_sf(data = lista_conejo_intervalos[[año]], size = 0.5, color = "red") +
                    theme_minimal() + 
                    labs(title = "Conejos") +
                    coord_sf(xlim = c(-13, 5), ylim = c(35, 45)) +
                    theme(plot.title = element_text(hjust = 0.5, size = 15))
                })

(mapas_conejos[[1]] + mapas_liebres[[1]]) / mapas_linces[[1]] + plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Distribución en 2008-2013",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5)))

(mapas_conejos[[2]] + mapas_liebres[[2]]) / mapas_linces[[2]] + plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Distribución en 2014-2019",
                  theme = theme(plot.title = element_text(size = 18, hjust = 0.5)))

(mapas_conejos[[3]] + mapas_liebres[[3]]) / mapas_linces[[3]] + plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Distribución en 2020-2024",
                  theme = theme(plot.title = element_text(size = 18, hjust = 0.5)))

## Liebres cercanas ####
mapas_liebres_linces <- setNames(lapply(names(lista_final_intervalos), function(año){
  ggplot() +
    geom_sf(data = españa, fill = "#e6e6e6") +
    geom_sf(data = lista_final_intervalos[[año]], aes(fill = liebres_cerca), shape = 21, size = 2.5, alpha = 0.4) +
    scale_fill_viridis_c(option = "turbo") +
    theme_minimal() +
    labs(title = "Liebres", fill = "Liebres a menos\nde 10 Km") +
    theme(plot.title = element_text(hjust = 0.5, size = 20), legend.title = element_text(hjust = 0.5), legend.position = "left") +
    coord_sf(xlim = c(-7, -2.5), ylim = c(36, 40))
  }), names(lista_final_intervalos))

## Conejos cercanos ####
mapas_conejos_linces <- setNames(lapply(names(lista_final_intervalos), function(año){
  ggplot() +
    geom_sf(data = españa, fill = "#e6e6e6") +
    geom_sf(data = lista_final_intervalos[[año]], aes(fill = conejos_cerca), shape = 21, size = 2.5, alpha = 0.4) +
    scale_fill_viridis_c(option = "turbo") +
    theme_minimal() +
    labs(title = "Conejos", fill = "Conejos a menos\nde 10 Km") +
    theme(plot.title = element_text(hjust = 0.5, size = 20), legend.title = element_text(hjust = 0.5),) +
    coord_sf(xlim = c(-7, -2.5), ylim = c(36, 40))
  }), names(lista_final_intervalos))

(mapas_liebres_linces[[1]] + mapas_conejos_linces[[1]] +
    plot_annotation(title = "Distribución linces 2008-2013",
                    theme = theme(plot.title = element_text(size = 25, hjust = 0.5))))

(mapas_liebres_linces[[2]] + mapas_conejos_linces[[2]] +
    plot_annotation(title = "Distribución linces 2014-2019",
                    theme = theme(plot.title = element_text(size = 25, hjust = 0.5))))

(mapas_liebres_linces[[3]] + mapas_conejos_linces[[3]] +
    plot_annotation(title = "Distribución linces 2020-2024",
                    theme = theme(plot.title = element_text(size = 25, hjust = 0.5))))


#Heat maps ####

##Liebre ####
#Intervalos
lista_gráficos_liebre_intervalos <-  lapply(names(lista_final_intervalos), function(año){
  ggplot(lista_final_intervalos[[año]], aes(x= liebres_cerca, y=linces_cerca))+
    geom_point(col= "red", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    geom_density2d_filled(alpha= 0.5, show.legend = F)+
    theme_minimal()+
    labs(title =as.character(año),
         x= " ",
         y= " ")
})
wrap_plots(lista_gráficos_liebre_intervalos, nrow = 3)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5)),
    caption = "Eje X: Liebres cercanas  |  Eje Y: Linces cercanos") & 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
  )

#Años 20

lista_gráficos_liebre_años <-  lapply(años_20, function(año){
  ggplot(lista_final_años[[año]], aes(x=liebres_cerca, y=linces_cerca))+
    geom_point(col= "red", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    geom_density2d_filled(alpha= 0.5, show.legend = F)+
    theme_minimal()+
    labs(title =as.character(año),
         x= " ",
         y= " ")
})

wrap_plots(lista_gráficos_liebre_años, nrow = 2)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5)),
    caption = "Eje X: Liebres cercanas  |  Eje Y: Linces cercanos") & 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
  )


##Conejo ####
lista_gráficos_conejo_intervalos <-  lapply(names(lista_final_intervalos), function(año){
  ggplot(lista_final_intervalos[[año]], aes(x=conejos_cerca, y=linces_cerca))+
    geom_point(col= "blue", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    geom_density2d_filled(alpha= 0.5, show.legend = F)+
    theme_minimal()+
    labs(title =as.character(año),
         x= " ",
         y= " ")
})

wrap_plots(lista_gráficos_conejo_intervalos, nrow = 3)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5)),
    caption = "Eje X: Conejos cercanos  |  Eje Y: Linces cercanos") & 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
  )

#Años 20

lista_gráficos_conejo_años <-  lapply(años_20, function(año){
  ggplot(lista_final_años[[año]], aes(x=conejos_cerca, y=linces_cerca))+
    geom_point(col= "blue", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    geom_density2d_filled(alpha= 0.5, show.legend = F)+
    theme_minimal()+
    labs(title =as.character(año),
         x= " ",
         y= " ")
})

wrap_plots(lista_gráficos_conejo_años, nrow = 2)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5)),
    caption = "Eje X: Conejos cercanos  |  Eje Y: Linces cercanos") & 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
  )

# Linces en Parques Nacionales ####
#Definir Parques naturales 
enps <- st_read("Enp/Enp2025.shp")
enps$geometry
ppnn <- enps |>
        subset(FIGURA_LP %in% c("Parque Nacional","Parque Natural") &
                 CCAA_N_ENP != "Illes Balears") |>
        select(FIGURA_LP, geometry)

#Obtenemos los datos
getwd()

espana <- ne_countries(scale = "medium", country = "Spain", returnclass = "sf")

#Vemos el porcentaje de linces dentro de los parques nacionales y naturales
st_is_valid(ppnn)
ppnn <- st_make_valid(ppnn)

lista_sf_20 <- setNames(lapply(años_20, function(años){
  datos <- do.call(rbind, lista_sf_lince[años])
  return(datos)
}),años_20)

sf_20 <- do.call(rbind, lista_sf_20)

join <- st_join(sf_20, ppnn, join = st_within)

en_parque <- !is.na(join$FIGURA_LP)
porcentaje <- mean(en_parque) * 100
porcentaje

#Porcentajes en los últimos cinco años
joins <- lapply(lista_sf_20, function(x) {
  st_join(x, ppnn, join = st_within)
})

porcentajes <- sapply(joins, function(j) {
  mean(!is.na(j$FIGURA_LP)) * 100})

porcentajes

#Gráfico
dev.off()
plot(st_geometry(espana),
     main = "Distribución de linces en áreas protegidas",
     xlab = "Longitud", ylab = "Latitud",
     family = "serif", font = 2,
     xlim=c(-8, -1), ylim=c(36, 44),
     col = "#FFFAFA",
     axes = TRUE,
     lwd = 2)
grid()
plot(sf_20$geometry, pch = 19, col = adjustcolor("black", alpha.f = 0.5), add = TRUE)
plot(subset(ppnn, FIGURA_LP == "Parque Nacional"), col =  adjustcolor("red", alpha.f = 0.25), add = TRUE)
plot(subset(ppnn, FIGURA_LP == "Parque Natural"), col =  adjustcolor("yellow", alpha.f = 0.25),  add= TRUE)

text(x = 2, y = 36.3,
     labels = paste0(round(porcentaje, 1), "% en áreas protegidas"),
     adj = c(0.5,1), cex = 0.9, font = 2)

legend("bottomleft",
       legend = c("Parque Nacional", "Parque Natural", "Lince"),
       fill = c(adjustcolor("red", alpha.f = 0.25), adjustcolor("yellow", alpha.f = 0.25), NA),
       border = c("black", "black", NA),
       pch = c(NA, NA, 16),
       col = c(NA, NA, "black"))

## Histogramas de linces dentro y fuera de los parques ####
interv <- st_join(lista_final_intervalos[["2020-2024"]], ppnn, join = st_within)
interv$protegido <- !is.na(interv$FIGURA_LP)

#Histograma conejos
ggplot(interv, aes (x= conejos_cerca, fill=protegido)) +
  geom_histogram(position="identity", alpha = 0.5, bins =20) +
  scale_fill_manual(values = c("violetred", "aquamarine"),
                    labels = c("No protegido", "Protegido"),
                    name = "Situación") +
  theme_minimal() +
  labs(title = "Conejos cerca de linces",
       x = "Conejos < 10 km", y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

#Con geom density
ggplot(interv, aes (x= conejos_cerca, fill=protegido)) +
  geom_density(position="identity", alpha = 0.5) +
  xlim(0, 145) +
  scale_fill_manual(values = c("violetred", "aquamarine"),
                    labels = c("No protegido", "Protegido"),
                    name = "Situación") +
  theme_minimal() +
  labs(title = "Conejos cerca de linces",
       x = "Conejos < 10 km", y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

#Boxplot
ggplot(interv, aes (x=protegido, y= conejos_cerca)) +
  geom_boxplot(fill = c("violetred", "aquamarine"), alpha = 0.5, width=0.4) +
    scale_y_continuous(trans = "log1p") +
  scale_x_discrete(labels = c("FALSE" = "No protegido", "TRUE" = "Protegido")) +
  theme_minimal() +
  labs(x = NULL, y = "Conejos < 10 km") +
  theme(legend.position = "none")

#Histograma liebres
ggplot(interv, aes(x = liebres_cerca, fill = protegido)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  scale_fill_manual(values = c("violetred", "aquamarine"),
                    labels = c("No protegido", "Protegido"),
                    name = "Situación") +
  theme_minimal() +
  labs(title = "Liebres cerca de linces",
       x = "Liebres < 10 km", y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

#Con geom_density
ggplot(interv, aes (x= liebres_cerca, fill=protegido)) +
  geom_density(position="identity", alpha = 0.5) +
  xlim(0,20) +
  scale_fill_manual(values = c("violetred", "aquamarine"),
                    labels = c("No protegido", "Protegido"),
                    name = "Situación") +
  theme_minimal() +
  labs(title = "Liebres cerca de linces",
       x = "Liebres < 10 km", y = "Frecuencia") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12))

#Boxplot
ggplot(interv, aes (x=protegido, y= liebres_cerca)) +
  geom_boxplot(fill = c("violetred", "aquamarine"), alpha = 0.5, , width=0.4) +
  scale_y_continuous(trans = "log1p") +
  scale_x_discrete(labels = c("FALSE" = "No protegido", "TRUE" = "Protegido")) +
  theme_minimal() +
  labs(x = NULL, y = "Liebres < 10 km") +
  theme(legend.position = "none")
