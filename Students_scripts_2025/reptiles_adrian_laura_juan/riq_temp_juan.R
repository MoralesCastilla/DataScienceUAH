
# Establecemos el directorio de trabajo

setwd("~/GitHub/prac_reptiles/1_temp")


# Rep_traits ####
# Llamamos a las librerías que vamos a usar

library(tidyverse)



# Leemos y visualizamos los datos de temperatura

datos_temp_clean <- read.csv("~/GitHub/prac_reptiles/1_temp/1_data/datos_temp_clean.csv", sep = ",", dec = ".")[,-1]

glimpse(datos_temp_clean)



## Hacemos plots ####

# Relacionamos la temperatura ambiental y la temperatura corporal:

ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb)) +
  geom_point() +
  geom_smooth(method = "lm")

# Existe una relación positiva entre la temperatura media ambiental y corporal



# Añadimos barras de error:

datos_temp_clean <- mutate(datos_temp_clean, 
                           meantemp_max = mean_temp + temp_seasonality/100,
                           meantemp_min = mean_temp - temp_seasonality/100)
x11()
ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, col = family)) +
  geom_point() +
  geom_smooth(method = "lm")  +
  geom_errorbar(aes(ymin = min_tb, ymax = max_tb), width = .3, colour = "red", alpha = .2) +
  geom_errorbar(aes(xmin = meantemp_min, xmax = meantemp_max), width = .3, colour = "orange", alpha = .4)

ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, col = order)) +
  geom_point() +
  geom_smooth(method = "lm")  +
  geom_errorbar(aes(ymin = min_tb, ymax = max_tb), width = .3, colour = "red", alpha = .2) +
  geom_errorbar(aes(xmin = meantemp_min, xmax = meantemp_max), width = .3, colour = "orange", alpha = .4)

ggplot(datos_temp_clean, aes(x = mean_temp, y = mean_tb, col = bio_region)) +
  geom_point() +
  geom_smooth(method = "lm")  +
  geom_errorbar(aes(ymin = min_tb, ymax = max_tb), width = .3, colour = "red", alpha = .2) +
  geom_errorbar(aes(xmin = meantemp_min, xmax = meantemp_max), width = .3, colour = "orange", alpha = .4)

datos_reptiles <- read.csv("datos_reptiles_full.csv", sep = ";", dec = ",")

glimpse(datos_reptiles)





# Nos quedamos solo con las columnas que necesitamos (también les cambiamos los nombres)

colnames(datos_reptiles) # Para ver los nombres de las variables/columnas

datos_temp <- datos_reptiles %>% 
  select(c(Species, Family, Order, Main.biogeographic.region, Minimal.elevation..m., Maximum.elevation..m., Mean.Annual.Temperature...C.,
           Temperature.Seasonality..standard.deviation.100., Mean.Tb, Minimum.Tb, Maximum.Tb)) %>% 
  rename(species = Species, order = Order, family = Family, bio_region = Main.biogeographic.region, min_elevation = Minimal.elevation..m., max_elevation = Maximum.elevation..m.,
         mean_temp = Mean.Annual.Temperature...C., temp_seasonality = Temperature.Seasonality..standard.deviation.100.,
         mean_tb = Mean.Tb, min_tb = Minimum.Tb, max_tb = Maximum.Tb)

# write.csv(datos_temp, "~/GitHub/prac_reptiles/0_data_exp/1_data/datos_temp.csv") # Guardamos el dataframe



# Quitamos los NA's

datos_temp_clean <- na.omit(datos_temp)

glimpse(datos_temp_clean)

which(is.na(datos_temp_clean), arr.ind=TRUE) # Buscamos NA's para asegurarnos de que ha funcionado :)

write.csv(datos_temp_clean, "~/GitHub/prac_reptiles/1_temp/1_data/datos_temp_clean.csv")


mean_lat <-read.csv("1_data/mean_lat_rep2000.csv")

?merge

datos_tem_lat <- merge( x = datos_temp_clean, y = mean_lat )
datos_tem_lat$dif_temp <- datos_temp_clean$mean_tb - datos_temp_clean$mean_temp

x11()


ggplot(datos_tem_lat, aes(x = abs(latmedia), y = dif_temp)) +
  
  geom_point(aes(shape = family, colour = order)) +
  scale_shape_manual(values=c(1,2,3,4,5,6,7)) +
  
  geom_smooth(method = "lm", aes(color = order)) +
  scale_colour_manual(values=c("darkgreen", "darkblue", "darkred"))

# Con altitud

ggplot(datos_tem_lat, aes(x = max_elevation - min_elevation, y = dif_temp)) +
  
  geom_point(aes(shape = family, colour = order)) +
  scale_shape_manual(values=c(1,2,3,4,5,6,7)) +
  
  geom_smooth(method = "lm", aes(color = order)) +
  scale_colour_manual(values=c("darkgreen", "darkblue", "darkred"))
 

dev.off

# Con latitud

ggplot(datos_tem_lat, aes(x = abs(latmedia), y = dif_temp)) +
  
  geom_point(aes(shape = order, colour = bio_region), cex = 3) +
  scale_shape_manual(values=c(1,2,3,4,5)) +
  
  geom_smooth(method = "lm", aes(color = bio_region)) +
  scale_colour_manual(values=c("darkgreen", "darkblue", "darkred", "black", "darkorange", "yellow", "salmon"))






# Código para crear el gráfico de riqueza ####

library(ggplot2)
library(terra) # Asegúrate de que esté cargado
library(sf)    # Asegúrate de que esté cargado
library(geodata)
library(tidyverse)
library(dplyr)


## Matriz presencia ausencia ####

# 1. Cargar y preparar los datos
long_lat_rep5000 <- read.csv("C:\\Users\\julia\\OneDrive\\Escritorio\\Documents\\Github\\prac_reptiles\\1_temp\\1_data\\long_lat_rep5000.csv", sep = ",", dec = ".")[,-1]
long_lat_rep <- long_lat_rep5000 |>
  rename(sciname = "scientificName", x = "decimalLongitude", y = "decimalLatitude") |>
  na.omit()

# Renombrar las columnas para mayor claridad.
names(long_lat_rep) <- c("species", "longitude", "latitude")

# 2. Crear la matriz de presencia-ausencia
library(dplyr)
library(tidyr)
presencia_ausencia <- long_lat_rep %>%
  distinct(longitude, latitude, species) %>% # Para eliminar duplicados por ubicación y especie
  pivot_wider(id_cols = c(longitude, latitude), names_from = species, values_from = species,
              values_fn = function(x) ifelse(length(x) > 0, 1, 0), values_fill = 0)

# A. Definimos las coordenadas de las esquinas del rango global deseado
esquinas_globales <- tribble(
  ~longitude, ~latitude,
  -180, -90,
  -180,  90,
  180, -90,
  180,  90)

# B. Obtenemos las coordenadas únicas ya presentes en tu matriz 'presencia_ausencia'
coordenadas_actuales <- presencia_ausencia %>%
  select(longitude, latitude)

# C. Creamos un data frame que contenga las coordenadas 
#    actuales más las esquinas globales. distinct() asegura que no haya duplicados
#    si alguna esquina ya existía en los datos originales.
todas_coordenadas <- bind_rows(coordenadas_actuales, esquinas_globales) %>%
  distinct(longitude, latitude)

# D. Realiza un 'right_join'. Esto asegura que todas las filas de 'todas_coordenadas'
#    estén presentes en el resultado. Las filas que solo estaban en 'todas_coordenadas'
#    (es decir, las esquinas que no estaban en los datos originales) tendrán NA
#    en las columnas de las especies.
presencia_ausencia_extendida <- right_join(presencia_ausencia, todas_coordenadas, by = c("longitude", "latitude"))

# E. Reemplazamos los valores NA (introducidos por el join para las nuevas filas de las esquinas)
#    por 0 (ausencia) en todas las columnas de especies.
#    Identificamos las columnas de especies como todas aquellas que NO son 'longitude' o 'latitude'.
columnas_especies <- setdiff(names(presencia_ausencia_extendida), c("longitude", "latitude"))
presencia_ausencia_extendida <- presencia_ausencia_extendida %>%
  mutate(across(all_of(columnas_especies), ~ ifelse(is.na(.), 0, .)))

# F. Sobrescribimos la variable original 'presencia_ausencia' con la versión extendida.

presencia_ausencia <- presencia_ausencia_extendida


# 3. Calculamos la riqueza de especies 
presencia_ausencia <- presencia_ausencia %>%
  mutate(species_richness = rowSums(select(., -longitude, -latitude), na.rm = TRUE))

# 4. Preparamos los datos para el mapa
map_data <- presencia_ausencia %>%
  select(longitude, latitude, species_richness)

## Gráficos ####

### Mapa de puntos superpuestos ####

library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr) # Asegúrate de que dplyr esté cargado

world <- ne_countries(scale = "medium", returnclass = "sf")

# Hay que tener la columna 'riqueza_cat' como factor ordenado
map_data <- map_data %>%
  mutate(riqueza_cat = cut(species_richness, breaks = c(0, 2, 4, 6, 8, 10, Inf),
                           labels = c("0-1", "2-3", "4-5", "6-7", "8-9", ">10"), right = FALSE)) %>%
  mutate(riqueza_cat = factor(riqueza_cat, levels = c("0-1", "2-3", "4-5", "6-7", "8-9", ">10")))

# Definimos los tamaños y colores para cada categoría
tamanios_riqueza <- c(0.10, 0.848, 1.686, 2.524, 3.362, 4.200, 5.000)
colores_riqueza <- c("green", "darkgreen", "yellow", "orange", "red", "darkred")
nombres_riqueza <- levels(map_data$riqueza_cat)
x11()
mapa_puntos_superpuestos <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +
  # Iteramos sobre cada categoría de riqueza y dibujamos los puntos correspondientes
  lapply(1:length(nombres_riqueza), function(i) {
    geom_point(data = filter(map_data, riqueza_cat == nombres_riqueza[i]),
               aes(x = longitude, y = latitude, color = riqueza_cat),
               size = tamanios_riqueza[i],
               alpha = 1) 
    # Lapply lo que hace es ordenar las capas según su riqueza, por lo que al 
      #superponer capas las de mayor riqueza quedan sobre las de menor riqueza.
      #(Aplica una función)
  }) +
  geom_sf(data = world, fill = "transparent", color = "black") + # Ponemos 2 veces geom_sf 
    #porque así tenemos el fondo blanco sobre el que poner los puntos mientras que esta 
    #segunda vez superpone las líneas del mapa en caso de que se tapen. 
    #Ésto va a ser más evidente luego en el mapa de densidad
  scale_color_manual(values = setNames(colores_riqueza, nombres_riqueza), name = "Riqueza") +
  scale_size_manual(values = setNames(tamanios_riqueza, nombres_riqueza), name = "Riqueza") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = seq(-180, 180, by = 30), labels = scales::number) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30), labels = scales::number) +
  labs(title = "Riqueza de Especies", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.background = element_rect(fill = "aliceblue")) +
  guides(alpha = "none")

mapa_puntos_superpuestos
setwd("~/GitHub/prac_reptiles/1_temp")
ggsave("puntos_superpuestos.pdf", width=50, height=25, unit="cm", dpi=100000)



### Mapa densidad ####

library(ggplot2)
library(sf)
library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf")
x11()
mapa_dens <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") + #mapa mundo
  stat_density_2d(data = map_data,
                  aes(x = longitude, y = latitude, fill = after_stat(density)),
                  geom = "raster", contour = FALSE, n = 2000) +
  scale_fill_gradientn(colors = c("transparent", "green", "darkgreen", "yellow","yellow4", 
                                  "orange", "red", "darkred", "darkviolet"),
                       values = c(0, 0.00014, 0.0054, 0.14, 0.29, 0.43, 0.57, 0.71, 0.86, 1),
                       name = "Densidad",
                       limits = c(0, NA)) + # Establecer límite inferior en 0
  geom_sf(data = world, fill = "transparent", color = "black") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_x_continuous(breaks = seq(-180, 180, by = 30), labels = scales::number) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30), labels = scales::number) +
  labs(title = "Mapa de Densidad de Ubicaciones de Riqueza de Especies", x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.background = element_rect(fill = "aliceblue"))
mapa_dens

setwd("~/GitHub/prac_reptiles/1_temp")
ggsave("mapa_densidad.pdf", width=50, height=25, unit="cm", dpi=100000)

# Guardar mapas: ####

setwd("~/GitHub/prac_reptiles/1_temp")

pdf("Graph.pdf", width=10/2.54, height=20/2.54)

#ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) +
#  geom_point(size=3) +
#  scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))

    #( Poner el código del gráfico donde va lo verde )

dev.off()
# Hasta que no se le da a dev.off no lo guarda


## También se puede usar lo siguiente para guardar la última imagen generada ####
 #en varios posibles formatos.

ggsave("linda3.pdf", width=8, height=8, unit="cm", dpi=300)
ggsave("linda3.tiff", width=8, height=8, unit="cm", dpi=300)
ggsave("linda3.jpg", width=8, height=8, unit="cm", dpi=300)
ggsave("linda3.png", width=8, height=8, unit="cm", dpi=300)


library(cowplot)
save_plot("graph.jpg", mapa_dens, base_asp = 100/2.54, base_height = 50/2.54,)

# Mapa densidad.


ggsave("mapa_dens.pdf", width=100, height=50, unit="cm", dpi=400)


# Relación riqueza-Tª####

library(rgbif)
library(raster)
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
library(viridis) 
library(scales)
# Cargamos el raster con las temperaturas medias diarias.
setwd("C:\\Users\\julia\\Downloads")
Daily_T <- rast("air.mon.v401.ltm.1981-2010.nc")
setwd("~/GitHub/prac_reptiles/1_temp")

# Para ver un poco cómo está organizado.
head(Daily_T)
nlyr(Daily_T)
time(Daily_T)

mean_T <- mean(Daily_T)
nlyr(mean_T)

terra::ext(Daily_T)
# Vemos el sistema de coordenadas, es 0:360 por lo que tenemos que cambiarlo a -180:180

Daily_T_r <- terra::rotate(Daily_T) # Cambia de 0:360 a -180:180
mean_T_r <- mean(Daily_T_r)
# Convertimos mean_T en dataframe para usar ggplot:

mean_T_df <- as.data.frame(mean_T_r, xy = TRUE)


# Gráfico
colores <- c("darkviolet", "darkblue", "blue","turquoise", "lightblue",
               "lightgreen", "green", "darkgreen", "yellow",
               "yellow4", "orange", "red", "darkred")

world <- ne_countries(scale = "medium", returnclass = "sf")
x11()
ggplot()+
  # Dibuja primero el raster de temperatura (como fondo)
  geom_tile(data = mean_T_df,
              aes(x = x, y = y, fill = mean)) + # 'x', 'y' son longitud y latitud; 'mean' es el valor del píxel
  
  # Aplica una escala de color para el relleno del raster
  scale_fill_gradientn(colors = colores, name = "Temperatura",
                       na.value = "transparent")+
  # Dibuja el mapa mundial encima (con relleno transparente para ver el raster de fondo)
  geom_sf(data = world, fill = "transparent", color = "black") +
  coord_fixed() +
  # Ajusta el sistema de coordenadas del mapa
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = TRUE) +
  
  
  # Ajusta las etiquetas de los ejes X e Y
  scale_x_continuous(breaks = seq(-180, 180, by = 30), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30), labels = scales::number_format(accuracy = 1)) +
  
  # Añade títulos y etiquetas
  labs( x = "Longitud", y = "Latitud") +
  
  # Estilo del tema
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.background = element_rect(fill = "aliceblue"))
    
setwd("~/GitHub/prac_reptiles/1_temp")
ggsave("T_media_global.pdf", width=50, height=25, unit="cm", dpi=1000000)

  ## Unir  para no contar 2 veces la misma riqueza ####
  
  library(terra) # Para el manejo de rasters
  library(sf)    # Para trabajar con datos espaciales
  library(dplyr) # Para manipulación de datos (agrupación y resumen)
  
  # --- 1. Definimos la nueva cuadrícula (raster) ---
  target_grid_template <- terra::rast(
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90,
    ncol = 2000, nrow = 1000,
    crs = "EPSG:4326")
  
  # --- 2. Calculamos la riqueza de especies única por celda ---
  
  # 2.1. Obtenemos los IDs de celda para cada punto en 'presencia_ausencia'
  # El argumento 'xy' de cellFromXY espera una matriz (por eso usamos as.matrix)
  cell_ids <- terra::cellFromXY(target_grid_template, as.matrix(presencia_ausencia[, c("longitude", "latitude")]))
  
  # 2.2. Combinamos los datos de presencia/ausencia con los IDs de celda
  # y filtramos cualquier punto que caiga fuera de la cuadrícula o en NA
  presencia_ausencia_con_celdas <- presencia_ausencia %>%
    dplyr::mutate(cell_id = cell_ids) %>%
    dplyr::filter(!is.na(cell_id)) # Elimina puntos que no caen en ninguna celda definida

  
  # 2.3. Identificar las columnas que contienen los datos de especies (1s o 0s)
  # Asumiendo que 'longitud' y 'latitud' son las dos primeras columnas, y el resto son especies.
  species_column_names <- names(presencia_ausencia_con_celdas)[3:(ncol(presencia_ausencia_con_celdas) - 1)] # Excluye longitud, latitud y cell_id
  
  
  # 2.4. Agrupar por 'cell_id' y calcular la riqueza única por celda
  # Para cada especie, verificamos si hay al menos un '1' (presencia) dentro de esa celda.
  # Si hay un '1', esa especie se cuenta como presente en esa celda.
  # Luego sumamos el número total de especies presentes por celda.
  riqueza_por_celda_df <- presencia_ausencia_con_celdas %>%
    dplyr::group_by(cell_id) %>%
    dplyr::summarise(
      riqueza = sum(
        # 'across' permite aplicar una función a múltiples columnas.
        # 'all_of' asegura que solo las columnas de especies definidas sean usadas.
        # 'any(. == 1)' devuelve TRUE si al menos un 1 está presente en la columna de esa especie para el grupo de la celda.
        dplyr::across(
          .cols = dplyr::all_of(species_column_names),
          .fns = ~ any(. == 1)), # Cuenta como 1 si la especie está presente en la celda
        na.rm = TRUE # Asegura que si alguna especie tiene NA, no afecte la suma.
      )
    ) %>%
    dplyr::ungroup()
  
  # 2.5. Crear un SpatRaster a partir de los datos de riqueza por celda
  # Primero, inicializamos un raster vacío con la misma estructura que nuestro template (la cuadrícula que hemos creado)
  riqueza_raster <- target_grid_template
  # Asignamos NA a todos los valores inicialmente
  terra::values(riqueza_raster) <- NA
  # Asignamos los valores de riqueza calculados a las celdas correspondientes
  terra::values(riqueza_raster)[riqueza_por_celda_df$cell_id] <- riqueza_por_celda_df$riqueza
  
  # --- 3. Remuestreamos la temperatura (mean_T) a la nueva cuadrícula ---
  temperatura_raster <- terra::resample(mean_T, target_grid_template, method = "average")
  
  # --- 4. Combinamos los dos rasters en uno multi-capa ---
  combined_raster <- c(temperatura_raster, riqueza_raster)
  names(combined_raster) <- c("temperatura", "riqueza")
  
  # --- 5. Convertimos el SpatRaster combinado a un data.frame ---
  final_df_combinado <- as.data.frame(combined_raster, xy = TRUE)
  names(final_df_combinado)[1:2] <- c("longitud", "latitud")
  final_df_combinado <- na.omit(final_df_combinado)
  
  
  
  
  
  # Relación Tª-riqueza´gráfico
  
  
  ggplot(final_df_combinado,aes(x = temperatura, y = riqueza)) +
    
    geom_point()+
    
    geom_smooth(method = "gam")
    
  setwd("~/GitHub/prac_reptiles/1_temp")
  ggsave("R/T.png", width=24, height=24, unit="cm", dpi=300)

  col_riq<- c("transparent", "green", "darkgreen", "yellow","yellow4", "orange", "red", "darkred", "darkviolet")

  
  
###############################################################################
  
  library(ggplot2)
  library(scales) # Para scales::number_format
  library(dplyr)
  
  # Utilizamos la IA de Google (Gémini) para que nos explicase cómo ajustar a 
    #partir de unos datos un modelo siguiendo una curva de Gauss
  
  # --- 1. Definir la función Gaussiana ---
  # Esta función representa la curva de campana de Gauss.
  # temp:  variable independiente (temperatura)
  # A:  riqueza máxima (altura del pico)
  # mu:  temperatura óptima (posición del pico)
  # sigma:  tolerancia/dispersión (ancho de la campana)
  gaussian_model <- function(temp, A, mu, sigma) {
    A * exp(- (temp - mu)^2 / (2 * sigma^2))
  }
  
  # --- 2. Preparar estimaciones iniciales para nls() ---
  # ¡ESTO ES CRÍTICO! NLS necesita buenas estimaciones iniciales para converger.
  # DEBES ADAPTAR ESTOS VALORES A LO QUE VEAS EN TU GRÁFICO DE DISPERSIÓN REAL.
    # Con ensayo error, hasta que obtuvimos un resultado por parte de nls(), 
    #por suerte no costó mucho y solo hubo que cambiar en la aproximación de sigma en vez de /4 poner /3.
  
  # Estima A: El valor más alto de riqueza que observes.
  A_start <- max(final_df_combinado$riqueza, na.rm = TRUE)
  
  # Estima mu: La temperatura donde la riqueza parece ser más alta.
  # Puedes usar un valor medio del rango de temperatura, o el valor de temp donde la riqueza es max.
  mu_start <- final_df_combinado$temperatura[which.max(final_df_combinado$riqueza)]
  
  # Estima sigma: Relacionado con el ancho de la campana. Un buen punto de partida es un cuarto
  # o la mitad del rango total de tu variable independiente.
  sigma_start <- diff(range(final_df_combinado$temperatura, na.rm = TRUE)) / 3
  if (sigma_start == 0) sigma_start <- 1 # Evitar división por cero si el rango es 0
  
  initial_params <- list(A = A_start, mu = mu_start, sigma = sigma_start)
  
  cat("Estimaciones iniciales para nls():\n")
  print(initial_params)
  cat("Si nls falla, ajusta estos valores manualmente basándote en tu gráfico.\n\n")
  
  # --- 3. Ajustar el modelo Gaussiano con nls() ---
  fit_gaussian <- tryCatch({
    nls(riqueza ~ gaussian_model(temperatura, A, mu, sigma),
        data = final_df_combinado,
        start = initial_params,
        control = nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/2048) # Ajustes para mejor convergencia
    )
  }, error = function(e) {
    message("Error al ajustar el modelo Gaussiano: ", e$message)
    message("Las estimaciones iniciales (A, mu, sigma) pueden ser inadecuadas o el modelo no converge.")
    message("Intenta visualizarlos en tu gráfico de dispersión y ajusta 'initial_params'.")
    return(NULL) # Devuelve NULL si el ajuste falla
  })
  
 # Con este chunk de código en caso de error porque nls() no fuera capaz de 
  #calcular las cosas porque no estuviera hecha bien la aproximación que nos pedía
  
  # --- 4. Extraer coeficientes y calcular pseudo R-cuadrado si el modelo se ajustó ---
  if (!is.null(fit_gaussian)) {
    coefs_gaussian <- coef(fit_gaussian)
    A_est <- coefs_gaussian["A"]
    mu_est <- coefs_gaussian["mu"]
    sigma_est <- coefs_gaussian["sigma"]
    
    # Calcular el pseudo R-cuadrado (para modelos no lineales)
    # Mide la proporción de la varianza en riqueza explicada por el modelo.
    ss_total <- sum((final_df_combinado$riqueza - mean(final_df_combinado$riqueza, na.rm = TRUE))^2, na.rm = TRUE)
    ss_residual <- sum(residuals(fit_gaussian)^2, na.rm = TRUE)
    pseudo_r_squared <- 1 - (ss_residual / ss_total)
    
    cat("\nCoeficientes del modelo Gaussiano ajustado:\n")
    print(coefs_gaussian)
    cat(sprintf("Pseudo R-cuadrado: %.3f\n", pseudo_r_squared))
    
    # --- 5. Construir la cadena de la fórmula para el plot ---
    # Usamos sprintf para formatear los números y plotmath para la expresión matemática.
    formula_label <- sprintf(
      "italic(Riqueza) == %.2f * exp(- (italic(Temperatura) - %.2f)^2 / (2 * %.2f^2))",
      A_est, mu_est, sigma_est
    )
    r_squared_label <- sprintf("italic(R)^2 == %.2f", pseudo_r_squared)
    
    # Con esto, conseguimos que en el própio gráfico del modelo escriba la fórmula con notación matemática.
    
    # Combina las etiquetas usando la sintaxis de plotmath para apilar (`atop`)
    etiqueta_final <- paste0(
      formula_label,
      " ~ atop('', ", r_squared_label, ")" # Corregido para que 'atop' sea parseado correctamente
    )
    
    
    # Gráfico relación Temperatura y Riqueza.
   x11()
     ggplot(final_df_combinado, aes(x = temperatura, y = riqueza)) +
      geom_point(alpha = 0.6) + # Tus puntos de datos
      
      # stat_function dibuja una función
      stat_function(
        fun = gaussian_model,
        args = as.list(coefs_gaussian), # Pasa los coeficientes estimados al modelo
        color = "blue",
        linewidth = 1.2
      ) +
      
      # Añadir la fórmula y el R-cuadrado al gráfico
      annotate("text",
               x = Inf, y = Inf, # Posición (esquina superior derecha)
               label = etiqueta_final,
               hjust = 1.05, vjust = 1.05, # Ajustar la justificación
               parse = TRUE, # ¡Crucial para interpretar la cadena como expresión matemática!
               color = "black",
               size = 4.5) +
      
      labs(title = "Ajuste de Riqueza con una Curva Gaussiana",
           x = "Temperatura",
           y = "Riqueza de Especies") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white", color = NA)) # Centrar el título
    
  } else {
    # Si el ajuste del modelo falló, solo muestra el gráfico de dispersión con un mensaje
    ggplot(final_df_combinado, aes(x = temperatura, y = riqueza)) +
      geom_point(alpha = 0.6) +
      labs(title = "No se pudo ajustar el modelo Gaussiano",
           subtitle = "Revisa tus datos o las estimaciones iniciales de los parámetros",
           x = "Temperatura",
           y = "Riqueza de Especies") +
      theme_minimal()
  }
  
  setwd("~/GitHub/prac_reptiles/1_temp")
  ggsave("Gauss-formula.png", width=24, height=24, unit="cm", dpi=300)
  
  
 # Mapas T/R #### 

  cols_riq <- c("transparent", "transparent", "green", "yellow", "red", "darkred")
  col_breaks <- c(0, 1, 2, 3, 4, 5)
  
  
   x11() 
 
 ggplot() +
    # Dibuja el raster de temperatura (como fondo)
    geom_tile(data = mean_T_df,
              aes(x = x, y = y,
                  fill = 4.11 * exp(-((mean  - 22.71)^2) / (25.67^2)))) + # La fórmula de riqueza que hemos calculado
    
    # Aplica una escala de color personalizada para el relleno del raster
    scale_fill_gradientn(
      colors = cols_riq, 
      values = scales::rescale(col_breaks), # Escala los valores a [0,1] para el gradiente
      name = "Riqueza", 
      na.value = "transparent", # Mantiene los NA's transparentes
      limits = c(0, 5), # Fuerza el rango de la escala de color de 0 a 5
      oob = scales::squish # Los valores fuera de 0-5 se "aplastan" al color más cercano (0 o 5)
    ) +
    # Dibuja el mapa mundial encima (con relleno transparente para ver el raster de fondo)
    geom_sf(data = world, fill = "transparent", color = "black") +
    
    # Ajusta el sistema de coordenadas del mapa
    coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE, crs = "EPSG:4326") + # Asumiendo WGS84
    
    # Ajusta las etiquetas de los ejes X e Y
    scale_x_continuous(breaks = seq(-180, 180, by = 30), labels = scales::number_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), labels = scales::number_format(accuracy = 1)) +
    
    # Añade títulos y etiquetas
    labs(x = "Longitud", y = "Latitud") +
    
    # Estilo del tema
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
          panel.background = element_rect(fill = "aliceblue"),
          panel.border = element_blank()) # Para asegurar que no haya línea negra exterior 
  
  
  
  setwd("~/GitHub/prac_reptiles/1_temp")
  ggsave("T-Rreal.pdf", width=50, height=25, unit="cm", dpi=400000)

  
  
  
  
  
ggplot() +
    # Dibuja el raster de temperatura (como fondo)
    geom_tile(data = mean_T_df,
              aes(x = x, y = y,
                  fill = 4.11 * exp(-((mean +5 - 22.71)^2) / (25.67^2)))) + # Tu fórmula de riqueza
    
    # Aplica una escala de color personalizada para el relleno del raster
    scale_fill_gradientn(
      colors = cols_riq, # Tu vector de colores
      values = scales::rescale(col_breaks), # Escala los valores a [0,1] para el gradiente
      name = "Riqueza", # Nombre de la leyenda
      na.value = "transparent", # Mantiene los NA's transparentes
      limits = c(0, 5), # Fuerza el rango de la escala de color de 0 a 5
      oob = scales::squish # Los valores fuera de 0-5 se "aplastan" al color más cercano (0 o 5)
    ) +
    # Dibuja el mapa mundial encima (con relleno transparente para ver el raster de fondo)
    geom_sf(data = world, fill = "transparent", color = "black") +
    
    # Ajusta el sistema de coordenadas del mapa
    coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE, crs = "EPSG:4326") + # Asumiendo WGS84
    
    # Ajusta las etiquetas de los ejes X e Y
    scale_x_continuous(breaks = seq(-180, 180, by = 30), labels = scales::number_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), labels = scales::number_format(accuracy = 1)) +
    
    # Añade títulos y etiquetas
    labs(x = "Longitud", y = "Latitud") +
    
    # Estilo del tema
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
          panel.background = element_rect(fill = "aliceblue"),
          panel.border = element_blank())
  
  setwd("~/GitHub/prac_reptiles/1_temp")
  ggsave("T-R+5.pdf", width=50, height=25, unit="cm", dpi=4000000)

  
  ## Mapa diferencia ####

  library(ggplot2)
  library(scales)
  library(sf)
  library(dplyr)
  
  # Carga un mapa mundial de ejemplo (si no lo tienes ya)
  # install.packages("rnaturalearth")
  library(rnaturalearth)
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  
  # --- 1. Calcular la riqueza predicha para AMBOS escenarios en tu data.frame ---
  
  # **Fórmula de Riqueza del Escenario 1 (con +5 en la media)**
  riqueza_scenario1_func <- function(temp_mean) {
    # Tu primera fórmula, como la tenías en el ggplot original
    4.11 * exp(-((temp_mean + 5 - 22.71)^2) / (25.67^2))
  }
  
  # **Fórmula de Riqueza del Escenario 2 (la que acabas de proporcionar, sin +5)**
  riqueza_scenario2_func <- function(temp_mean) {
    # Tu segunda fórmula
    4.11 * exp(-((temp_mean - 22.71)^2) / (25.67^2))
  }
  
  
  mean_T_df <- mean_T_df %>%
    mutate(
      riqueza_scenario1 = riqueza_scenario1_func(mean),
      riqueza_scenario2 = riqueza_scenario2_func(mean)
    )
  
  # --- 2. Calcular la diferencia entre las dos riquezas predichas ---
  # La diferencia (Escenario 1 - Escenario 2)
  mean_T_df <- mean_T_df %>%
    mutate(diferencia_riqueza = riqueza_scenario1 - riqueza_scenario2)
  
  # --- 3. Definir la escala de color para la diferencia ---
  # Para diferencias, lo ideal es una escala divergente (ej. de rojo a azul, con blanco en el cero).
  # `max_abs_diff` ayuda a centrar el cero.
  max_abs_diff <- max(abs(mean_T_df$diferencia_riqueza), na.rm = TRUE)
  diff_limits <- c(-max_abs_diff, max_abs_diff) # Límites simétricos alrededor de cero
  
  # Colores para la diferencia: de rojo (negativo) a azul (positivo)
  # Puedes ajustar estos colores a tu gusto.
  diff_colors <- c("darkblue", "lightblue", "white", "red", "darkred")
  
  # Valores que corresponden a esos colores (de -max_abs_diff a 0 a +max_abs_diff)
  # Los valores intermedios ayudan a que el gradiente sea suave.
  diff_values <- scales::rescale(c(diff_limits[1],
                                   diff_limits[1] / 2, # Un punto intermedio en el lado negativo
                                   0,                   # El centro (cero)
                                   diff_limits[2] / 2,  # Un punto intermedio en el lado positivo
                                   diff_limits[2]),
                                 to = c(0, 1))
  
  # --- 4. Graficar el mapa de la diferencia de riqueza ---
  ggplot() +
    # Dibuja el raster de la diferencia de riqueza
    geom_tile(data = mean_T_df,
              aes(x = x, y = y, fill = diferencia_riqueza)) +
    
    # Aplica la escala de color divergente para la diferencia
    scale_fill_gradientn(
      colors = diff_colors,
      values = diff_values,
      name = "Diferencia\nde Riqueza", # Nombre para la leyenda
      na.value = "transparent",
      limits = diff_limits, # Límites simétricos para centrar el cero
      oob = scales::squish # "Aplasta" valores fuera de los límites a los colores de los extremos
    ) +
    # Dibuja el mapa mundial encima (con relleno transparente para ver el raster de fondo)
    geom_sf(data = world, fill = "transparent", color = "black") +
    
    # Ajusta el sistema de coordenadas del mapa
    coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE, crs = "EPSG:4326") +
    
    # Ajusta las etiquetas de los ejes X e Y
    scale_x_continuous(breaks = seq(-180, 180, by = 30), labels = scales::number_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 30), labels = scales::number_format(accuracy = 1)) +
    
    # Añade títulos y etiquetas
    labs(title = "Diferencia de Riqueza Predicha (Curva Desplazada - Curva Original)",
         x = "Longitud", y = "Latitud") +
    
    # Estilo del tema
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
      panel.border = element_blank() # Elimina el borde alrededor del panel
    )

  setwd("~/GitHub/prac_reptiles/1_temp")
  ggsave("difT-R.pdf", width=50, height=25, unit="cm", dpi=4000000)
 