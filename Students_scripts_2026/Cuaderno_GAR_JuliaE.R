#################################################
##
##      CUADERNO GAR - Julia Escribano Elvira
##
#################################################

library(tidyverse)

#día 1 - inicio ####
mi_vector <- c(2, 3, 4, 5)

#nuestro primer grafico
#install.packages("tidyverse")
library(tidyverse) 
data(diamonds)
write.csv(diamonds, "01_data/diamonds.csv")
str(diamonds)

ggplot(diamonds, aes(carat, price)) +
  geom_hex()

ggsave("03_resultados/diamonds.pdf")

#día 2 - conceptos básicos programación ####

#buscar ayuda de una determinada función
?read.table
?write.table

#R como calculadora
mi_primer_objeto <- c(9*10, 10/3, 10 + 5)
mean(mi_primer_objeto)

#guardamos la tabla en nuestro orfanor de tipo .txt y con separador de tabuladores
write.table(mi_primer_objeto,
            file = "03_resultados/mi_primer_objeto.txt", 
            sep = "\t", dec = ",",
            qmethod = "double")

#escribir una función que sume tres números que sean a,b,c
sumar <- function(a,b,c){
   a + b + c
  }
sumar(1, 4, 7)
sumar(a = 1, b = 4, c = 7)
#escribir una función que sume todas las posiciones de un vector numérico doble

###vamos a crear un data.frame de animales salvajes

#variable "id" que sea el identificador del animal 
#numero discreto del 1 al 1000
id <- 1:1000
str(id)
glimpse (id)
length(id)
class(id)
names(id)
summary(id) #las estadisticas

#variable que sea la altura que sea un sequencia (seq)
#de 0,5 a 4 metros con 100 posiciones
#que se repita 10 veces (rep)
alt <- seq(0.5, 4, length.out = 100)

altura <- rep(alt, times=10)
str(altura)
summary(altura)

#peso que sean los kgs del animal que
#tenga una distrbibución normal de media  
#40 kgs  y una desviación estándar de 10 con rnorm()
peso <- rnorm(1000, mean = 40, sd = 10)
str(peso)
summary(peso)

#siete especies
set.seed(123) #para que el resultado sea reproducible
especies <- c("rana","serpiente","triceratops","cuervo",
              "jirafa", "caracol", "perro") #genero mis especies

#usando la función sample generar un vector con las especies muestreadas
especies <- factor(sample(especies, 1000, replace = TRUE)) 

str(especies)
summary(especies)

#día 3 ####
df_salvaje <- data.frame(id, altura, peso, especies)

head(peso)
peso [4] #selecciona la posición 4
peso [-4] #selecciona todos menos el 4º
peso [c(4, 7, 14, 46)]
range(peso)
peso[peso > 50] #seleccionamos los que pesan más de 50
peso[peso != 40] #seleccionamos los distintos de 40

#cheat-sheet: 1º pasos al explorar una base de datos
#estructura, nombres y dimensiones
#medias rangos y valos clave (Nas, 0)
#variables a renombrar, seleccionar o filtrar
summary(df_salvaje)
str(df_salvaje)

nrow(df_salvaje)
ncol(df_salvaje)
dim(df_salvaje)

#View(df_salvaje)
head(df_salvaje)

#diferencias entre un vector y un df
#filtrar serpientes
#filtrar el peso mayor de 50
# ordenar por altura
head(df_salvaje[peso > 50, ])

serpientes <- df_salvaje[df_salvaje$especies == "serpiente", ]
summary(serpientes$peso)
nrow(df_salvaje) > nrow(serpientes)

serpientes50 <- df_salvaje[especies == "serpiente"  & peso > 50, ]
nrow(serpientes50) > nrow(serpientes)


head(df_salvaje[df_salvaje$especies == "serpiente", ])
head(df_salvaje[peso > 50, ])

sort(df_salvaje$altura, decreasing = FALSE)

#seleccionar y filtrar con Rbase
head(df_salvaje[sort(altura),])
sort(as.numeric(serpientes50$altura))

#seleccionar y filtrar con tidyverse
?filter
serpientes50 <- filter(df_salvaje, especies == "serpiente" &
                         peso > 50) |>
                      arrange(altura) |>
                      head()

#15 sitios donde hemos muestreado los 1000 individuos de 7 spp
sitio <- sample(1:15, 1000, replace = TRUE)
df_salvaje <- cbind(df_salvaje, sitio)

kk <- df_salvaje |>
  mutate(peso_100 = peso * 100) |>
  head()
names(df_salvaje)
#df_salvajes$sitio <- sample(1:15, 1000, replace = TRUE)

names(df_salvaje)
df_salvaje_spp_col <- df_salvaje |>
  pivot_wider(names_from = especies, values_from = peso, values_fill = 0) 

df_salvaje_spp_col <- df_salvaje |>
  pivot_wider(names_from = especies, values_from = peso,) |>
  mutate(across(everything(), ~replace_na (.x, 0)))

#df_salvaje_spp_col[is.na(df_salvaje_spp_col)] <- 0

#view(df_salvaje_spp_col)
str(df_salvaje_spp_col)
summary(df_salvaje_spp_col)
mutate()

names(df_salvaje_spp_col)
df_sitio_sspcol_peso <- df_salvaje_spp_col |>
  group_by(sitio)|>
  summarise(perro = sum(perro),
           triceratops = sum (triceratops),
           caracol = sum(caracol),
           cuervo = sum(cuervo),
           jirafa = sum(jirafa),
           serpiente = sum(serpiente),
           rana = sum(rana)
            )

df_sitio_spp_peso <- df_salvaje |>
  group_by (sitio, especies) |>
  summarise(peso = sum(peso))

ggplot(df_sitio_spp_peso, aes(x = especies, y = sitio, fill = peso)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_c(name = peso) + #escala de colores
  theme_minimal()
  
#día 4 - tidyverse####

#mtcars de tidyverse
#chequear los datos
dim(mtcars)
data(mtcars)
head(mtcars)
#view(mtcars)
tail(mtcars)
summary(mtcars)
glimpse(mtcars)
str(mtcars)
mtcars$car_names <- row.names(mtcars) 
names(mtcars)

#nuestro objetico: seleccionar la mejor marca en base a mpg * cyl
#1. seleccionamos variables de interes
cars <- mtcars |>
        select(car_names, mpg, cyl)
str(cars)

#2. filtramos el número de cilindros mayor que 4
#3. multiplicamos el número de cikinros por mpg
#4. extraer la marca que es la primera palabra

cars <- cars |>
  filter(cyl > 4) |>
  mutate(mpg_cyl = mpg * cyl) |>
  mutate(brand = word(car_names, 1)) |>
  group_by(brand) |>
  summarise(mpg_cyl = mean(mpg_cyl)) |>
  arrange(desc(mpg_cyl)) |> #ordenamos las marcas por mpg_cyl descendiente
  slice(1:3) |> #sllice para cortar las 3 primeras posiciones
  mutate(ranking = 1:3)
head(cars)

#car_brand lo ordenamos de manera descendiente
#slice para cortar las tres primeras posiciones

#df con dos columnas, una con posiciones  otro con el tipo de medalla
#con 3 observaciones
medallas <- data.frame(ranking = seq(1,4, by = 1),
                       medalla= c("oro", "plata", "bronce", "losser"))
str(medallas)
winner <- full_join(cars, medallas, by = "ranking")
head(winner)
#unimos las tres mejores marcas con las tres medallas


#día 5 y día 6 ####

co_data <- read_rds("01_data/co2.rds")
tmin_data <- read_rds("01_data/tmin_sevilla(1).rds")
tmax_data <- read_rds("01_data/tmax_sevilla(1).rds")

#explorar los datos

str(co_data)
names(co_data)

co_data_l <- co_data |>
  pivot_longer(
    cols = "1970":"2019",
    names_to = "co_year", 
    values_to = "co_value" 
  )

glimpse(co_data_l)

str(tmin_data)
str(tmax_data)
summary(tmin_data)
#view(co_data_l)
#depurar la base de datos

unique(co_data_l$country_name)

co_data_l_sp <- co_data_l |>
  filter(country_name == "Spain and Andorra") |>
  select(!country_name)

str(co_data_l_sp)

tmin_data_sel <- tmin_data |>
  select(!ID_coords)

tmax_data_sel <- tmax_data |>
  select(!ID_coords)

temp_data <- full_join(tmax_data_sel, tmin_data_sel) |>
  mutate(
    tmean = (Tmax + Tmin)/2 ,
    date = as.Date(date),
    year = format(date, format = "%Y"),
  ) |>
  select (!date) |>
  group_by(year) |>
  summarise (
    tmean = mean(tmean)
  )
#hacer la media de temperatura en una nueva columna

unique(temp_data$year) 
#install.packages("testthat")
library(testthat)
?testthat
#test_that("mismo numero años",
#         expect_equal(unique(temp_data$year),
#                     unique(co_temp$year)))

co_temp <- inner_join(temp_data, co_data_l_sp, 
                      by = join_by(year == co_year)) |>
  mutate(
    across(c(year, co_value), as.numeric)
  ) 

test_that("mismas filas", 
          expect_equal(nrow(co_temp), 
                       nrow(co_data_l_sp)))            
str(co_temp)

#data visualisation

gg_sevilla_co <- ggplot(co_temp, aes(x = year, y = co_value)) +
  geom_point() +
  geom_smooth(method = "lm")

co_temp_total <- co_temp |>
  group_by(year) |>
  summarise(
    tmean = mean(tmean),
    co_value = sum(co_value)) 

#rename(year = co_year), para cambiar el nombre de una columna

str(co_temp_total)

gg_sevilla_co <- ggplot(co_temp_total, aes(x = year, y = co_value)) +
  geom_point() +
  geom_smooth(method = "lm")

gg_sevilla_co

gg_sevilla_temp <- ggplot(co_temp_total, aes(x = year, y = tmean)) +
  geom_point() + 
  geom_smooth(method = "lm")

gg_sevilla_temp

gg_co_temp <- ggplot(co_temp_total, aes(x = co_value, y = tmean, colour = year)) +
  geom_point() +
  scale_color_viridis_b() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
  )

gg_co_temp

modelo <- lm(co_temp_total$tmean ~ co_temp_total$co_value)
summary(modelo)

#install.packages("patchwork")
library(patchwork)

figura_final <- (gg_sevilla_co + gg_sevilla_temp) / gg_co_temp

ggsave(file = "03_resultados/fig1_co_temp.png", 
       plot = figura_final,
       dpi = 300,
       width = 200,
       height = 140,
       units = "mm")

citation()
citation("ggplot2")

# día 7 ###
######################################################################################################## #
######                                                                                               ### #
######                     Iniciación práctica a la gestión de datos ambientales con R               ### #
######                              Universidad de Alcalá, 2025-2026                                 ### #
######                                 Profesora Sara Villén Pérez                                   ### #
######                                                                                               ### #
######                               2) VISUALIZACIÓN DE DATOS EN R                                  ### #                                                                                       #####
######                                                                                               ### #
######################################################################################################## #

# Recommended reading:
# ggplot2 reference: https://ggplot2.tidyverse.org/reference/
# Chang, W. (2021). R graphics cookbook: practical recipes for visualizing data. "O'Reilly Media, Inc.".(2nd edition)
#     https://r-graphics.org/
# R colors https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html
# ggplotgui: to play and copy the code: http://shiny.gmw.rug.nl/ggplotgui/


################################################## #
#### 0 ### PREPARACIÓN DEL ESPACIO DE TRABAJO #### 
################################################## #
# 1 # Limpiar el espacio de trabajo
rm(list=ls())

# 2 # Crear proyecto para Bloque IV Visualización
# File > New Project > New Directory > New project

# 3 # Comprobar directorio de trabajo
getwd()
# Si necesitas cambiar el directorio de trabajo tienes 2 opciones: 
# Ctrl+Mayus+H
# setwd("tu/ruta/aqui")
# (aunque si trabajas en un projecto no necesitas cambiarlo) 

# 4 # Colocar este script dentro de la carpeta del proyecto

# 5 # Abrir el proyecto y abrir el script desde files

# 6 # Instalar paquetes (una vez) y llamar a las librerías que vamos a usar
#install.packages("tidyverse")
# install.packages("gcookbook")
# install.packages("ggrepel")
# install.packages("hexbin")
#install.packages("patchwork")
library(tidyverse) # Metapaquete tidyverse del que usaremos ggplot2 para gráficos, dplyr para manipulación de datos, tidyr para reorganizar datos
library(gcookbook) # Bases de datos que vamos a usar
library(ggrepel)   # Para etiquetas que se solapan (funcionalidad interesante que no incluye ggplot de serie)
library(hexbin)    # Para mapas de frecuencia hexagonales (funcionalidad interesante que no incluye ggplot de serie)
library(patchwork) # Para figuras combinadas (funcionalidad interesante que no incluye ggplot de serie)

#******************************************** *
########### # 
#### BLOQUE 1  -  INTRODUCCIÓN A GGPLOT    ####   
########### # 
#******************************************** *

################################################### # 
#### 1 ### INTRODUCCIÓN A LA LÓGICA DE GGPLOT  #### 
################################################### # 

########### #
#### 1.1 ## ESTRUCTURA DE LOS DATOS: comparación con gráficos básicos en R #### 
#################################### #
# Datos de ejemplo
simpledat

### USANDO GRÁFICOS DEL PAQUETE BASE:
##################################### #
# Gráfico de barras agrupado por Bs
barplot(simpledat)

# Gráfico de barras agrupado por As, le damos la vuelta a la base de datos
t(simpledat)  
barplot(t(simpledat))

# No es óptimo: para cambiar el gráfico necesitamos reestructurar los datos

### ¿CÓMO SERÍA CON GGPLOT2?
############################ #
# Para ggplot la estructura de datos es siempre la misma: FORMATO LARGO/VERTICAL
simpledat_long
# Formato largo:
#     - Cada línea representa una observación
#     - La información viene dada en variables, no por su posición en la matriz

# Gráfico de barras agrupado por Bs:
ggplot(simpledat_long, aes(x=Aval, y=value, fill=Bval)) +
  geom_col()
# Observa:
# x=Aval define que las categorías de la variable "Aval" se representen en el eje x
# y=value define que en el eje y (es decir, la altura de las barras) se represente la variable "value"
# fill=Bval define que el color de la barra represente las categorías de "Bval" 
# geom_col() pide un gráfico de barras 

# $$$ Ejercicio: considerando lo anterior, haz el gráfico agrupado por As:
# $$$ RESPUESTA:
ggplot(simpledat_long, aes(x=Bval, y=value, fill=Aval)) +
  geom_col()

########### #
#### 1.2 ## TIPO DE GRÁFICO: comparación con gráficos base en R #### 
############################ #
# Datos de ejemplo
simpledat

### USANDO GRÁFICOS BASE: 
######################## #
# Gráfico de barras agrupado por Bs:
barplot(simpledat)

# Gráfico de LÍNEAS agrupado por Bs:
plot(simpledat[1,], type="l")
lines(simpledat[2,], type="l", col="blue")

# No es óptimo:
#   1) Para cambiar de tipo de gráfico, es necesario usar una 
#   función totalmente diferente (falta de lógica estructurada)
#   2) Las líneas no se pintan simultaneamente y por ello los ejes no se ajustan bien:
#   Los límites del eje y se definen para la línea negra (y la azul queda fuera del área del gráfico)
#   3) El eje x se interpreta como continuo en lugar de categórico

### ¿CÓMO SERÍA CON GGPLOT2?
############################ #
# Los componentes del gráfico se combinan en capas, usando +
# Los gráficos se construyen agregando componentes gradualmente 
# (o sustituyendo unos por otros)

# Gráfico de barras agrupado por Bs:
ggplot(simpledat_long, aes(x=Aval, y=value, fill=Bval)) +
  geom_col()

# Gráfico de líneas agrupado por Bs:
ggplot(simpledat_long, aes(x=Aval, y=value, colour=Bval, group=Bval)) +
  geom_line()

# Observaciones:
#   Sustituimos geom_col() por geom_line()
#   Usamos colour para colorear las líneas, y group para agrupar por Bs
#   Los límites del eje y se ajustan automáticamente a ambas líneas porque se dibujan simultáneamente
#   El eje x se entiende como categórico
#   Se genera una leyenda automáticamente para el color de la línea


########################################################################################## #
#### 2 ### CONSTRUCCIÓN DE UN GRÁFICO SIMPLE EN GGPLOT: ggplot(data, aes()) + geom_() #### 
########################################################################################## #
# Lista de posibles capas en ggplot: http://ggplot2.tidyverse.org/reference/

########### #
#### 2.1 ## datos #### 
###################### #
# data.frame, formato largo
# ejemplo
dat <- tibble(ID=1:4, xval=2:5, yval=c(3,5,6,9), group=c("A","B","A","B"))
dat

# podemos transformar nuestros datos a 
# formato largo usando pivot_longer() del paquete {tidyr}  

########### #
#### 2.2 ## función ggplot() #### 
################################# #
ggplot(dat, aes(x=xval, y=yval ))

# Observación:
# ¡no hay puntos, barras ni líneas!
# ... porque para eso es necesario definir cómo visualizar los datos (geom_())

# $$$ Ejercicio: Observa los ejes. ¿Están relacionados con xval y yval?
# $$$ RESPUESTA: sí

# $$$ Ejercicio: crea un nuevo gráfico con xval en el eje y, y yval en el eje x
# y observa los ejes.
# $$$ RESPUESTA:
ggplot(dat, aes(x=yval, y=xval))

###########  #
#### 2.3 ## ggplot() + geom_() #### 
################################### #
# Incluyamos la función geom_() para indicar cómo representar los datos
# Para un diagrama de dispersión, usamos geom_point() 
ggplot(dat, aes(x=xval, y=yval)) + geom_point() 

# $$$ Ejercicio: dibuja una línea que cruce los puntos de datos (pista: geom_line())
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + 
  geom_point()

# $$$ Ejercicio: crea un gráfico con líneas y puntos:
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval, )) +
  geom_point() +
  geom_line()


########### #
#### 2.4 ## aes() ####
###################### #
# Ahora identifiquemos los puntos de los grupos A y B con diferentes colores:
ggplot(dat, aes(x=xval, y=yval, colour=group)) + geom_point()
# Mismo resultado:
ggplot(dat, aes(x=xval, y=yval)) + geom_point(aes(colour=group))

# $$$ Ejercicio: dibuja una línea diferente para cada grupo (A y B)
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval, colour=group )) +
  geom_point() +
  geom_line()

# $$$ Ejercicio: incluye puntos en el gráfico anterior sin diferenciar
# el color por grupo (es decir, líneas coloreadas por grupo, puntos negros)
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + 
  geom_line(aes(colour=group))+ 
  geom_point() 
ggplot(dat, aes(x=xval, y=yval,colour=group)) + 
  geom_line()+ 
  geom_point(colour="black") 

# $$$ Ejercicio: ahora colorea tanto las líneas como los puntos por grupo
# (pista: hay dos formas de hacerlo)
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + 
  geom_line(aes(colour=group)) + 
  geom_point(aes(colour=group))

ggplot(dat, aes(x=xval, y=yval, colour=group)) + 
  geom_point() + 
  geom_line() 

############ #
#### 2.5 ## aes: ¿qué argumentos son válidos para cada función? ####
##################################################################### #
# $$$ Ejercicio: ¿Qué argumentos son válidos para geom_point()? 
# (Pista: revisa la sección "Aesthetics" en la ayuda de la función usando: ?geom_point)
# $$$ RESPUESTA:
?geom_point
# x	
# y	
# •	alpha	→ NA
# •	colour	→ via theme() #líneas y cosas sólidas
# •	fill	→ via theme() #relleno
# •	group	→ inferred
# •	shape	→ via theme()
# •	size	→ via theme() #variable numérica + mapas
# •	stroke	→ via theme() #grosor



# $$$ Ejercicio: Elige alguna de estas opciones para identificar 
# la variable "group" en el siguiente gráfico.
# Prueba al menos alpha, shape, size y presta atención a los mensajes de advertencia.
dat
ggplot(dat, aes(x=xval, y=yval)) + geom_point()
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval, alpha=group)) + geom_point()
ggplot(dat, aes(x=xval, y=yval, size=group)) + geom_point()
ggplot(dat, aes(x=xval, y=yval, shape=group)) + geom_point()



########### #
#### 2.6 ## Recomendaciones importantes para añadir capas #### 
##############################################################

## ¿Dónde colocar el "+" ?
########################### #
# Para facilitar la lectura del código, cada función (es decir, cada capa) 
# suele ubicarse en una línea diferente. Entonces, ¿dónde colocar el "+"?

# $$$ Ejercicio: prueba las dos opciones y responde la pregunta anterior.
# Opción a:
ggplot(dat, aes(x=xval, y=yval))  
+ geom_point()
# Opción b:
ggplot(dat, aes(x=xval, y=yval)) +
  geom_point()
# $$$ RESPUESTA: detrás de ggplot, arriba


# $$$ Ejercicio: ¿qué pasa si no incluimos el "+"?
ggplot(dat, aes(x=xval, y=yval)) 
geom_point()
# $$$ RESPUESTA: que lee una línea solamente


## Almacenar el gráfico en un objeto
######################################
# Para generar diferentes versiones del gráfico
p <- ggplot(dat, aes(x=xval, y=yval)) + geom_point()
p
l <- p + geom_line()
l
# O incluso:
b <- ggplot(dat, aes(x=xval, y=yval)) 
p <- b + geom_point()
l <- b + geom_line()
pl <- b + geom_point() + geom_line()
p
l
pl


#******************************************
############## #
#### BLOQUE 2  -  GEOMETRÍAS: geom_()  #### 
############## #
#******************************************

#***************************************************************************************
# BLOQUE 2.1.  - GRÁFICOS BÁSICOS
#***************************************************************************************
# * 3-Gráficos de dispersión:            geom_point()
# * 4-Gráficos de líneas:                geom_line()
# * 5-Gráficos de áreas:                 geom_area()
# * 6-Gráfico de barras de valores:      geom_col()
#***************************************************************************************

##################################################### #  
####  3 ### GRÁFICOS DE DISPERSIÓN: geom_point() ####    
##################################################### #

############ #
#### 3.1. ## Gráfico de dispersión básico #### 
############################################## #
# x e y suelen ser variables continuas (<int> o <dbl>)
# Ejemplo:
heightweight <- as_tibble(heightweight)
heightweight

# $$$ Ejercicio: Completa los espacios para hacer un gráfico de dispersión 
# básico usando la base de datos heightweight. Quieres representar la 
# relación entre "ageYear" y "heightIn". 
# heightIn es la variable respuesta (en el eje y).
#ggplot(heightweight, aes(x=ageYear , y=heightIn )) + geom_point()
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear , y=heightIn )) + geom_point()

############ #
#### 3.2. ## Gráfico de dispersión agrupado #### 
################################################ #
# Una variable categórica definirá la estética de los puntos

# $$$ Ejercicio: diferencia los puntos de hombres y mujeres:
# a) usando diferentes colores (pista: colour)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear , y=heightIn, colour=sex )) + geom_point()


# b)* usando diferentes formas de puntos (pista: shape)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear , y=heightIn, shape=sex )) + geom_point()


# c)* usando tanto diferentes colores como formas
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear , y=heightIn, colour=sex, colour=sex )) + geom_point()

# d)* usando diferentes tamaños de puntos (pista: size) (¡presta atención a la advertencia!) 
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear , y=heightIn, size=sex )) + geom_point()


############ ############# #size_sum()
#### 3.3. ## Gráfico de dispersión con una tercera variable continua #### 
######################################################################### #
# Una variable continua definirá la estética de los puntos

# $$$ Ejercicio: Incluye la variable "weightLb" en el siguiente gráfico:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  geom_point()
# a) usando colores
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) +  geom_point()

# b)* usando tamaños de puntos (pista: usa alpha=0.2 en geom_point para facilitar la visualización con transparencia)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) +  geom_point(alpha=0.2)
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) +  geom_point(shape=6)


# $$$ Ejercicio: Ahora representa "weightLb" con diferentes tamaños de puntos y 
# "sex" con diferentes colores
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +  
  geom_point(alpha=0.2)


# OBSERVACIÓN: la precisión en la percepción es mayor para las coordenadas (x,y) que para 
# los colores o tamaños. ¡Elige las variables para cada estética teniendo en cuenta esto!

# $$$ Ejercicio*: representa "weightLb" con diferentes tamaños de puntos y 
# "sex" con diferentes formas
# $$$ RESPUESTA:


# OBSERVACIÓN: no es recomendable mapear tamaño y forma simultáneamente,
# porque es difícil comparar el tamaño de diferentes formas 
# (de hecho, el tamaño de diferentes formas es diferente)

############ #
#### 3.4. ## Gráficos de dispersión: soluciones para la superposición de puntos #### 
##################################################################################### #

## Superposición media
######################## #
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(size=5)

# Solución 1 - transparencia de los puntos: alpha
# $$$ Ejercicio: modifica el valor de alpha (de 0 a 1)
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(alpha=0.8, size=5)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(alpha=0.3, size=5)

# Solución 2 - tamaño de los puntos: size
# $$$ *Ejercicio: modifica el valor de size
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(size=5)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(size=2)

# Solución 3 - forma de los puntos: shape
# $$$ *Ejercicio: modifica el valor de shape
# Pista: valores de shape para ver mejor la superposición: 0-14
# En mi opinión, los mejores son: 0-6, 8
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape=1, size=5)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape=6, size=5)

## *Superposición alta
####################### #
sa <- ggplot(diamonds, aes(x=carat, y=price)) 
sa + geom_point()

# Solución 1 - transparencia de los puntos: alpha
# $$$ *Ejercicio: implementa esta solución usando transparencia al 90% y 99% 
# No te preocupes si el gráfico tarda en dibujarse: es porque son muchos puntos.
# $$$ RESPUESTA:
sa + geom_point(alpha=0.2)

# Solución 2 - graficar la "densidad de puntos" en un raster
# $$$ *Ejercicio: grafica la densidad de puntos en el gráfico price-carat
# pista: geom_bin2d(). Es un geom_(), por lo que no se necesita geom_point()
# Por defecto, geom_bin2d() divide el espacio en 30x30=900 celdas
# $$$ RESPUESTA:
sa + geom_bin2d()

# $$$ *Ejercicio: elige el número de cuadrantes en los que se divide cada eje
# usando el argumento "bins"
# $$$ RESPUESTA:
sa + geom_bin2d(binds=70)

## *Superposición dentro de una variable categórica
################################################### #
# Datos de ejemplo
ChickWeight <- as_tibble(ChickWeight)
ChickWeight
# Observación: time está definido como continuo, pero en realidad es categórico

sc <- ggplot(ChickWeight, aes(x=factor(Time), y=weight))
sc + geom_point()

# Solución 1: dispersión aleatoria de los puntos (geom_jitter)
sc + geom_jitter(width = 0.2)

# $$$ *Ejercicio: ejecuta el gráfico anterior varias veces.
# ¿Cambia? ¿Por qué?
# $$$ RESPUESTA: cambia porque los separa de forma aleatoria

# $$$ *Ejercicio: modifica los valores de width en geom_jitter()
sc + geom_jitter(width=0.2)
# $$$ RESPUESTA: 
sc + geom_jitter(width=0.05)

# $$$ *Ejercicio: en cuál de los siguientes gráficos puede ser recomendable 
# usar width>0 y height>0?
ggplot(ChickWeight, aes(x=Time, y=weight)) + 
  geom_jitter(width=0.2, height=0)
ggplot(ChickWeight, aes(x=weight, y=Time)) + 
  geom_jitter(width=0, height=0.2)
# $$$ RESPUESTA: width>0 en el primero y height>0 en el segundo.


# Solución 2: no hagas un diagrama de dispersión ;)
# La variación de los puntos se puede sintetizar de otras maneras
# por ejemplo, un diagrama de cajas agrupado por una variable categórica
# $$$ *Ejercicio: usa "geom_boxplot()" para representar el peso en diferentes momentos
# Consejo: usa factor(Time) o group=Time para especificar que deseas agrupar los datos en categorías de Tiempo
# $$$ RESPUESTA:
ggplot(ChickWeight, aes(y=weight, x=factor(Time))) + 
  geom_boxplot() 

############ #
#### 3.5. ## *Diagramas de dispersión con rug marginal: geom_rug() #### 
####################################################################### #
# Para ver la distribución de los datos a lo largo de cada eje

# Ejemplo:
faithful <- as_tibble(faithful)
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_point() + 
  geom_rug()

# $$$ *Ejercicio: añade un rug a la siguiente figura, para ver la distribución 
# de los datos en cada variable
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  geom_point()
# $$$ RESPUESTA: 
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  geom_point() + geom_rug()

############ #
#### 3.6. ## Diagrama de dispersión con línea de regresión ajustada: geom_smooth() #### 
####################################################################################### #
heightweight
sr <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sr + geom_point()

# $$$ Ejercicio: representar los puntos y la línea ajustada
# $$$ RESPUESTA: 
sr + geom_point() + geom_smooth(method=loess)

# Observación: por defecto usa el método "loess", que combina
# polinomios ajustados localmente
# Observación 2: por defecto define un intervalo de confianza del 95%

# $$$ Ejercicio*: busca en la ayuda de R cuáles son los argumentos de geom_smooth()
# para modificar el método de ajuste y el nivel de confianza.
# Crea un nuevo gráfico ajustando un modelo lineal (lm) con un intervalo de confianza del 99%.
# $$$ RESPUESTA: 
?geom_smooth
sr + geom_point() + geom_smooth(method=lm, level=0.99)

# $$$ Ejercicio*: encuentra cómo ocultar el intervalo de confianza
# $$$ RESPUESTA: 
sr + geom_point() + geom_smooth(method=lm, level=0.99, alpha=0)

# $$$ Ejercicio*: colorea la línea en rojo y el intervalo de confianza en rosa
# $$$ RESPUESTA: 
sr + geom_point() + geom_smooth(method=lm, level=0.99, colour="green", fill="pink")


# $$$ Ejercicio: agrupa los puntos por sexo y ajusta una línea para cada sexo
# $$$ RESPUESTA: dos opciones:
sr + geom_point() + geom_smooth(method=lm, level=0.99, colour="green", fill="pink")


# $$$ Ejercicio**: imagina cómo representarías una línea ajustada con un modelo diferente a los 
# ofrecidos en las opciones de geom_smooth() (es decir, cómo lo harías manualmente)
# $$$ RESPUESTA: 
sr + geom_point() + geom_smooth(method=lm, level=0.99, colour="green", fill="pink")


############ #
#### 3.7. ## Estética en diagramas de dispersión #### 
##################################################### #

## Configuración de estética en geom_point(): forma, tamaño, color, relleno
############################################################################# #
# $$$ Ejercicio: prueba a cambiar los valores de forma, tamaño, color y relleno 
# en el siguiente gráfico
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape=1, size=2, colour= "black", fill="white")

# Consejo 1: puedes definir las formas de los puntos con números (0-25 son los básicos, que funcionan siempre bien)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape=25, size=3, colour= "red", fill="blue")


# Consejo 2: puedes definir formas de puntos con cualquier símbolo del teclado entre ""
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape="&", size=2, colour= "black")


# Consejo 3: puedes definir formas de puntos con atajos en un teclado numérico extendido, usando "" 
# Alt+número. Ejemplo: Alt+3 da como resultado un corazón. 
# Ver una lista de posibilidades en: https://typefacts.com/en/articles/keyboard-shortcuts
# ¡Y a jugar! ;)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape="☼", size=5, colour="red")


# Consejo 4: entre las formas habituales, el relleno solo funciona para las formas 21-25
# Es decir, 21–25 tienen fill y colour
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape=22, size=3, colour= "red", fill="pink")

# Consejo 5: observa algunas opciones de forma en este gráfico:
tibble(p=c(0:25,32:127)) %>%
  ggplot() +
  geom_point(mapping=aes(x=p%%16, y=p%/%16, shape=p), size=5, fill="red") +
  geom_text(mapping=aes(x=p%%16, y=p%/%16+0.25, label=p), size=3)+
  scale_y_reverse() +
  scale_shape_identity() +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())
# Observa que:
# formas 1-14 solo tienen un contorno (-> usa "colour")
# formas 15-20 son sólidas (-> usa "colour")
# formas 21-25 tienen contorno y relleno (usa "colour" y "fill", respectivamente)
# por encima de 25, son símbolos (-> usa "colour")


############################################### #
#### 4 ### GRÁFICOS DE LÍNEAS: geom_line() ####   
############################################### #

############ #
#### 4.1. ## Gráfico de Línea Básico #### 
######################################### #

## x, y: variables contínuas
############################## #
# Datos
BOD <- as_tibble(BOD)
BOD

# $$$ Ejercicio: transforma el siguiente diagrama de dispersión en un gráfico de línea
ggplot(BOD, aes(x=Time, y=demand)) + geom_point() 
# $$$ RESPUESTA:
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() 

## y: variable continua; x: categórica 
####################################### #
# Ahora Time será un factor (categórico):
BOD$TimeFact <- factor(BOD$Time)
BOD
# $$$ Ejercicio: lee el error:
ggplot(BOD, aes(x=TimeFact, y=demand)) + geom_line()

# $$$ Ejercicio: ahora prueba de esta manera:
ggplot(BOD, aes(x=TimeFact, y=demand, group=1)) + geom_line()

# Observación: cuando la variable x es un factor, 
# group=1 indica que todos los puntos pertenecen al mismo grupo y deben estar unidos por una sola línea.
# El "1" es una convención. Funcionaría group="cualquier cosa", porque sólo es para indicar que 
# todos los puntos pertenecen al mismo grupo, pero lo estándar es group=1.


############ #
#### 4.2. ## Gráfico de Líneas Múltiples: una línea por grupo #### 
################################################################## #

## x, y: variables continuas
############################## #
# Datos
tg <- ToothGrowth %>%
  dplyr::group_by(supp, dose) %>%
  dplyr::summarise(length = mean(len), .groups="drop")
tg

ggplot(tg, aes(x=dose, y=length)) + geom_line() + geom_point()
# Observación: hay diferentes valores de y para un mismo valor de x.
# Eso sugiere que tenemos datos agrupados.

# $$$ Ejercicio: Transforma el siguiente gráfico para representar dos líneas, 
# una para los grupos OJ y otra para los grupos VC.
# Sugerencia: pueden tener colores diferentes
ggplot(tg, aes(x=dose, y=length)) + geom_line()
# $$$ RESPUESTA:
ggplot(tg, aes(x=dose, y=length, fill=supp, colour=supp)) + 
  geom_line() + 
  geom_point(size=4, shape=21)

ggplot(tg, aes(x=dose, y=length, fill=supp,)) + 
  geom_line() + 
  geom_point(size=4, shape=21)

# $$$ Ejercicio*: Ahora diferéncialas por tipo de línea
# (¡publicar en colores es caro!)
# Pista: linetype
# $$$ RESPUESTA:
ggplot(tg, aes(x=dose, y=length, fill=supp, linetype=supp)) + 
  geom_line() 

# $$$ Ejercicio*: diferencia los dos grupos por la forma de los puntos
# Consejo: puedes agrandar los puntos para facilitar la discriminación de formas
# $$$ RESPUESTA:
ggplot(tg, aes(x=dose, y=length, fill=supp, size=supp)) + 
  geom_point()



# $$$ Ejercicio*: diferencia los dos grupos por color de los puntos
# Consejo: fill para formas 21-25, colour para otras
# Consejo 2: puedes agrandar los puntos para facilitar la discriminación de colores
# $$$ RESPUESTA:
ggplot(tg, aes(x=dose, y=length, fill=supp, colour=supp)) + 
  geom_point()

## *1 variable continua y 1 categórica
####################################### #
# Como vimos antes, en gráficos de líneas con variables categóricas necesitamos especificar 
# el argumento "group" en aes() (group=1 si todos pertenecen al mismo grupo como antes)
ggplot(tg, aes(x=factor(dose), y=length, group=1)) + geom_line()

# Si queremos hacer una línea por grupo, necesitamos especificar el factor de agrupación en 
# (cuando es una variable categórica): 
ggplot(tg, aes(x=factor(dose), y=length, group=supp)) + 
  geom_line()

# Si también queremos diferenciarlas con una estética (por ejemplo, color),
# necesitamos especificar ambos argumentos (por ejemplo, group y colour)
ggplot(tg, aes(x=factor(dose), y=length, group=supp, colour=supp)) + 
  geom_line()

# $$$ Datos para ejercicios
uspopage2 <- uspopage %>%
  as_tibble() %>%
  mutate(Year_fact = factor(Year))
uspopage2

# $$$ *Ejercicio: representa la variación de Thousands a lo largo de Year, 
# para cada AgeGroup
# Pista: Year es continuo
# $$$ RESPUESTA:
ggplot(uspopage2, aes(x=Year, y=Thousands, colour=AgeGroup)) +
  geom_line()

# $$$ *Ejercicio: representa la variación de Thousands a lo largo de Year_fact, 
# para cada AgeGroup
# Pista: Year_fact es categórico 
# $$$ RESPUESTA:
ggplot(uspopage2, aes(x=factor(Year_fact), y=Thousands, group=AgeGroup, colour=AgeGroup)) +
  geom_line()

############ #
#### 4.3. ## *Intervalo de confianza #### 
######################################## #
# Datos
clim <- climate %>%
  as_tibble() %>%
  subset(Source == "Berkeley",
         select=c("Year", "Anomaly10y", "Unc10y"))
clim

## IC como área: geom_ribbon()
################################ #
# $$$ Ejercicio: tienes este gráfico de líneas:
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line()
# completa el código para hacer un gráfico de líneas con un área de intervalo de confianza
# considerando que:
# para cada año, el límite inferior del IC se define por: Anomaly10y-Unc10y
# para cada año, el límite superior del IC se define por: Anomaly10y+Unc10y
# ggplot(clim, aes(x=Year, y=Anomaly10y)) +
#   geom_ribbon(aes(ymin=_____, ymax=_____), alpha=0.2, fill="blue") +
#   geom_line()
# $$$ RESPUESTA:
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.2, fill="blue") +
  geom_line()

## *IC con dos líneas
###################### #
# $$$ *Ejercicio: intenta crear un gráfico de líneas con el IC representado por dos líneas, 
# considerando que:
# para cada año, el límite inferior del IC se define por: Anomaly10y-Unc10y
# para cada año, el límite superior del IC se define por: Anomaly10y+Unc10y
# Consejo: diferencia la línea principal y las del IC por color o tipo de línea (por ejemplo, colour="red", linetype="dotted")
# $$$ RESPUESTA:
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.2, colour="red", linetype="dotted") +
  geom_line()

ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line() +
  geom_line(aes(y=Anomaly10y-Unc10y), colour="red", linetype="dotted") +
  geom_line(aes(y=Anomaly10y+Unc10y), colour="red", linetype="dotted") 



# Observación: ¡podemos graficar diferentes variables en el eje y usando diferentes geoms!

############ #
#### 4.4. ## Estética de los gráficos de líneas #### 
#################################################### #

## Límites de los ejes: ylim(), xlim(), expand_limits()
############### #
# $$$ Ejercicio: observa cómo los siguientes códigos cambian los ejes
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + 
  ylim(0, 25) + xlim(-10,20)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + 
  expand_limits(y=0, x=c(-10,20))
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + 
  expand_limits(y=0)

# $$$ Ejercicio: cambia los límites del eje x para que varíen 
# de 0 al valor máximo de Time, usando xlim()
# Pista: el máximo de Time es max(BOD$Time)
# $$$ RESPUESTA:
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + 
  xlim(0, max(BOD$Time)) + ylim(-10,20)

# $$$ *Ejercicio: amplía los límites del eje x para que comiencen en 0, usando expand_limits()
# $$$ RESPUESTA:
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + 
  expand_limits(x=0)

# $$$ *Ejercicio: amplía los límites del eje x de 0 a 10, usando expand_limits()
# $$$ RESPUESTA:
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + 
  expand_limits(x=c(0,10))

## Ejes logarítmicos: scale_y_log10(), scale_x_log10()
##################################################### #
# $$$ Ejercicio: considerando el título de esta sección, imagina cómo transformar 
# el eje y a una escala logarítmica en base 10:
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() 
# $$$ RESPUESTA:
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() +
  scale_y_log10()

## *Evitar la superposición de líneas o puntos: position=position_dodge()
######################################################################### #
# $$$ Ejercicio: modifica el valor en position_dodge() para evitar la 
# superposición en líneas y puntos
# Consejo: empieza probando valores entre 0.1 y 0.5
ggplot(tg, aes(x=dose, y=length, shape=supp)) +
  geom_line(position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2), size=4)

ggplot(tg, aes(x=dose, y=length, shape=supp)) +
  geom_line(position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.5), size=4)
## *Configuración de estética en geom_line(): linetype, size, colour
##################################### #
# $$$ *Ejercicio: modifica los valores de linetype, size, colour. 
# Consejo: puedes definir colores ya sea por código numérico o por nombre
# Consejo 2: Aquí tienes posibilidades de colores (por código y nombre):
# https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html
# Consejo 3: Opciones de linetype: "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dotdash", linewidth=2, colour="blue")

# $$$ **Ejercicio: crea un nuevo patrón de linetype
# Consejo 4: puedes definir un patrón de linetype con números en pares: 
# el primero define la longitud del segmento y el segundo la longitud del espacio
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="1199", linewidth=2, colour="coral1")
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="22", linewidth=2, colour="coral1")

## Líneas antes que puntos
########################### #
# $$$ Ejercicio: observa las diferencias en el código y los gráficos:
# Óptimo
ggplot(tg, aes(x=dose, y=length, colour=supp)) +
  geom_line() +
  geom_point(shape=22, size=7, fill="white")
# Subóptimo
ggplot(tg, aes(x=dose, y=length, colour=supp)) +
  geom_point(shape=22, size=7, fill="white") +
  geom_line() 

## Áreas antes que líneas
########################## #
# $$$ Ejercicio: observa las diferencias en el código y los gráficos:
# Óptimo
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), fill="lightblue") +
  geom_line()
# Subóptimo
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line() +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), fill="lightblue")

# Observación: ggplot2 va pintando las capas por orden de aparición en el código

############################################# #
#### 5 ### GRÁFICOS DE ÁREA: geom_area() ####     
############################################# #

############ #
#### 5.1. ## Gráfico de Área Básico #### 
######################################## #
# Datos
sunspotyear <- tibble(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year))
sunspotyear

# $$$ Ejercicio: Imagina cómo representar las manchas solares por año, 
# usando un gráfico de área.
# Consejo: geom_area() es la función para gráficos de área
# $$$ RESPUESTA:
ggplot(sunspotyear, aes(x=Year, y= Sunspots)) + 
  geom_area()

############ #
#### 5.2. ## Gráfico de Área Múltiple: un área por grupo #### 
############################################################# #

## Apilado: por defecto
########### #
# ¿Recuerdas este gráfico?:
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()

# $$$ Ejercicio: Transforma el gráfico en dos áreas superpuestas
# Pista: recuerda que "colour" se usa para colorear líneas, 
# y "fill" para colorear áreas
# $$$ RESPUESTA:
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_area()


# $$$ Ejercicio: representa cómo ha variado el tamaño de la población de EE.UU. 
# a lo largo del tiempo para cada grupo de edad, usando áreas apiladas
# Datos:
uspopage <- as_tibble(uspopage)
uspopage
# $$$ RESPUESTA:

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup )) +
  geom_area()
## Apilado Proporcional: proporción de cada grupo: geom_area(position="fill")
################################################## #
# Datos
uspopage

# $$$ Ejercicio: especifica position="fill" como argumento de geom_area()
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area(position="fill")


## *No Apilado: geom_area(position="identity")
################ #
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_area(position="identity")

# $$$ *Ejercicio: representa cómo ha variado el tamaño de la población de EE.UU. 
# a lo largo del tiempo para cada grupo de edad, usando áreas no apiladas
uspopage
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year,  y=Thousands, fill=AgeGroup)) + geom_area(position="identity")

############ #
#### 5.4. ## Estética de los Gráficos de Área #### 
################################################## #

## Configuración de estética en geom_area: colour, fill, alpha
################################### #

### *Transparencia del área: alpha
# $$$ Ejercicio: cambia el valor del argumento alpha de 0 a 1
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(position="identity", alpha=0.5)
# $$$ RESPUESTA:


### *Líneas para resaltar el perímetro del área: colour
# $$$ *Ejercicio: dibuja una línea negra sobre las áreas en los siguientes gráficos 
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(alpha=0.4, colour="black")  

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(position="identity", alpha=0.2, colour="black")
# $$$ RESPUESTA:


# $$$ *Ejercicio: colorea cada línea con el mismo color que su área
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, colour=AgeGroup)) +
  geom_area(position="identity", alpha=0.2) 

# $$$ Ejercicio*: piensa en otra forma de dibujar una línea negra sobre cada área
# Consejo: ¡piensa en construir un gráfico en capas! puedes usar diferentes funciones
# Consejo 2: necesitarás el argumento position="stack" en la nueva capa (la capa de área se apila por defecto)
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, colour=AgeGroup)) +
  geom_area(position="identity", alpha=0.2) +
  geom_line(position="stack")

### Colour, fill, alpha 
# $$$ Ejercicio: cambia los valores de los argumentos colour, fill, alpha
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(colour="red", fill="darkorchid1", alpha=0.5)


######################################################### #
#### 6 ### GRÁFICOS DE BARRAS DE VALORES: geom_col() ####   
######################################################### #

########### #
#### 6.1 ## Gráfico de barras básico #### 
######################################### #

## Variable categórica en el eje x
################################## #
pg_mean <- as_tibble(pg_mean)
pg_mean

# $$$ Ejercicio: Crea un gráfico de barras con una barra por "grupo", 
# de altura "weight".
# ggplot(_____, aes(x=_____, y=_____)) + geom_col()
# $$$ RESPUESTA:
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col()

## Variable continua en el eje x
################################## #
# Datos
BOD
# $$$ Ejercicio: Representa la demanda en diferentes momentos
# $$$ RESPUESTA:
ggplot(BOD, aes(x=Time, y=demand)) + geom_col()

# Observación: se representan todos los valores posibles entre el mínimo 
# y el máximo en el eje x, incluyendo time=6 para el cual no hay 
# información de demanda

# $$$ Ejercicio*: Representa solo los valores en el eje x para los cuales 
# hay información en y
# Pista: puedes transformar la variable x en factor usando factor()
# $$$ RESPUESTA:
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_col()

############ #
#### 6.2. ## Gráfico de barras múltiple: agrupar barras considerando una segunda variable #### 
############################## #

## Gráfico de barras agrupadas - barras apiladas (por defecto)
################################# #
# Datos
cabbage_exp <- as_tibble(cabbage_exp)
cabbage_exp

# $$$ Ejercicio: Representa el peso en diferentes fechas
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight)) + geom_col()

# $$$ Ejercicio: Ahora quieres identificar qué parte del peso corresponde 
# a cada grupo de Cultivar
# Sugerencia: Puedes colorear las barras usando el argumento "fill"
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_col()


# Observación: por defecto, las barras están apiladas (una encima de la otra)

## Gráfico de barras agrupadas - barras lado a lado: geom_col(position="dodge")
###################################### #
# $$$ Ejercicio: Para cada fecha, quieres dos barras, una al lado de la otra: 
# una para cada grupo de Cultivar
# Pista: usa la misma estructura que para el gráfico de barras apiladas, 
# pero añadiendo el argumento position="dodge"
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_col(position="dodge")


# $$$ *Ejercicio: ahora diferencia las barras por el color de sus líneas periféricas
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, colour=Cultivar)) + geom_col(position="dodge")

## Gráfico de barras agrupadas - barras superpuestas: geom_col(position="identity")
###################################### #
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position="identity", alpha=0.4)

############ #
#### 6.3. ## Estética de gráficos de barras #### 
#################################### #

## Gráfico de barras horizontal: coord_flip()
###################### #
# Para barras horizontales, necesitamos cambiar las coordenadas del eje
# usando coord_flip() como una nueva capa

# $$$ Ejercicio: transforma el siguiente gráfico en un gráfico de barras horizontal
ggplot(cabbage_exp, aes(x=Date, y=Weight)) +
  geom_col() 
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight)) +
  geom_col() + 
  coord_flip() 

# Coord_flip() se puede usar con otros geoms:
# $$$ Ejercicio*: cambia el eje del siguiente gráfico de líneas:
dat <- tibble(ID=1:4, xval=2:5, yval=c(3,5,6,9), group=c("A","B","A","B"))
ggplot(dat, aes(x=xval, y=yval)) + geom_line(aes(colour=group))
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + geom_line(aes(colour=group)) + coord_flip()

## Reordenar la posición de las barras: x=reorder()
######################## #
# Gráfico básico:
ggplot(cabbage_exp, aes(x=Date, y=Weight)) +
  geom_col() 
# Barras reordenadas por peso (de menor a mayor)
ggplot(cabbage_exp, aes(x=reorder(Date, Weight), y=Weight)) +
  geom_col() 
# Barras reordenadas por peso (de mayor a menor)
ggplot(cabbage_exp, aes(x=reorder(Date, -Weight), y=Weight)) +
  geom_col() 

# $$$ Ejercicio: Representa el cambio en los estados, ordenando las barras de mayor a menor cambio
# Sugerencia: usa la variable Abb en lugar de state para una mejor visualización
# Sugerencia2: usa la función reorder() para ordenar las barras
# Datos:
upc <- uspopchange %>%
  as_tibble() %>%
  subset(rank(Change)>40)
upc
ggplot(upc, aes(x=reorder(Abb, -Change), y=Change)) + 
  geom_col() 
# $$$ RESPUESTA:

# $$$ Ejercicio: ahora identifica con diferentes colores los estados del Sur y del Oeste
# $$$ RESPUESTA:
ggplot(upc, aes(x=reorder(Abb, -Change), y=Change, fill=Region)) + 
  geom_col() 

## *Controlar el ancho de las barras en geom_col()
################################## #
# $$$ *Ejercicio: modifica el ancho de las barras en el siguiente gráfico usando 
# el argumento width
# Sugerencia: width puede variar de 0 a 1 sin superposición de barras. Por defecto, width=0.9
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + 
  geom_col() 
# $$$ RESPUESTA:
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + 
  geom_col(width=0.5) 

## *Controlar el espacio entre barras agrupadas (geom_col(position=position_dodge()))
######################################################## #
# $$$ *Ejercicio: modifica el espacio entre las barras en el siguiente gráfico
# modificando position=position_dodge():
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position=position_dodge(0.9))
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position=position_dodge(0.5))

## CONFIGURACIÓN de estética en geom_col()
#################################### #
# $$$ *Ejercicio: define un color que te guste para todas las barras en
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col()
# $$$ RESPUESTA:
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(fill="pink")


# $$$ *Ejercicio: ahora define el color de la línea que rodea las barras
# $$$ RESPUESTA:
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(fill="pink", colour="black")

# $$$ *Ejercicio: Ahora haz que esta línea sea "discontinua" (Sugerencia: linetype)
# $$$ RESPUESTA:
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(fill="pink", colour="black", linetype="dashed", linewidth =0.5 )


## *MAPEO de estética en geom_col()
#################################### #
# Ya hemos visto varios... ¡pero hagamos otro!
# Datos:
csub <- climate %>%
  as_tibble() %>%
  subset(Source=="Berkeley" & Year >= 1900)

# $$$ **Ejercicio: quieres colorear valores positivos y negativos con colores diferentes
ggplot(csub, aes(x=Year, y=Anomaly10y)) +
  geom_col()
# Sugerencia: crea una variable lógica para identificar valores positivos y negativos y luego úsala 
# $$$ RESPUESTA:
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=Anomaly10y>0)) +
  geom_col()

library(tidyverse) # Metapaquete tidyverse del que usaremos ggplot2 para gráficos, dplyr para manipulación de datos, tidyr para reorganizar datos
library(gcookbook) # Bases de datos que vamos a usar
library(ggrepel)   # Para etiquetas que se solapan (funcionalidad interesante que no incluye ggplot de serie)
library(hexbin)    # Para mapas de frecuencia hexagonales (funcionalidad interesante que no incluye ggplot de serie)
library(patchwork) # Para figuras combinadas (funcionalidad interesante que no incluye ggplot de serie)


#***************************************************************************************
# BLOQUE 2.2.  - GRÁFICOS DE DISTRIBUCIÓN: gráficos de frecuencia y densidad
#***************************************************************************************
# * 7- Gráficos de barras de 
#     frecuencia e histogramas: geom_bar(), geom_histogram()   [FRECUENCIA]
# * 8- Polígono de frecuencia : geom_freqpoly()                [FRECUENCIA]
# * 9- Curva de densidad      : geom_density()                 [DENSIDAD]

# * 10- Diagrama de cajas     : geom_boxplot()                 [FRECUENCIA]
# * 11- Gráfico de violín     : geom_violin()                  [DENSIDAD]

# * 12- Mapa de frecuencia 2D : geom_bin2d(), geom_hex()       [FRECUENCIA]
# * 13- Mapa de densidad 2D   : geom_density_2d(), 
#                               geom_density_2d_filled, 
#                               stat_density_2d                [DENSIDAD]
#***************************************************************************************

# Gráfico de FRECUENCIA: asocia una frecuencia a cada valor de una variable (OBSERVADO)
# Gráfico de DENSIDAD  : asocia una probabilidad a cada valor de una variable (PREDICHO)

# Un gráfico de densidad calcula y dibuja la estimación de densidad de Kernel.
# https://en.wikipedia.org/wiki/Kernel_density_estimation

#***************************************************************************************
# * 7- Gráficos de barras de frecuencia e histogramas: geom_bar(), geom_histogram() [FRECUENCIA]
# * 8- Polígono de frecuencia :                        geom_freqpoly()              [FRECUENCIA]
# * 9- Curva de densidad      :                        geom_density()               [DENSIDAD]
#***************************************************************************************
############################################################################################# #  
#### 7 ### GRÁFICOS DE BARRAS DE FRECUENCIA E HISTOGRAMAS: geom_bar(), geom_histogram()  ####   
############################################################################################# #  
# geom_bar()/ geom_histogram: cuentan observaciones (frecuencia de valores en una variable)
# geom_col(): usa valores existentes (2 variables)

########### #
#### 7.1 ## Gráficos de barras de frecuencia: geom_bar() #### 
############################################################# #
# PARA VARIABLES CATEGÓRICAS

# Datos
diamonds

# Cada fila en el dataframe es un caso. El objetivo es representar el NÚMERO DE CASOS
# (es decir, la frecuencia) en cada nivel de una variable categórica (por ejemplo, la variable "cut") 
ggplot(diamonds, aes(x=cut)) + geom_bar()
# Observación: no estamos especificando "y", porque "y" será la frecuencia de casos en los niveles de x
# (ver título del eje y: "count")

# $$$ Ejercicio: Crea otro gráfico de barras de frecuencia de otra variable categórica en la base de datos de diamantes
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=color)) + geom_bar()


########### #
#### 7.2 ## Histogramas: geom_bar(), geom_histogram() #### 
########################################################## #
# PARA VARIABLES CONTINUAS

# $$$ Ejercicio: Crea un histograma de una variable continua en la base de datos de diamantes usando geom_bar()
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price)) + geom_bar()


# Observación: geom_bar() crea una barra por cada valor de x

# $$$ Ejercicio*: Confirma que el valor predeterminado es geom_bar(stat="count") en estos gráficos
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price)) + geom_bar(stat="count")


# $$$ Ejercicio: Transforma tu gráfico usando geom_bar(stat="bin")
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price)) + geom_bar(stat="bin")


# Observación: stat="bin" agrupa los valores de una variable continua en el eje x
# (en lugar de hacer una barra por cada valor de x, hace una barra para un rango de valores)

# $$$ Ejercicio: Repite el gráfico usando geom_histogram() en lugar de geom_bar()
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price)) + geom_histogram()


# Observación 1: geom_histogram es análogo a geom_bar(stat="bin")
# Observación 2: En geom_histogram(), x siempre es una variable continua
# Observación 3: geom_histogram() es más flexible para definir el número y ancho de las barras que geom_bar (ver abajo..)

# $$$ Ejercicio*: Representa la frecuencia de valores en la variable waiting usando geom_histogram()
faithful <- as_tibble(faithful)
faithful
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) + geom_histogram()

########### #
#### 7.3 ## Histogramas múltiples: agrupar barras considerando una segunda variable #### 
################################## #

## Barras superpuestas: geom_histogram(position="identity")
####################### #
heightweight <- as_tibble(heightweight)
heightweight
ggplot(heightweight, aes(x=heightIn)) +   
  geom_histogram(position="identity", alpha=0.4)
ggplot(heightweight, aes(x=heightIn, fill=sex)) +   
  geom_histogram(position="identity", alpha=0.4)

# $$$ Ejercicio: representa el precio de los diamantes para diferentes cortes,
# usando barras superpuestas (una capa por cada tipo de corte)
# ¿Cuántos niveles tiene el factor "cut"? ¿Es fácil ver la distribución de cada nivel?
diamonds
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price, fill=cut)) +
  geom_histogram(position="identity", alpha=0.4)

########### #
#### 7.4 ## Estética de los histogramas #### 
############################################ #

## Controlar el número de barras en geom_histogram()
########################################## #
# $$$ Ejercicio: Modifica el parámetro bins a continuación
ggplot(diamonds, aes(x=carat)) + geom_histogram(bins = 50)
# $$$ RESPUESTA:


## Controlar el ancho de las barras en geom_histogram()
######################################## #
# $$$ Ejercicio*: Modifica el parámetro binwidth a continuación
ggplot(diamonds, aes(x=carat)) + geom_histogram(binwidth = 0.5)
# $$$ RESPUESTA:


######################################################## #   
#### 8 ### POLÍGONOS DE FRECUENCIA: geom_freqpoly() ####   
######################################################## #
# Un polígono de frecuencia muestra la misma información que un histograma (frecuencia de cada valor),
# pero usa una línea para unir cada valor de frecuencia

########### #
#### 8.1 ## Polígono de frecuencia básico #### 
############################################## #

# $$$ Ejercicio: Adivina cómo completar el siguiente gráfico para hacer un polígono de frecuencia
ggplot(faithful, aes(x=waiting)) +
  geom_freqpoly()
# $$$ RESPUESTA:


# $$$ Ejercicio*: Usa el argumento "bins" o el argumento "binwidth" para definir la precisión (como en el histograma)
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) +
  geom_freqpoly(binds=1)

########### #
#### 8.2 ## Polígono de frecuencia múltiple ####
################################################ #

# $$$ Ejercicio: ¿Recuerdas este gráfico? - Transfórmalo en un gráfico de polígono de frecuencia
ggplot(diamonds, aes(x=price, fill=cut)) + 
  geom_histogram(position="identity", alpha=0.3)
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price, colour=cut)) + 
  geom_freqpoly(position="identity", alpha=0.3)

# $$$ Ejercicio*: Representa la frecuencia de valores de heightIn para cada sexo, usando un polígono
str(heightweight)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, colour=sex)) +
  geom_freqpoly()

########### #
#### 8.3 ## Estética del polígono de frecuencia ####
############################################# #

## CONFIGURACIÓN de estética
# $$$ Ejercicio*: Modifica el color y el tipo de línea
ggplot(faithful, aes(x=waiting)) + 
  geom_freqpoly(bins=60, colour= "blue", linetype="dashed")
# $$$ RESPUESTA:


############################################################################# #
#### 9 ### CURVA DE DENSIDAD: geom_density() o geom_line(stat="density") ####   
############################################################################# #

############ #
#### 9.1. ## Curva de densidad básica ####
########################################## #
# geom_density()
ggplot(faithful, aes(x=waiting)) + 
  geom_density()

# geom_line(stat="density")
ggplot(faithful, aes(x=waiting)) + 
  geom_line(stat="density")

############ #
#### 9.2. ## **Curva de densidad escalada ####
############################################ #
# geom_density() escalado a una escala proporcional (rango=0-1)
ggplot(faithful, aes(x=waiting)) + 
  geom_density(aes(y=stat(scaled)))

############ #
#### 9.3. ## **Comparación de curvas observadas y estimadas ####
############################################################## #
# $$$ Ejercicio**: Intenta superponer las distribuciones observadas y estimadas de la variable "waiting"
# en la base de datos "faithful".
# Probablemente tendrás problemas :)
# Colorea cada distribución con un color diferente para entender el problema.
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(alpha=0.2) +
  geom_density(aes(y=stat(scaled)))

# Observación: geom_density() representa probabilidades, por lo que el área bajo geom_density() suma 1. 
# Como consecuencia, los valores del eje y son muy pequeños (mucho menores que los valores observados),
# por lo que si graficamos distribuciones observadas y estimadas juntas, no veremos variación en la curva de densidad.

# $$$ Ejercicio**: Inténtalo de nuevo.
# Consejo: reescala el eje y del gráfico de frecuencia a la escala del eje y en el gráfico estimado.
# Para ello, define aes(y=stat(density)) en el gráfico de frecuencia.
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting, y=stat(density))) + 
  geom_histogram(alpha=0.5) +
  geom_density(colour="red")

# Observación: stat(density) es una "variable calculada" o una "estética calculada" 
# de geom_histogram() y geom_density(). 
# Es una nueva variable creada por geom_histogram() y geom_density() y depende de los datos originales. 
# Ver "Computed variables" en ?geom_density y ?geom_histogram.
# Ver también: https://ggplot2.tidyverse.org/reference/#stats
# Ambas son estimaciones de densidad escaladas para integrar 1. Una es continua y la otra por intervalos.
# En este ejemplo, el gráfico está usando la de geom_histogram().
# Esta es la forma más utilizada para superponer, pero hay otras opciones...

# $$$ Ejercicio**: Ahora reescala el eje y de la curva de densidad a la escala de conteos.
# Consulta la ayuda de geom_histogram() e intenta entender por qué el gráfico es ligeramente diferente al anterior.
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(alpha=0.5) +
  geom_density(aes(y=stat(count)))
?geom_histogram

# Observación: count en geom_density() es densidad * número de puntos,
# mientras que count en geom_histogram() es el número de puntos.

# $$$ Ejercicio**: Ahora reescala ambos ejes y a un máximo de 1.
# Consulta la ayuda de geom_histogram() y geom_density().
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(alpha=0.5, aes(y=stat(density))) +
  geom_density()

ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(aes(y=stat(ncount))) +
  geom_density(aes(y=stat(scaled)))

# $$$ Ejercicio**: Repítelo usando el otro gráfico de frecuencia 
# (si usaste histograma, ahora usa freqpoly).
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) + 
  geom_freqpoly(alpha=0.5, aes(y=stat(density))) +
  geom_density()

############ #
#### 9.4. ## Curvas de densidad múltiples ####
############################################## #

## Curvas de densidad superpuestas: por defecto
######################################## #
# Datos:
heightweight <- as_tibble(heightweight)
heightweight

# $$$ Ejercicio: representa la curva de densidad de heightIn para cada sexo
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, fill=sex)) +
  geom_density(alpha=0.4)

# $$$ Ejercicio**: ¿Recuerdas este histograma?:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_histogram(position="identity", alpha=0.4)
# Ahora superpone el histograma con curvas de densidad para cada sexo
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_histogram(position="identity", alpha=0.4) +
  geom_density(alpha=0.4, aes(y=scaled))

## Curvas de densidad apiladas: geom_density(position="stack")
########################################################## #
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_density(alpha=0.4)

# $$$ Ejercicio: Crea una versión apilada del gráfico anterior, usando el argumento position="stack"
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_density(alpha=0.4, position="stack")

# $$$ Ejercicio: Ahora crea una versión apilada proporcional, usando el argumento position="fill"
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_density(alpha=0.4, position="fill")

############ #
#### 9.5. ## Estética de las curvas de densidad ####
#################################################### #

## Nivel de ajuste
#################### #
# La estimación puede estar más o menos ajustada a los datos

# $$$ Ejercicio: Modifica el valor del argumento adjust (adjust=1 por defecto)
# en los siguientes gráficos
ggplot(faithful, aes(x=waiting)) +
  geom_line(stat="density", adjust=0.1) 
ggplot(faithful, aes(x=waiting)) +
  geom_density(adjust=0.2) 

# $$$ Ejercicio**: Modifica el argumento adjust en la curva de densidad para encontrar el mejor ajuste
# al polígono de frecuencia
ggplot(faithful, aes(x=waiting)) + 
  geom_freqpoly(aes(y=stat(density))) +
  geom_density(colour="red", adjust=0.22)
# $$$ RESPUESTA:


## Configuración de estética: fill, colour, alpha
###################### #

# $$$ Ejercicio: usa fill, colour y alpha para modificar el siguiente gráfico según se indica:
ggplot(faithful, aes(x=waiting)) +
  geom_density() 
# Área azul transparente y línea azul encima
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) +
  geom_density(fill="blue")  

# *Solo área azul transparente
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) +
  geom_density(fill="blue", colour=NA, alpha=.2)


## Límites del eje x 
##################### #
# $$$ *Ejercicio: amplía los límites del eje x para ver toda la curva de densidad
# Pista: xlim()
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) +
  geom_density(fill="blue", alpha=.2) +
  xlim(35, 105)

#***************************************************************************************
# * 10- Boxplot          : geom_boxplot()                 [FRECUENCIA]
# * 11- Gráfico de violín: geom_violin()                  [DENSIDAD]
#***************************************************************************************
######################################## #
#### 10 ### BOXPLOT: geom_boxplot() ####
######################################## #

############# #
#### 10.1. ## Boxplot múltiple ####
################################### #
# Distribución de una variable continua,
# en grupos definidos por intervalos de una segunda variable
# o en grupos definidos por los niveles de una variable categórica
heightweight
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_boxplot()

# Recordando: Elementos de un boxplot (del centro hacia afuera):
# * LÍNEA: Mediana (percentil 50)
# * CAJA: Rango intercuartil (IQR) (percentiles 25 y 75)
# * BIGOTES: Desde el IQR hasta los valores más grandes/pequeños o hasta 1.5*IQR
# * PUNTOS: Valores atípicos (más allá de 1.5*IQR)

# Para entender los boxplots...
# https://en.wikipedia.org/wiki/Box_plot
# https://towardsdatascience.com/the-box-plot-guide-i-wish-i-had-when-i-started-learning-r-d1e9705a6a37/
# https://towardsdatascience.com/understanding-boxplot-infinity-gauntlet-of-the-dataverse-cd57cd067711/

# Krzywinski, M., & Altman, N. (2014). Visualizing samples with box plots: use box plots to illustrate the spread and differences of samples. Nature Methods, 11(2), 119-121.
# https://www.nature.com/articles/nmeth.2813.pdf?origin=ppub


# $$$ Ejercicio: representa el precio de los diamantes en función de 
# las clases de corte usando un boxplot
diamonds
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot()

############ #
#### 10.2.## Boxplot de un solo grupo ####
########################################## #
ggplot(heightweight, aes(x=1, y=ageYear)) +
  geom_boxplot()
# Observación: geom_boxplot() necesita que el eje x esté definido.
# Si no nos interesa el eje x, podemos definir cualquier valor 
# (por ejemplo, x=1) y luego ocultar las etiquetas del eje x
ggplot(heightweight, aes(x=1,y=ageYear)) +
  geom_boxplot() + 
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())

############ #
#### 10.3.## Boxplot + media ####
################################# #
# $$$ Ejercicio: incluye un punto que represente la media en el siguiente boxplot
# Pista: stat_summary(fun="mean", geom="point") 
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot() +
  # $$$ RESPUESTA:
  ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot() +
  stat_summary(fun="mean", geom="point") 

# $$$ Ejercicio*: verifica que el boxplot realmente representa la mediana
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot() +
  stat_summary(fun="mean", geom="point") 


############ #
#### 10.4.## Estética del Boxplot ####
###################################### #

## Enfatizar la mediana: notch=TRUE
######################## #
# Las muescas son útiles para dar una guía aproximada sobre la diferencia significativa entre medianas;
# Las muescas de dos cajas que no se superponen evidencian una diferencia estadísticamente significativa entre las medianas.
# El ancho de las muescas es proporcional al rango intercuartil (IQR) de la muestra e inversamente proporcional a la raíz cuadrada del tamaño de la muestra.
# https://en.wikipedia.org/wiki/Box_plot
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_boxplot(notch=TRUE) #muesca = rango intercuartilico. Más datos, más pequeña es

## Ancho de la caja: width
############# #
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_boxplot(width=0.2)

## Sin valores atípicos: outlier.colour=NA
#################### #
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot()
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot(outlier.colour=NA)         # Sin valores atípicos
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot(outlier.colour="grey70")   # ... o valores atípicos en un color más claro...
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot(outlier.alpha=0.2)        # ... o valores atípicos con mayor transparencia...

## Configuración de estética
###################### #
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_boxplot(notch=TRUE, colour="blue", fill="violet", linetype=4)


################################################# #
#### 11 ### GRÁFICO DE VIOLÍN: geom_violin() ####
################################################# #

############# #
#### 11.1. ## Gráfico de violín básico ####
########################################### #
# Estimar distribuciones
# Podemos entenderlo como dos curvas de densidad reflejadas
# Útil para comparar múltiples distribuciones de datos (fácil de visualizar)

ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_violin(colour="brown", fill="orange")
ggplot(heightweight, aes(y=ageYear,x=1)) +
  geom_violin(colour="brown", fill="orange")

# $$$ Ejercicio: representa el precio de los diamantes en las clases de corte usando un gráfico de violín
diamonds
str(diamonds)
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin(fill="pink")

############ #
#### 11.2.## Violín + boxplot ####
################################## #

# $$$ Ejercicio: superponer las distribuciones estimadas y observadas
# (incluyendo la media)
# Consejo 1: ajusta el ancho del boxplot (piensa en el boxplot como los "trastes" del violín)
# Consejo 2: podrías querer omitir los valores atípicos del boxplot
# Consejo 3: podrías querer colorear la caja del boxplot en negro
# Consejo 4: ¡necesitas pensar en el orden de las capas!
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin(fill="pink") +
  geom_boxplot(width=0.09, outlier.colour = NA, fill= "black") +
  stat_summary(fun="mean", geom="point", colour="red")

############ #
#### 11.3.## Estética del gráfico de violín ####
################################### #

## *Límites: trim
########## #
# $$$ *Ejercicio: usa trim=FALSE en geom_violin().
# Busca en la ayuda qué hace trim y cuál es su valor por defecto.
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_violin(colour="brown", fill="orange")
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_violin(colour="brown", fill="orange", trim=FALSE)
?geom_violin

## *Escala
########## #
# $$$ *Ejercicio: busca en la ayuda qué hace el argumento "scale" y
# prueba diferentes valores en el siguiente gráfico
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin(colour="purple", fill="lightblue")
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin(colour="purple", fill="lightblue", scale= "width") #mismo ancho

ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin(colour="purple", fill="lightblue", scale= "count") #propocrional al tamaño muestral
## Ajuste
########## #
# $$$ Ejercicio: modifica el argumento adjust en el siguiente gráfico y adivina qué hace:
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_violin(colour="brown", fill="orange", adjust=0.02)
# $$$ RESPUESTA:



#***************************************************************************************
# * 12- Mapa de frecuencia 2D : geom_bin2d(), geom_hex()                 [FRECUENCIA]
# * 13- Mapa de densidad 2D   : geom_density_2d(), 
#                               geom_density_2d_filled, stat_density_2d  [DENSIDAD]
#***************************************************************************************
################################################################### #
#### 12 ### MAPA DE FRECUENCIA EN 2D: geom_bin2d(), geom_hex() ####
################################################################### #
# Equivalente a un histograma que representa dos variables a la vez

############# #
#### 12.1. ## Mapa de frecuencia 2D básico ####
######################################### #
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_bin2d()
# Observación: el color representa la "altura de la barra", como si fuera 3D

# $$$ Ejercicio: repite el gráfico con celdas hexagonales 
# (Sugerencia: geom_hex() del paquete {hexbin})
# $$$ RESPUESTA:
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_hex()

## Dos variables continuas
############################ #
# $$$ Ejercicio: representa el mapa de frecuencia del peso en quilates y el precio de los diamantes
str(diamonds)
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_hex()

# Observación: ¿recuerdas? geom_bin2d() es una alternativa para
# la superposición de puntos en un diagrama de dispersión

## Una variable continua y una categórica
############################################### #
# $$$ *Ejercicio: representa el mapa de frecuencia del color y el precio de los diamantes
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=color, y=price)) +
  geom_tile()

## Dos variables categóricas
############################## #
# $$$ Ejercicio: representa el mapa de frecuencia de la claridad y el color de los diamantes
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=color, y=clarity)) +
  geom_count()


############# #
#### 12.2. ## Estética del mapa de frecuencia 2D ####
##################################################### #
# $$$ Ejercicio: prueba a cambiar el argumento bins
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_bin2d(bins=80)
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_hex(bins=20)

# $$$ Ejercicio: ¿tiene sentido modificar el argumento bins en un gráfico de
# dos variables categóricas?
# $$$ RESPUESTA: 
#No, porque los niveles del factor ya están definidos

####################################################################################################### #
#### 13 ### MAPA DE DENSIDAD EN 2D: geom_density_2d(), geom_density_2d_filled(), stat_density_2d() ####
####################################################################################################### #
# Valores estimados (función de densidad) en función de dos variables predictoras

############# #
#### 13.1. ## Mapa de densidad 2D básico ####
############################################# #

## Opción 1: Curvas de nivel: geom_density_2d()
######################### #
# ¿Ves algún patrón en esta nube de puntos?
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_point()

# ¿Cuál es la probabilidad de que un nuevo punto caiga en el espacio del gráfico?
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_point() + 
  geom_density_2d()
# Observación: puedes entenderlas como curvas de nivel en un mapa topográfico:
# la "altitud" depende de la densidad de puntos y define la probabilidad 
# de que un nuevo punto caiga allí

## Opción 2: Curvas de nivel rellenas: geom_density_2d_filled() 
######################### #
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_point() + 
  geom_density_2d_filled()
# Observación: son como las curvas de nivel de antes pero con los intervalos 
# entre curvas coloreados según la "altitud"

## Opción 3: Mapa ráster: stat_density_2d
######################### #
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  stat_density_2d(aes(fill=stat(density)), geom="raster", contour=FALSE)
# Observación: el espacio entre las dos variables se divide en celdas,
# y la densidad de puntos (i.e., la probabilidad de que un nuevo punto 
# caiga en la celda) se define por el color

# $$$ *Ejercicio: modifica el gráfico anterior para que la densidad de 
# puntos esté representada por la transparencia en lugar de por el color
# $$$ RESPUESTA:
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  stat_density_2d(aes(alpha=stat(density)), geom="raster", contour=FALSE, fill="pink")

############# ########colour = ###### #
#### 13.2. ## Mapa de frecuencia + mapa de densidad ####
######################################################## #
# $$$ *Ejercicio: Encuentra una forma adecuada de superponer mapas de frecuencia y densidad
# en el siguiente gráfico:
ggplot(faithful, aes(x=eruptions, y=waiting))
# $$$ RESPUESTA: 
ggplot(faithful, aes(x=eruptions, y=waiting)) +
  stat_density_2d(aes(alpha=stat(density)), geom="raster", contour=FALSE, fill="red") +
  geom_bin2d()



############# #
#### 13.3. ## Estética del mapa de densidad 2D ####
################################################### #

## Colorear curvas de nivel por nivel (es decir, por "altitud de montaña")
###################################### #
# $$$ *Ejercicio: Colorea las curvas de nivel por nivel en el siguiente gráfico
# Consejo: el nivel de las curvas de nivel es una nueva variable estimada por 
# geom_density_2d, que puede ser llamada como stat(level)   
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  stat_density_2d(aes(fill=stat(level)), size=2, colour="pink") 

# $$$ RESPUESTA:


## Tamaño de las curvas de nivel (hacerlas más gruesas para ver el color)
################################# #
# $$$ *Ejercicio: aumenta el argumento size para una mejor visualización del color
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), size=0.5)
# $$$ RESPUESTA:
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), size=5)

## Ajuste
########## #
# Recuerda: las funciones de densidad implican un nivel de ajuste a los datos

# $$$ Ejercicio: modifica los valores del argumento adjust en este gráfico
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), adjust=c(0.5,0.1))
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), adjust=0.5)

# Observación 1:
# adjust = 1	suavizado automático de ggplot
# adjust < 1	menos suavizado (más detalle)
# adjust > 1	más suavizado (menos detalle)
# Observación 2: se puede ajustar cada eje por separado o los dos a la vez

# $$$ *Ejercicio: Cambia el ajuste de los dos ejes a la vez en este gráfico
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_point() + 
  geom_density_2d_filled()
# $$$ RESPUESTA:


# $$$ *Ejercicio: Cambia el ajuste de cada eje en este gráfico
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  stat_density_2d(aes(fill=stat(density)), 
                  geom="raster", contour=FALSE)
# $$$ RESPUESTA:
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  stat_density_2d(aes(fill=stat(density)), 
                  geom="raster", contour=FALSE, adjust=0.5)

#**************************************
############## #
#### BLOQUE 3  -  CAPAS OPCIONALES: facet_(), stat_(), scale_(), coord_(), labs(), annotate(), theme(), y más  ####
############## #
#**************************************

#################################################### #
#### 15 ### FACETAS: facet_grid(), facet_wrap() #### 
#################################################### #
# Reglas para crear gráficos múltiples
# Objetivo: dividir un gráfico agrupado para obtener un gráfico por grupo
# datos:
mpg
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
p
# Gráfico agrupado por la variable drv:
d <- ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point() 
d
# Gráfico agrupado por la variable cyl:
c <- ggplot(mpg, aes(x=displ, y=hwy, color=factor(cyl))) + geom_point()
c

############# #
#### 15.1. ## Facetas en líneas o columnas ####
############################################### #

## Facetas verticales: una variable, gráficos en líneas: facet_grid(x~.)
###################### #
p + facet_grid(drv ~ .) #capa de tipo facet

## Facetas horizontales: una variable, gráficos en columnas: facet_grid(.~x)
###################### #
c + facet_grid(. ~ cyl)

############# #
#### 15.2. ## Facetas en una cuadrícula ####
############################################ #

## Facetas en una cuadrícula con 2 variables: facet_grid(x~z)
############################################# #
# Una variable define las líneas y la otra define las columnas
p + facet_grid(year ~ class)
p + facet_grid(drv ~ cyl)

## Facetas en una cuadrícula con 1 variable: facet_wrap( ~z)
############################################ #
p + facet_wrap( ~ class)
p + facet_wrap( ~ trans)
p + facet_wrap( ~ class)

# Se puede definir el número de líneas o columnas:
p + facet_wrap( ~ class, nrow=2)
p + facet_wrap( ~ class, ncol=2)

# $$$ Ejercicio: Haz otro gráfico usando la base de datos mpg e incluyendo facetas.
# $$$ RESPUESTA:
y <- ggplot(mpg, aes(x=class, y=hwy, color=factor(cyl))) + geom_point()
y + facet_grid(manufacturer ~ cyl)
y + facet_wrap( ~ hwy)

############# #
#### 15.3. ## Facetas con ejes flexibles: scales="free_x", scales="free_y", scales="free" ####
######################################## #
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
p
p + facet_grid(drv ~ cyl) # Por defecto todos los ejes son iguales (están "fijos")

# Eje y libre
p + facet_grid(drv ~ cyl, scales="free_y")
# Eje x libre
p + facet_grid(drv ~ cyl, scales="free_x")
# Ejes x e y libres
p + facet_grid(drv ~ cyl, scales="free")

# $$$ Ejercicio: Maximiza la variación de los puntos en el eje (o ejes) que consideres 
# más necesario en este gráfico
p + facet_wrap( ~ class, ncol=2)
# $$$ RESPUESTA:
p + facet_wrap( ~ class, ncol=2, scales="free")

############################################# #
#### 16 ### *CAPAS ESTADÍSTICAS: stat_() ####
############################################# #
# Estadísticas de los datos a ser representadas

############# #
#### 16.1. ## **Funciones sin datos: stat_function() ####
####################################################### #

## Graficar funciones estándar 
########################### #
dat <- tibble(ex = c(-5, 5))  # ejemplo (ex) solo para especificar el rango
dat

# Gaussiana: dnorm
ggplot(dat, aes(x=ex)) + 
  stat_function(fun = dnorm)
ggplot(dat, aes(x=c(-5,5))) + 
  stat_function(fun = dnorm)

# Logarítmica: log
ggplot(dat, aes(x=ex)) +
  stat_function(fun = log)

# Exponencial: exp
ggplot(dat, aes(x=ex)) +
  stat_function(fun = exp)

# Seno y Coseno: sin, cos
ggplot(dat, aes(x=ex)) +
  stat_function(fun = sin, colour = "red") +
  stat_function(fun = cos, colour = "blue")

## *Definir los parámetros de la función (args =   )
#################################################### #
# $$$ *Ejercicio: modificar la media y la desviación estándar de la siguiente función gaussiana
ggplot(dat, aes(x=ex)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1))
# $$$ RESPUESTA:
ggplot(dat, aes(x=ex)) +
  stat_function(fun = dnorm, args = list(mean = 2, sd = 0.2))

## **Funciones personalizadas
############################ #
ggplot(dat, aes(x=ex)) +
  stat_function(fun=function(x, alpha, beta) {alpha + beta*x + 2*x^2} , args=list(alpha=2, beta=3))  # esto es solo un ejemplo

## **Comparar la función con datos simulados
############################################ #
df <- tibble(simul = rnorm(100)) # datos aleatorios siguiendo una distribución gaussiana
df
ggplot(df, aes(x=simul)) + 
  geom_histogram()
ggplot(df, aes(x=simul)) + 
  stat_function(fun = dnorm, colour = "red")

# $$$ **Ejercicio: comparar stat_function(dnorm) con la curva de densidad de simul
# (es decir, representarlas juntas)
# $$$ RESPUESTA:
ggplot(df, aes(x=simul)) + 
  geom_density(colour="green") + 
  stat_function(fun = dnorm, colour = "red")
ggplot(df, aes(x=simul)) +
  geom_histogram(aes(y=after_stat(density))) + 
  geom_density(colour="green") + 
  stat_function(fun = dnorm, colour = "red")

############# #
#### 16.2. ## stat_summary() ####
################################# #
# ¿Lo recuerdas?
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot() +
  stat_summary(fun="median", geom="point", colour="red") +
  stat_summary(fun="mean", geom="point", colour="blue")

# También podríamos definir una función para los valores mínimo y máximo:
ggplot(diamonds, aes(x=cut, y=price)) +
  stat_summary(fun = mean, fun.min = min, fun.max = max) 
# Media ± error estándar (https://es.wikipedia.org/wiki/Error_est%C3%A1ndar)
ggplot(diamonds, aes(x=cut, y=price)) +
  stat_summary() # por defecto: geom = "pointrange"; es decir, mean_se()
# Veremos otras barras de error con geom_errorbar() (apartado 20.5)

################################## #
#### 17 ### ESCALAS: scale_() ####
################################## #
# Reglas de mapeo en relación con la escala
# Ver viñeta de especificación estética en: vignette("ggplot2-specs") 
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html

############# #
#### 17.1 ### PALETAS DE COLORES:  scale_fill_(), scale_colour_() ####
################################# #
## Nota: ¡todas las siguientes funciones funcionan tanto con fill como con colour!

############# #
## 17.1.1. ## Paletas predefinidas para variables discretas  
############################################################ #
d <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()

## Paletas por defecto
####################### #
# Escalas de color predeterminadas para variables categóricas.
# Asigna cada nivel a un tono equidistante en la rueda de colores.
# No son paletas seguras para personas con daltonismo.

# Paletas por defecto (las tres primeras dan el mismo resultado):
d
d + scale_fill_discrete()
d + scale_fill_hue()       
d + scale_fill_brewer()

## Paleta ColorBrewer (http://colorbrewer.org.)
###################### #
# Por defecto
d + scale_fill_brewer()

# Paletas disponibles en ColorBrewer:
# install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

# Las paletas de ColorBrewer se llaman por su nombre:
d + scale_fill_brewer(palette="Oranges")
d + scale_fill_brewer(palette="Paired")
d + scale_fill_brewer(palette="Spectral")
d + scale_fill_brewer(palette="YlOrRd")

## Paletas seguras para personas con daltonismo
################################################ #
# Escala segura para daltonismo en variables discretas
d + scale_fill_viridis_d()

## Escala de grises
################ #
d + scale_fill_grey()

## Invertir la dirección de la paleta y/o usar un rango específico de ella
################################################################ #
d + scale_fill_grey()
# $$$ Ejercicio: Modifica los argumentos "start" y "end" de 0 a 1
# $$$ RESPUESTA:
d + scale_fill_grey(start=0.1, end=0.4)
d + scale_fill_grey(start=0.7, end=0)
d + scale_fill_grey(start=0.1, end=0.9)


############# #
## 17.1.2. ## Paletas predefinidas para variables continuas  
############################################################ #
c <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) +
  geom_point(size=3)
c

## Paletas por defecto
##################### #
c
c + scale_colour_continuous()
c + scale_colour_gradient()

## Paletas seguras para personas con daltonismo
######################### #
# Daltonismo en variables continuas
c + scale_colour_viridis_c()

############# #
## 17.1.3. ## Crear paletas para variables discretas manualmente  
############################################################## #
dm <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
dm

## 1 - Observar el número de niveles y su orden
length(levels(heightweight$sex))
levels(heightweight$sex)

## 2 - Opción 1: Definir la paleta siguiendo el orden de los niveles:
# a) Usando nombres de colores
dm + scale_colour_manual(values=c("orange", "purple"))

# b) Usando códigos RGB
dm + scale_colour_manual(values=c("#CC6666", "#7777DD"))

## 2 - Opción 2: Definir el color para cada nivel del factor:
dm + scale_colour_manual(values=c(m="violet", f="red"))

## Paleta de colores
#################### #
# Ver ColourChart (nombres y códigos de colores) en:
# https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html

## Adobe Color
################ #
# https://color.adobe.com/

## My Color Space
################## #
# Buscar combinaciones de colores atractivas:
# https://mycolor.space/   # Paletas discretas
# https://mycolor.space/gradient  # Gradientes de 2 colores
# https://mycolor.space/gradient3 # Gradientes de 3 colores

## ColourPicker
################# #
install.packages("colourpicker")
# Ver en Addins > Colour Picker

## Paletas manuales seguras para personas con daltonismo  
################################## #
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
cb_palette2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")

#c("#3EB9C2 "#FFc)("#8BBD52", "#FFFFFF", "#FFFFFF")FFFF", "#FFFFFF")d + scale_fill_manual(values=cb_palette)
d + scale_fill_manual(values=cb_palette2)

############# #
## 17.1.4. ## Crear paletas para variables continuas manualmente  
################################################################# #
# Gradiente con 2 colores
c + scale_colour_gradient(low="black", high="white")

# Gradiente con 3 colores: uno intermedio y dos extremos
c + scale_colour_gradient2(low="red", mid="white", high="blue",
                           midpoint=100) # ¡puedes establecer el valor intermedio!

# Gradiente con n colores, equitativamente espaciados
c + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))

# $$$ Ejercicio: ¡Crea tu propia paleta! - puedes usar la paleta de colores, colour picker 
# o "My Color Space" para elegir los colores
# $$$ RESPUESTA:
c + scale_colour_gradientn(colours = c("pink", "orange", "purple", "white"))

c("#FFFFFF", "#FFFFFF", "#FFFFFF")############# #
## 17.1.5. ## Cambiar la luminosidad, saturación y rango de matices del color
################################################################ #
# l = luminosidad (light)
# c = saturación (chroma)
# h = matiz (hue)

## Luminosidad del color (light): 0 a 100 (por defecto: l = 65)
##################### #
p <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
p
# $$$ Ejercicio: cambiar el parámetro de luminosidad
p + scale_fill_hue(l=65)
# $$$ RESPUESTA:
p + scale_fill_hue(l=30)


## Saturación del color (chroma) (por defecto: c = 100) (el máximo depende del matiz y de la luminosidad)
################## #
# $$$ Ejercicio: cambiar el parámetro de saturación
p + scale_fill_discrete(c=100)
# $$$ RESPUESTA:
p + scale_fill_discrete(c=80)


## Rango de matices (hue): 0 a 360 (por defecto: h = c(0,360))
#################### #
# $$$ Ejercicio: cambiar el rango de matices
p + scale_fill_discrete(h=c(0,360))
# $$$ RESPUESTA:
p + scale_fill_discrete(h=c(0,250))


############# #
#### 17.2 ### PALETAS PARA OTRAS ESTÉTICAS: scale_alpha_(), scale_linetype_(), scale_shape() ####
########################################### #

############# #
## 17.2.1. ## Escalas de transparencia: scale_alpha_()
####################################### #
ct <- ggplot(heightweight, aes(x=ageYear, y=heightIn, alpha=weightLb)) +
  geom_point(size=3)
ct
## Rango de alpha
############### #
ct + scale_alpha(range = c(0.2, 1))

############# #
## 17.2.2. ## Escalas de tipo de línea: scale_linetype_()
####################################### #
dl <- ggplot(uspopage, aes(x=Year, y=Thousands, linetype=AgeGroup)) + 
  geom_line()

# Los tipos de línea disponibles en R son:
# "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash".

dl + scale_linetype_manual(values=c("11","22","33","44","55","66","77","88"))
dl + scale_linetype_manual(values=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "solid", "dashed"))

############# #
## 17.2.3. ## Escalas de forma: scale_shape()
################################ #
dms <- ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) + geom_point()
dms

dms + scale_shape_manual(values = c(25,1))

############# #
## 17.2.4. ## Escalas de tamaño: scale_size(), scale_radius(), scale_size_area()
################################# #

# Continuo
cmss <- ggplot(mpg, aes(displ, hwy, size = hwy)) +
  geom_point()

cmss + scale_size(range = c(0, 10))    # define el tamaño como área
cmss + scale_radius(range = c(0, 10))  # define el tamaño como radio
cmss + scale_size_area(max_size=10)    # define el tamaño como área, asegurando que el valor 0 = área 0

# Discreto (ver aviso)
dmss <- ggplot(heightweight, aes(x=ageYear, y=heightIn, size=sex)) + geom_point(shape=1)
dmss

dmss + scale_size_manual(values=c(4, 2))  # define el tamaño como área

############# #
#### 17.3 ### MANIPULAR EJES usando scale_() y otras funciones análogas ####
############################## #
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p
############# #
## 17.3.1. ## Límites de los ejes: xlim(), ylim(), scale_y_continuous(),
################################ # scale_x_continuous(), expand_limits()
p + scale_y_continuous(limits=c(0,10)) #scale no es solo para aes
p + ylim(0, 10)
p + expand_limits(y=0:10)
#depende del momento usamos unas u otras

############# #
## 17.3.2. ## Dirección de los ejes
############################# #

## Invertir la dirección de los ejes continuos: scale_x_reverse(), scale_y_reverse()
##################################### #
p + scale_y_reverse()

## Invertir la dirección de los ejes categóricos: scale_x_discrete(limits=rev(levels()))
##################################### #
p
p + scale_x_discrete(limits=rev(levels(PlantGrowth$group)))

## Definir manualmente la dirección de los ejes categóricos: scale_x_discrete(limits=)
############################################## #
p
p + scale_x_discrete(limits=c("trt1","trt2","ctrl"))

############# #
## 17.3.3. ## Seleccionar niveles de ejes categóricos: scale_x_discrete(limits=), 
#################################################### # scale_y_discrete(limits=)
p
p + scale_x_discrete(limits=c("ctrl","trt1"))

############# #
## 17.3.4. ## Transformación de la escala de los ejes: scale_x_continuous(trans=), 
#################################################### # scale_y_continuous(trans=)
p
p + scale_y_continuous(trans = "log")
p + scale_y_continuous(trans = "log10")
p + scale_y_continuous(trans = "exp")
p + scale_y_log10() # ya vimos este antes

############# #
## 17.3.5. ## Intervalos en los ejes: scale_y_continuous(),
################################### # scale_x_continuous()
sp <- ggplot(marathon, aes(x=Half,y=Full)) + geom_point()

## Definir intervalos
################# #
sp
sp + scale_y_continuous(breaks=seq(0, 420, 30)) +  # de 0 a 420, cada 30
  scale_x_continuous(breaks=seq(0, 420, 30))

sp + scale_y_continuous(breaks=c(150,200,250,300)) #le decimos los números del eje que queremo

## Eliminar intervalos
####################### #
sp
sp + scale_y_continuous(breaks=NULL)+
  scale_x_continuous(breaks=NULL)

############# #
## 17.3.6. ## Marcas en los ejes 
################################# #

## Posición de las marcas: scale_y_continuous(breaks=),
######################## # scale_x_continuous(breaks=)
p + scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6, 8))

## Etiquetas de las marcas: scale_y_continuous(labels=),
######################### # scale_x_continuous(labels=)
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
  geom_point()
hwp
hwp + scale_y_continuous(breaks=c(50, 56, 60, 66, 72),
                         labels=c("Pequeño", "Realmente\nbajo", "Bajo",
                                  "Mediano", "Algo alto"))

############# #
## 17.3.7. ## Títulos de los ejes: scale_y_continuous(name=), 
################################ # scale_x_continuous(name=),
#                                  labs(x = , y = ),
#                                  xlab(), ylab()
hwp + scale_y_continuous(name = "Altura (pulgadas)") # 

hwp + labs(x="Edad en años", y="Altura en pulgadas")

hwp + xlab("Edad en años") + 
  ylab("Altura en pulgadas")


####################################### #
#### 18 ### COORDENADAS: coord_()  ####
####################################### #
# Espacio del gráfico

############# #
#### 18.1. ## Invertir ejes x/y: coord_flip() ####
################################# #
# $$$ Ejercicio: Intercambia los ejes en el siguiente gráfico
p 
# $$$ RESPUESTA:
p + coord_flip()


############# #
#### 18.2. ## Gráficos circulares: coord_polar() ####
################################### #
# $$$ Ejercicio: Transforma el siguiente histograma en un gráfico circular
cir <- ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15) +
  scale_x_continuous(limits=c(0,360))
cir
# $$$ RESPUESTA:
cir <- ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15) +
  scale_x_continuous(limits=c(0,360)) +
  coord_polar() #lo hace circular
cir


### Ver también la extensión de ggplot: {ggradar} https://exts.ggplot2.tidyverse.org/ggradar.html

############# #
#### 18.3. ## Ejes x/y en la misma escala: coord_fixed() ####
########################################## #
sp
sp + coord_fixed()

############################################ #
#### 19 ### TÍTULOS Y ETIQUETAS: labs() ####
############################################ #
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + 
  geom_point()

p + labs(title = "Título del gráfico",
         subtitle = "Subtítulo del gráfico",
         x = "Título del eje x",
         y = "Título del eje y",
         tag = "B", 
         caption = "Figura 1. Aquí puedes insertar una nota o pie de figura.", 
         colour = "Título de la leyenda") +  
  scale_colour_discrete(name= "Título de la leyenda",        
                        labels=c("Grupo 1", "Grupo 2"))

## Observación 1: el título de la leyenda se puede especificar en labs o en scale_
## Observación 2: En este caso, la variable en la leyenda fue especificada por colour.
## Necesitarás especificar el aes usado para la variable de la leyenda (ya sea en labs() o
## en scale_()).

# Existe una fórmula rápida alternativa para los títulos de los ejes:
p + 
  xlab("Título del eje x") +
  ylab("Título del eje y")

# $$$ Ejercicio: define el título y subtítulo del gráfico, 
# los títulos de los ejes, el título de la leyenda, la etiqueta y el pie de figura en el siguiente gráfico.
# Consejo: usa solo una leyenda. Usa el argumento guide=NULL en el lugar correcto para hacerlo.
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb, size=weightLb)) +
  geom_point(shape=24)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb, size=weightLb)) +
  geom_point(shape=24) +
  scale_size_continuous(guide= NULL) 


########################################################################################################################################## #
#### 20 ### ANOTACIONES: annotate(), geom_text(), geom_*line(), geom_errorbar(), geom_crossbar(), geom_linerange(), geom_pointrange() ####
########################################################################################################################################## #
# Insertar texto, formas, etc. en el gráfico 

############# #
#### 20.1. ## Anotaciones de texto ####
################################### #

## Anotaciones individuales: annotate("text")
########################## #
p <- ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point()

p + annotate(geom="text", x=3, y=48, label="Grupo 1",
             family="serif", fontface="italic", colour="darkred", size=8) +
  annotate(geom="text", x=4.5, y=66, label="Grupo 2",
           family="serif", fontface="bold", colour="green", size=5)

## Anotar todos los datos: geom_text_repel()
########################## #
# datos
countr <- countries %>%
  tibble() %>%
  subset(Year==2009 & healthexp>2000)
countr
ggplot(countr, aes(x=healthexp, y=infmortality)) + 
  geom_point() +
  geom_text_repel(aes(label=Name))

## Anotar cada faceta: geom_text()
####################### #
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() + facet_grid(. ~ drv)
p
# Crear un dataframe con anotaciones
f_labels <- tibble(drv = c("4", "f", "r"), etiquetas = c("4x4", "Delantera", "Trasera"))
f_labels

# Anotarlas con el mismo color
p + geom_text(data=f_labels, aes(label=etiquetas), x=4, y=40, color="red")

# Anotarlas con color dependiendo de la variable de la faceta
p + geom_text(data=f_labels, aes(label=etiquetas, color=etiquetas), x=4, y=40)

############# #
#### 20.2. ## Anotación matemática: annotate("text", parse=TRUE) ####
###################################### #
# parse=TRUE: las etiquetas se interpretarán como expresiones y 
# se mostrarán según lo descrito en ?plotmath
?plotmath
p <- ggplot(data.frame(x=c(-3,3)), aes(x=x)) + stat_function(fun = dnorm)
p
p + annotate("text", x=2, y=0.3, parse=TRUE,
             label="frac(1, sqrt(2 * pi)) * e ^ {-x^2 / 2}")

# $$$ *Ejercicio: Anotar otra expresión siguiendo la sintaxis en ?plotmath
# $$$ RESPUESTA:
p + annotate("text", x=2, y=0.3, parse=TRUE,
             label="frac(infinity, sqrt(2 * pi)) * e ^ {tilde(x)}")

############# #
#### 20.3. ## Líneas: ####
######################### #
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
p
## Líneas horizontales y verticales: geom_hline(), geom_vline()
################################# #
p + 
  geom_hline(yintercept=60) + 
  geom_vline(xintercept=15)

## Líneas diagonales: geom_abline()
################## #
p + geom_abline(intercept=37.4, slope=1.75)

# $$$ Ejercicio: Dibuja una línea 1:1 en el siguiente gráfico
ggplot(heightweight, aes(x=heightIn, y=heightIn, colour=sex)) + geom_point()
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, y=heightIn, colour=sex)) + geom_point() +
  geom_abline(intercept=0, slope=1)


## Líneas relacionadas con estadísticas de datos
#################################### #
hw_means <- heightweight %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(heightIn = mean(heightIn), .groups="drop")
hw_means

p + geom_hline(data=hw_means, 
               aes(yintercept=heightIn, colour=sex), 
               linetype="dashed", linewidth=1)

## Segmentos y flechas: annotate("segment")
####################### #
p <- ggplot(subset(climate, Source=="Berkeley"), aes(x=Year, y=Anomaly10y)) +
  geom_line()
p + annotate("segment", x=1950, xend=1980, y=-.25, yend=-.25, colour="red")

p + annotate("segment", x=mean(climate$Year), xend=max(climate$Year), y=-.25, yend=-.25, colour="red")

p + annotate("segment", x=1850, xend=1820, y=-0.8, yend=-0.95, 
             arrow=arrow(), colour="blue", size=2)

# $$$ Ejercicio: Resalta el pico en 1940 usando una flecha roja
# $$$ RESPUESTA:
p + annotate("segment", x=1900, xend=1940, y=-0.7, yend=-0, 
             arrow=arrow(), colour="red", size=2)

############# #
#### 20.4. ## Rectángulos: annotate("rect") ####
########################## #

p + annotate("rect", xmin=1950, xmax=1980, ymin=-1, ymax=1, 
             alpha=.1, fill="blue")

# $$$ Ejercicio: Resalta el siglo XIX en verde
# $$$ RESPUESTA:
p + annotate("rect", xmin=1800, xmax=1900, ymin=-1, ymax=1, 
             alpha=.1, fill="green")

############# #
#### 20.5. ## Barras de error: geom_errorbar() ####
########################## #
ce <- subset(cabbage_exp, Cultivar == "c39")
ce

## En un gráfico de barras
################ #
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_col() +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=0.3) #se= error estándar, no hace falta calcularlo en nuestros datos

## En un gráfico de líneas
################### #
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_line(aes(group=1)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=0.2)

## Solas
######### #
# $$$ Ejercicio: Prueba las siguientes funciones geom_() por separado 
# (en el siguiente gráfico, sin otros geoms)
# geom_crossbar(), geom_linerange(), geom_pointrange()
ggplot(ce, aes(x=Date, y=Weight))
# $$$ RESPUESTA:
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_crossbar(aes(ymin=Weight-se, ymax=Weight+se))
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_linerange(aes(ymin=Weight-se, ymax=Weight+se))
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_pointrange(aes(ymin=Weight-se, ymax=Weight+se))


########################################## #
#### 21 ### TEMAS: theme(), theme_*() ####
########################################## #
# Apariencia del gráfico no relacionada con los datos

############# #
#### 21.1. ## Temas completos (predefinidos) ####
################################################# #
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p

############# #
## 21.1.1. ## Básico 
###################### #
# https://ggplot2.tidyverse.org/reference/ggtheme.html
# Gris (predeterminado), se ponen al final
p + theme_grey()

# Blanco y negro
p + theme_bw()

# Claro
p + theme_light()

# Oscuro
p + theme_dark()

# Mínimo
p + theme_minimal()

# Clásico
p + theme_classic()

# Vacío
p + theme_void()

# Cuadrículas
p + theme_linedraw()

# ...

############# #
## 21.1.2. ## Paquete ggthemes: Temas adicionales, escalas y geoms para 'ggplot2'
####################### #
# https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
# Ejemplos:
# Tema de Wall Street Journal: theme_wsj()  
# Tema sin nada más que un color de fondo: theme_solid(fill = "white")
# Tema basado en The Economist: theme_economist() 


############# #
#### 21.2. ## Modificar elementos en Theme ####
############################################### #
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + 
  geom_point()
z <- ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +   geom_histogram(binwidth=15) +
  coord_polar() + scale_x_continuous(limits=c(0,360))

# $$$ Ejercicio: en los siguientes ejemplos, presta atención al uso de 
## element_line(), element_text(), element_rect()...

## Área del gráfico
############## #
p + theme(
  panel.grid.major = element_line(colour="red"),
  panel.grid.minor = element_line(colour="red", linetype="dashed", size=0.2),
  panel.background = element_rect(fill="lightblue"),
  panel.border = element_rect(colour="blue", fill=NA, linewidth=2))

## Texto
######## #
p + 
  labs(title = "Aquí el título del gráfico",
       caption="Figura 1.") +
  theme(
    axis.title.x = element_text(colour="blue", size=16),
    axis.text.x = element_text(colour="purple"),
    axis.title.y = element_text(colour="green", size=14, angle = 90),
    axis.text.y = element_text(colour="orange"),
    plot.title = element_text(colour="red", size=20, face="bold", family="serif"),
    plot.caption.position = "plot",
    plot.caption = element_text(colour="pink", hjust =))

## Leyenda
########## #
z + theme(
  legend.background = element_rect(fill="grey90", colour="red", linewidth=1),
  legend.title = element_text(colour="blue", face="bold", size=14),
  legend.text = element_text(colour="red"),
  legend.key = element_rect(colour="blue", linewidth=0.25),
  legend.position="bottom") # right, left, top, bottom, none

## Facetas
########## #
f <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() + facet_grid(.~class) 
f
f + theme(strip.background = element_rect(fill="pink"),
          strip.text.x = element_text(size=8, angle=-10, face="bold", colour="red"))

## Eliminar elementos: element_blank() o NA
################### #
### Usando element_blank()
hwp
hwp + theme(axis.title.x=element_blank())
hwp
hwp + theme(panel.grid.minor = element_blank())
hwp
hwp + theme(panel.background = element_blank())

### Usando NA
hwp
hwp + theme(axis.title.x=element_text(colour=NA)) # comprueba que el resultado no es exactamente igual que usando element_blank()
hwp
hwp + theme(panel.grid.minor = element_line(colour=NA))
hwp
hwp + theme(panel.background = element_rect(fill=NA))


## Eliminar leyendas de escalas específicas: guide=FALSE
##################################### #
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb, size=weightLb)) +
  geom_point(shape=24)
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb, size=weightLb)) +
  geom_point(shape=24) +
  scale_size_continuous(guide=FALSE)

## Eliminar el título de la leyenda de escalas específicas: name=NULL
########################################## #
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb)) +
  geom_point(shape=24)
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb)) +
  geom_point(shape=24) +
  scale_fill_continuous(name=NULL) 

############# #
#### 21.3. ## Ideas para facilitarse la vida usando Theme ####
############################################################## #

## Interfaz gráfica para editar elementos del tema en ggplot2
########################################################## #
library(ggThemeAssist)
# Procedimiento:
# 1. Selecciona las líneas de tu gráfico
# 2. Ve a "Addins" (en la barra superior) >> ggplot Theme Assistant
# 3. Define tus preferencias en la interfaz
# 4. Haz clic en Listo y observa tu nuevo código en el script

# Consideración: Puede ser útil para probar y aprender, pero después es más práctico funcionar sólo con código

# $$$ Ejercicio: modifica el tema del siguiente gráfico usando Theme Assistant
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb, fill=weightLb)) +
  geom_point(size=3, shape=24) +
  scale_colour_continuous(name=NULL) +  
  scale_fill_continuous(guide=FALSE) 
# $$$ RESPUESTA: 


## Guardar temas en objetos para usarlos en diferentes gráficos
########################################################## #
# Por ejemplo, para darle el mismo formato a todos los gráficos de un trabajo
mi_tema <- theme(plot.subtitle = element_text(vjust = 1), 
                 plot.caption = element_text(vjust = 1), 
                 panel.background = element_rect(linetype = "dashed"), 
                 plot.background = element_rect(fill = "antiquewhite"))

p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb, fill=weightLb)) +
  geom_point(size=3, shape=24) +
  scale_colour_continuous(name=NULL) +  
  scale_fill_continuous(guide=FALSE)
p
p+mi_tema

## Definir tu propio tema predeterminado: theme_set()
################################# #
# para que se use por defecto a menos que especifiques otro
theme_set(theme_bw())
p
# volver al original:
theme_set(theme_grey())


#*******************************************************
############# #
#### BLOQUE 4: GUARDAR FIGURAS Y MÁS                ####
############# #
#*******************************************************

############################## #
#### 22 ### GUARDAR FIGURAS ####
############################## #
# Observa que las guardamos en la carpeta "figuras" de nuestro proyecto usando "figuras/nombre.extension"

############# #
#### 22.1. ## ggsave() ####
########################### #
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
mi_figura_guardada <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()

# Opción 1: Guarda la última figura ejecutada en ggplot
ggsave("figuras/mi_figura.pdf", width=8, height=8, unit="cm", dpi=300) #se suele poner debajo del gráfico
ggsave("figuras/mi_figura.tiff", width=8, height=8, unit="cm", dpi=300)
ggsave("figuras/mi_figura.jpg", width=8, height=8, unit="cm", dpi=300)
ggsave("figuras/mi_figura.png", width=8, height=8, unit="cm", dpi=300)

# Opción 2: Guarda una figura que está guardada en un objeto
ggsave("figuras/mi_figura_guardada.tiff", mi_figura_guardada, width=8, height=8, unit="cm", dpi=300)

# Observación 1: Se recomienda dpi>=300
# Observación 2: se pueden guardar en múltiples formatos

############# #
#### 22.2. ## Combinar figuras: {patchwork} #### 
################################################ #
# Figuras individuales para combinar
fig1 <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
fig2 <- ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +   geom_histogram(binwidth=15) + coord_polar() + scale_x_continuous(limits=c(0,360))
fig3 <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
fig4 <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb, fill=weightLb)) + geom_point(size=3, shape=24)
fig5 <- ggplot(mpg, aes(x=displ, y=hwy, color=factor(cyl))) + geom_point()

# Ejemplo:
figura_combinada <- (fig1 | fig2) / fig3
figura_combinada
ggsave("figuras/figura_combinada.png", figura_combinada, width = 10, height = 8)

# |              gráficos lado a lado
# /              gráficos arriba/abajo
# ()             agrupa gráficos 
# plot_layout()  controla el número de columnas o filas independientemente de los operadores

# $$$ Ejercicio: Juega a combinar las figuras fig 1-5 de diferentes formas
# $$$ RESPUESTA:



################################## #
#### 23 ### EXTENSIONES DE GGPLOT ####
################################## #
# https://exts.ggplot2.tidyverse.org/gallery/
# Ejemplos: ggreppel, ggthemes, gganimate

################################################################# #
##'
##'  "Seminario - Hackathon datos espacialmente explícitos en R"
##'       
##'       Feb 2026
##'       by Ignacio Morales-Castilla
##'
################################################################# #


## limpiando el ambiente de trabajo
rm(list=ls())
options(stringsAsFactors = FALSE)


## establecer directorio de trabajo (wd)
getwd()  
setwd("mi_casitadecampo")


## cargar paquetes
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
theme_set(theme_bw())



## 1. Carga datos espaciales ##
############################ ##

## datos raster
### 1. carga datos de temperaturas medias de Europa desde 1ene2011 a 31dec2024
###https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php----

tmedia <- rast("tg_ens_mean_0.25deg_reg_2011-2024_v30.0e.nc")
tmedia
plot(tmedia[[141]])

### 2. carga datos de temperaturas medias de Europa desde 1ene2011 a 31dec2024
###https://surfobs.climate.copernicus.eu/dataaccess/access_eobs_chunks.php----

tmedia2011 <- subset(tmedia, 1:365)

plot(tmedia2011[[141]])


fechas <- time(tmedia2011)  #sacamos las fechas que incluye el objeto
class(fechas) 

#fechas <- as.Date(fechas) #convertir texto en fecha

format(fechas, "%Y") #sacar distintos elementos del vector 
format(fechas, "%m")
format(fechas, "%d")
format(fechas, "%j") #sacar el día juliano
format(fechas, "%d/%m/%Y") #orden y caracteres que los separen



diasmayo <- which(format(fechas,"%m")=="05") #sacar los días de un mes
plot(tmedia2011[[122]])

#sacar el mismo mes de todos nuestros años
fechasall <- time(tmedia)
diasmayos <- which(format(fechasall, "%m")=="05")


tmediamayo <- subset(tmedia2011, diasmayos)



# obtener el shapefile
CCAA_sf <- esp_get_ccaa()



# cortar raster en base al mapa de españa
tmediamayospain <- crop(tmediamayo, CCAA_sf)
plot(tmediamayospain[[5]])
lines(CCAA_sf)



# extraer datos
ccaa_spatvector <- vect(CCAA_sf$geometry) #transformar mapa de multipolygon a spatvector

# mapas en el mismo sistema de coordenadas
crs(tmediamayospain)
crs(ccaa_spatvector)



tmediamayoccaa <- extract(tmediamayospain, #temperaturas media para cada día para cada píxel
                          ccaa_spatvector)





unique(tmediamayoccaa$ID)

mediasdiasmayoccaa <- aggregate(.~ID, data = tmediamayoccaa, #Tº media para cada comunidad autónoma
                                mean, na.rm=T)


mediasdiasmayoccaa$ccaa <- CCAA_sf$ine.ccaa.name[1:17]

mediasjulioccaa <- rowMeans(mediasdiasjulioccaa[,2:32])

codauto = c("01","02","03")

mediasjulioccaa <- data.frame(codauto = c(paste0("0",1:9),as.character(10:17)),
                              tempjulio = as.numeric(mediasjulioccaa))

mediasjulioccaa[18:19,1] <- c("18","19")
mediasjulioccaa[18:19,2] <- 0

str(mediasjulioccaa)
# unir datos, de nuevo utilizando el comando merge
CCAAtemps_sf <- merge(CCAA_sf, mediasjulioccaa)




# Dibujar el mapa con ggplot


ggplot(CCAAtemps_sf) +   
  geom_sf(aes(fill = tempjulio), 
          color = "grey70",
          lwd = .3
  ) +       
  scale_fill_gradientn(
    colors = hcl.colors(10, "Reds", rev = TRUE),  
    n.breaks = 10,
    guide = guide_legend(title = "temp.julio")  
  ) +
  theme_void() +  
  theme(legend.position = c(0.1, 0.6)) 



# repetir para Europa

world <- ne_countries(scale = "medium", returnclass = "sf")

plot(world$geometry)


## subset to Europe
europe <- subset(world, continent == "Europe")
europesv <- vect(europe)
plot(europesv)


europe <- crop(europesv, ext(tmedia))
plot(europe)

################################################################# #
##'
##'  "Mapas, SIG y operaciones cartográficas sencillas en R"
##'       Feb 2025
##'       by Ignacio Morales-Castilla
##'
################################################################# #

## houskeeping
rm(list=ls())
options(stringsAsFactors = FALSE)


## Setting working directory. 
#setwd("~/OneDrive - Universidad de Alcala/Work_UAH_BeaGal/teaching/2023/Gestion de datos/clases/") 
#setwd("../OneDrive - Universidad de Alcala/Work_UAH_BeaGal/teaching/2023/Gestion de datos/clases/") 

getwd()  
setwd("mi_casitadecampo")


## useful mapping packages (install in case you don't have them)
#install.packages(c("googleway", "libwgeom",
#                  "rnaturalearth", "rnaturalearthdata"))


## Loading packages
#install.packages()
#install.packages()
#install.packages()
#install.packages()

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
theme_set(theme_bw())



## 1. Loading spatial data ####
############################ ##

## raster data
### 1a. OBTAIN WORLDCLIM DATA (package geodata)----

clima <- worldclim_global(var="tmax", 
                          res=10, 
                          path=".")

clima
plot(clima$wc2.1_10m_tmax_08)
dev.off()



## explore a zoomed region
plot(clima$wc2.1_10m_tmax_01, 
     xlim=c(-10,5), ylim=c(35,44))


## get a higher resolution map
climaspain <- worldclim_country("Spain", "tmax",
                                res=2.5,
                                path=getwd())

dev.off()
par(mfrow=c(1,2))
plot(clima$wc2.1_10m_tmax_01, 
     xlim=c(-10,5), ylim=c(35,44),
     main="mapa feo")

plot(climaspain$ESP_wc2.1_30s_tmax_1,
     xlim=c(-10,5), ylim=c(35,44),
     main="mapa lindo")



mapamundo <- world(resolution = 2,
                   path=getwd())

dev.off()
plot(mapamundo)

mapmadagascar <- mapamundo[mapamundo$NAME_0=="Spain",]
plot(mapmadagascar)
mapgalapagos <- crop(mapmadagascar,ext(-100,-86,-2,1))
plot(mapgalapagos)
tempgalapagos <- crop(climaspain,mapgalapagos)
plot(tempgalapagos$wc2.1_2.5m_tmax_01)
lines(mapgalapagos,lwd=2)


mapspain <- mapamundo["Spain",]
mapspain <- mapamundo[mapamundo$NAME_0=="Spain",]
mapspain <- mapamundo[69,]
which(mapamundo$NAME_0 == "Spain")
which(mapamundo$NAME_0 %in% c("Spain","Portugal","France"))

plot(mapspain)

## recortar el mapa para la españa peninsular
mappeninsula <- crop(mapspain, ext(-10,5,35,44))
plot(mappeninsula)
str(mappeninsula)

## cut map#wc2.1_10m_tmax_01# cut map
climaspaincrop <- crop(climaspain, ext(-10,5,35,44))
plot(climaspaincrop$ESP_wc2.1_30s_tmax_1)
lines(mappeninsula, col="black", lwd=1.5)


climaspainmask <- mask(climaspaincrop, mappeninsula)
plot(climaspainmask$ESP_wc2.1_30s_tmax_1)
lines(mappeninsula, col="black", lwd=2)




## polygon data
### 1b. Read in country shapefile data ----
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
plot(world$geometry)


## subset to Europe (porque hay una columna de continentes)
europe <- subset(world, continent == "Europe")
plot(europe$geometry)

## subset to Iberia
iberia <- subset(europe, sovereignt %in% c("Spain","Portugal"))
plot(iberia$geometry)
lines(iberia, col="black", lwd = 2.5)

dev.off()


### 1c. Read in parques nacionales shapefile data ----
## https://www.mapama.gob.es/app/descargas/descargafichero.aspx?f=enp.zip
enps <- st_read("Enp/Enp2025.shp")
enps$geometry


#lines(, col="darkred",lwd=0.5) Esto no funciona
plot(enps$geometry, col="darkred",lwd=0.5) #Esta sí


ppnn <- st_read("data/Limites_PyB.shp")
ppnn
plot(ppnn$geometry)
lines(ppnn$geometry, col="darkred", add=T)

table(enps$figura_lp)
enps$sitename

plot(climaspaincrop$ESP_wc2.1_30s_tmax_1)
plot(subset(enps, figura_lp=="Parque Nacional"),col="blue", add=T)


## cuantos tipos de parques
unique(enps$figura_lp)



## seleccionar solo parques nacionales y naturales

ppnnnat <- subset(enps, figura_lp %in% c("Sitio de Interés Científico",
                                         "Microrreserva"))
lines(ppnnnat, col=c("black","red"),
      lwd=1.5) #Lo añade por defecto sobre lo que hay



### 2. get coordinates for a species distribution ----
## 
# install.packages("rgbif")
library(rgbif)
occ_search(scientificName = "Pelophylax perezi")

ranita <- occ_data(scientificName = "Pelophylax perezi",
                   #country = "Spain", #No funciona xd
                   limit = 5000)
class(ranita)

table(ranita$data$country) #Incluye datos de zoos

ranita <-ranita$data
ranitaspain <- subset(ranita, country=="Spain")

points(x = ranitaspain$decimalLongitude,
       y = ranitaspain$decimalLatitude,
       col="darkred",pch=19, cex=0.5)

points(x = ranitaspain$decimalLongitude,
       y = ranitaspain$decimalLatitude,
       col="salmon",pch=19,cex=0.5)



## extraer datos de clima
ranitacoords <- data.frame(x = ranitaspain$decimalLongitude,
                           y = ranitaspain$decimalLatitude)

ranitatemps <- extract(climaspainmask, ranitacoords) #juntamos las coordenadas con los 12 mapas

head(ranitatemps)



## extract data according to coordinates

plot(climaspain$ESP_wc2.1_30s_tmax_1)

coordenadas <- data.frame(x=lince$data$decimalLongitude,
                          y=lince$data$decimalLatitude)

climacoords <- extract(climaspain, coordenadas)




## extraer datos according to polygons

tempmaxeneroppnn <- extract(climaspainmask$ESP_wc2.1_30s_tmax_1, 
                            parquecitos, 
                            fun=mean, na.rm=T) #le pedimos la media de los polígonos y le pedimos que borra los NAs (na.rm=T)


datostempsppnn <- cbind(tempmaxeneroppnn,
                        parquecitos$NOM_PARQUE)


climaspainmask
bio1enp$nombre <- ppnnnat$sitename
bio1enp[which.min(bio1enp$wc2.1_30s_bio_1),3]


bio1ppnn <- extract(climaspain$wc2.1_30s_bio_1, ppnn, 
                    fun=mean, na.rm=T)



## hay que cambiar la proyecci?n
ppnncor = st_transform(ppnn, crs(ppnnnat))
lines(ppnncor, col="red")

bio1ppnn <- extract(climaspain$wc2.1_30s_bio_1, ppnncor, 
                    fun=mean, na.rm=T)
bio1ppnn$idppnnat <- ppnncor$NOM_PARQUE


bio1ppnn <- extract(climaspain$wc2.1_30s_bio_1, ppnncor)
bio1ppnn

boxplot(wc2.1_30s_bio_1~ID, 
        ylab = "Temperatura (C)",
        xlab = "Parque Nacional",
        data=bio1ppnn)



dev.off()
unique(bio1ppnn$ID)




#'########
# Utilizando el paquete mapSpain https://github.com/rOpenSpain/mapSpain/blob/main/README.md ####
#'########
library(mapSpain)

# cargamos datos del censo de poblaci?n del INE, que viene disponible por 
#defecto en el paquete

census <- pobmun19
#census <- mapSpain::pobmun19


# extraemos los c?digos para cada comunidad aut?noma

codelist <- esp_codelist
#codelist <- mapSpain::esp_codelist
str(codelist)


# unimos las dos tablas utilizando el comando 'merge'
census <-
  unique(merge(census, codelist[, c("cpro", "codauto")], all.x = TRUE))



# agregamos los valores por comunidad aut?noma
census_ccaa <-
  aggregate(cbind(pob19, men, women) ~ codauto, data = census, sum)

# calculamos porcentajes

# mujeres
census_ccaa$porc_women <- census_ccaa$women / census_ccaa$pob19
census_ccaa$porc_women_lab <-
  paste0(round(100 * census_ccaa$porc_women, 2), "%")

# hombres
census_ccaa$porc_men <- census_ccaa$men / census_ccaa$pob19
census_ccaa$porc_men_lab <-
  paste0(round(100 * census_ccaa$porc_men, 2), "%")

census_ccaa



# unir los datos anteriores a un shapefile de espa?a (disponible en el paquete mapSpain)
# con informaci?n de los l?mites de las comunidades aut?nomas, para poder mapear

# obtener el shapefile
CCAA_sf <- esp_get_ccaa()
plot(CCAA_sf$geometry)

# unir datos, de nuevo utilizando el comando merge
CCAA_sf <- merge(CCAA_sf, census_ccaa)

# obtener la cajita para Canarias
Can <- esp_get_can_box()
plot(Can, add=T)



# Dibujar el mapa con ggplot


ggplot(CCAA_sf) +   
  geom_sf(aes(fill = porc_women), ## este m?dulo elige qu? variable se dibuja
          color = "grey70",
          lwd = .3
  ) +       
  geom_sf(data = Can, color = "grey70") +   ## aqu? a?adimos la l?nea de Canarias
  geom_sf_label(aes(label = porc_women_lab),  ## aqu? a?adimos etiquetas
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "spectral", rev = TRUE),  ## cambiamos la escala de colores
    n.breaks = 10, #breaks de la leyenda
    labels = function(x) {
      sprintf("%1.1f%%", 100 * x)
    },
    guide = guide_legend(title = "Porc. women")  ## a?adimos leyenda
  ) +
  theme_void() +  ## eliminamos el fondo gris feote
  theme(legend.position = c(0.1, 0.6))  ## especificamos d?nde queremos la leyenda



## altitudes y batimetr?a

hypsobath <- esp_get_hypsobath()
plot(hypsobath$geometry)

# hay que corregir un error en los datos de origen
# Remove:
hypsobath <- hypsobath[!sf::st_is_empty(hypsobath), ]
sum(sf::st_is_empty(hypsobath))


# Colores a partir de Wikipedia
# https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps
bath_tints <- colorRampPalette( #Colores típicos batimetría
  rev( #ñe damos la vuelta para que los colores oscuros sean el fondo marino
    c(
      "#D8F2FE", "#C6ECFF", "#B9E3FF",
      "#ACDBFB", "#A1D2F7", "#96C9F0",
      "#8DC1EA", "#84B9E3", "#79B2DE",
      "#71ABD8"
    )
  )
)

hyps_tints <- colorRampPalette(
  rev(
    c(
      "#F5F4F2", "#E0DED8", "#CAC3B8", "#BAAE9A",
      "#AC9A7C", "#AA8753", "#B9985A", "#C3A76B",
      "#CAB982", "#D3CA9D", "#DED6A3", "#E8E1B6",
      "#EFEBC0", "#E1E4B5", "#D1D7AB", "#BDCC96",
      "#A8C68F", "#94BF8B", "#ACD0A5"
    )
  )
)

## ordenamos los niveles de alturas
levels <- sort(unique(hypsobath$val_inf))


# generamos una paleta única de colores
br_bath <- length(levels[levels < 0])
br_terrain <- length(levels) - br_bath #Para que no incluya el 0 dos veces
pal <- c(bath_tints((br_bath)), hyps_tints((br_terrain))) #Nuestra paleta



# hacemos el dibujo para las islas Canarias
ggplot(hypsobath) +
  geom_sf(aes(fill = as.factor(val_inf)),
          color = NA
  ) +
  coord_sf(
    xlim = c(-10, +5), #Cogemos las coordenadas de Canarias
    ylim = c(35, 44)
  ) +
  scale_fill_manual(values = pal) + #Epleamos nuestra escala manual llamada pal y le decimos donde queremos los elementos
  guides(fill = guide_legend(
    title = "Elevation",
    direction = "horizontal",
    label.position = "bottom",
    title.position = "top",
    nrow = 1
  )) +
  theme(legend.position = "bottom")



# Y ahora dibujamos el mapa de altura/batimetr?a para el continente
spainhypbat <- ggplot(hypsobath) +
  geom_sf(aes(fill = as.factor(val_inf)),
          color = NA
  ) +
  coord_sf(
    xlim = c(-9.5, 4.4),
    ylim = c(35.8, 44)
  ) +
  scale_fill_manual(values = pal) +
  guides(fill = guide_legend(
    title = "Elevation",
    reverse = TRUE,
    keyheight = .8
  )) 

spainhypbat


######## package climateR ---- 
#install.packages("climateR")
#install.packages("tigris")
#remotes::install_github("mikejohnson51/AOI")
#library(AOI)
#library(tigris)

#states <- states(cb = TRUE)

### END ----



