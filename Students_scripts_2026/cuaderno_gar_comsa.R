#Día 1 ####
#Generar vectores
vector_num <- c(1,2,3,4)
vector_int <- c(1:4)
#Separar datos y scripts en un mismo proyecto
#Obtener dirección del directorio
getwd()
#Instalar y abrir las librerias
#install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
#Abrir base de datos
#diamonds <- read.csv("1_data/diamonds.csv")
data("diamonds")
#guardar descargados los datos
write.csv(diamonds,"1_data/diamonds.csv")
#Generar y guardar resultados gráficos
ggplot(diamonds, aes(carat,price)) +
  geom_hex()
ggsave("3_resultados/diamonds.pdf")

#Día 2 Rbase ####
#Obtener ayuda de función, palabras y paquetes
?read.table


#R como calculadora (objetos)
objeto_1 <- c(9*10, 10/3,10+3)
#Guardar
write.table(objeto_1,
            file = "1_data/primer_objeto.txt",
            sep= "\t",
            dec = ",")
#Crear funciones
sumar.tres <- function(a, b, c){
  suma= a + b + c
  return(suma)
}
sumar.tres(a=3, b=2, c=1)

sumar.vectores <- function(vec){
  if(class(vec) == "numeric"){
    suma= sum(vec)
    return(suma)
  }else{
    print("error, no es num")
  }
}
sumar.vectores(2:6)

#Crear un dataframe. Varios vectores con mismo tamaño
#Crear variable "id" que sea el identificador. Numero discreto de 1 a 1000
#Variable altura. Secuencia de 0.5m a 4m con 100 posiciones repetida 10 veces
#Variable peso. Distribución normal de media 40kg y desviación estandar de 10

id <- 1:1000
altura <- rep(seq(0.5, 4, by= (4-0.5)/99 ), 
            times=10)
#También 
#seq(0.5, 4, length.out=100)
peso <- rnorm(length(id), 40, 10)
datos <- data.frame(id, altura, peso)
#Estadísticas básicas
summary(peso)

plot(datos$altura, datos$peso)
#Valores cuantitativos:
#Discretos
#Continuos

#Valores cualitativos:
#Factores. Con niveles
#Bol. 1/0 o T/F
#Textos. "chr"

#Siete especies
set.seed(123) #Resultado reproducible
#Vector aleatorio
especies <- factor(sample(
  c("rana", "serpiente", "triceratops", "cuervo", "jirafa", "caracol", "perro"),
  1000, replace = T))

df_salvaje <- data.frame(datos, especies)
##Lectura como objeto de distinta clase: as.""

#Día 3 ####

#Diferencia entre vector y df:

#Exploración
#Estructura, nombre y dimensiones
#Medias, rangos y valores clave (NAs, 0)
#Variables a rem¡nombrar, seleccionar o filtrar

glimpse(df_salvaje)
summary(df_salvaje)

#Filtrar serpientes y media del peso. Ordenar por altura y filtar mayor a 50

#Selección dentro de un vector
#Posición vector[4] (es) vector[c(4, 5, 20)] (sin) [-4]
#Condición vector[vector > 50]
#In

serpientes <- df_salvaje [df_salvaje$especies == "serpiente" & df_salvaje$peso > 50, ]
summary(serpientes$peso)
sort(serpientes$peso)

#También
serpientes <- filter(df_salvaje, 
                     especies == "serpiente" & 
                       peso > 50)



#También
serpientes <- df_salvaje %>%
  filter(especies== "serpiente" & 
           peso > 50)
  
?select
#Grupos: 
#Vector. Una dimensión, datos del mismo tipo
#Matriz. Dos dimensiones, datos del mismo tipo
#Lista. Una dimensión, datos de distinto tipo
#Data frame. Dos dimensiones,  datos de distinto tipo

#Sitio donde hemos muestreado los 1000 individuos de 7 ssp
#sitio <- sample(1:15, 1000, replace = T)
#df_salvaje <- cbind(df_salvaje, sitio)

#También
df_salvaje$sitios <- sample(1:15, 1000, replace = T)

#Una columna por especie con pivot (pivot_wider)
df_sitio <- pivot_wider(df_salvaje, names_from = especies, values_from = peso )
df_sitio[is.na(df_sitio)] <- 0

#Nuevas variables funciones
df_sitio %>%
  mutate(peso100= peso*100)

#Agrupar
df_agrupado <- df_sitio |>
  group_by(sitios)|>
  summarise(caracol= sum(caracol),
            perro= sum(perro),
            jirafa= sum(jirafa),
            rana= sum(rana),
            serpiente= sum(serpiente),
            triceratops= sum(triceratops),
            cuervo= sum(cuervo))
#También en formato tidy
df_agrupado <- df_salvaje |>
  group_by(sitios, especies)|>
  summarise(peso= sum(peso))


#Heatmap del peso en cada sitio
ggplot(df_agrupado, aes( x= especies, y= sitios, fill = peso))+
  geom_tile(color= "white")+
  scale_fill_viridis_c(name = "peso")+
  theme_minimal()

#Día 4 ####
#Trabajamos con mtcars
data(mtcars)
names(mtcars)
nrow(mtcars)
str(mtcars)
glimpse(mtcars)
summary(mtcars)
#Añadir nombre del coche como variable
mtcars$car_name <- row.names(mtcars)

#Elegir mejor marca en base a mpg*cyl
#Seleccionar variables de interes: Nombres, cilindros y gasto (mpg)
#Filtrar número de cilindros >4
cars <- mtcars |>
  select(car_name, cyl, mpg)|>
  filter(cyl > 4)|>
  mutate(mpg_cyl= mpg * cyl)|>
  sort_by(cars$mpg_cyl)|> #Para marca, extraemos la primera palabra del nombre
  mutate(car_brand= word(car_name, c(1))
#Tomamos la media de la variable de cada marca
by_brand <- cars |>
  group_by(car_brand)|>
  summarise(mpg_cyl_mean= mean(mpg_cyl))|>
  sort_by(by_brand$mpg_cyl_mean)|> #También con arrange(desc(mpg_cyl_mean))
  slice(1:3)|>#Tres primeras filas
  mutate(ranking= 1:3)

#podium <- data.frame(pos= c("oro", "plata", "bronce"), head(by_brand$car_brand, 3))

#Unión de df por identificador común
#Debe de ser del mismo tipo

#Medallas por separado
medallas <- data.frame(medalla= c("oro", "plata", "bronce", "losser"), ranking= 1:4)

podium <- full_join(by_brand, medallas, by= "ranking")

#Día 5 ####

#Caso hipotético: Informe de correlación del calentamiento global con el aumento del CO2 en Sevilla.
#Script aparte

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
# install.packages("tidyverse")
# install.packages("gcookbook")
# install.packages("ggrepel")
# install.packages("hexbin")
# install.packages("patchwork")
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

# Gráfico de barras agrupado por As 
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
ggplot(dat, aes(x=xval, y=yval))

# Observación:
# ¡no hay puntos, barras ni líneas!
# ... porque para eso es necesario definir cómo visualizar los datos (geom_())

# $$$ Ejercicio: Observa los ejes. ¿Están relacionados con xval y yval?
# $$$ RESPUESTA: 
#Si, están en sus intervalos

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
ggplot(dat, aes(x=xval, y=yval)) + geom_line()

# $$$ Ejercicio: crea un gráfico con líneas y puntos:
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + geom_point() + geom_line()

########### #
#### 2.4 ## aes() ####
###################### #
# Ahora identifiquemos los puntos de los grupos A y B con diferentes colores:
ggplot(dat, aes(x=xval, y=yval, colour=group)) + geom_point()
# Mismo resultado:
ggplot(dat, aes(x=xval, y=yval)) + geom_point(aes(colour=group))

# $$$ Ejercicio: dibuja una línea diferente para cada grupo (A y B)
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval, colour = group)) + 
  geom_line()

# $$$ Ejercicio: incluye puntos en el gráfico anterior sin diferenciar
# el color por grupo (es decir, líneas coloreadas por grupo, puntos negros)
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + 
  geom_point() + 
  geom_line(aes(color=group))

# $$$ Ejercicio: ahora colorea tanto las líneas como los puntos por grupo
# (pista: hay dos formas de hacerlo)
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval, color=group)) + 
  geom_point() + 
  geom_line()


############ #
#### 2.5 ## aes: ¿qué argumentos son válidos para cada función? ####
##################################################################### #
# $$$ Ejercicio: ¿Qué argumentos son válidos para geom_point()? 
# (Pista: revisa la sección "Aesthetics" en la ayuda de la función usando: ?geom_point)
# $$$ RESPUESTA:
?geom_point
# 
# •	x	
# •	y	
# •	alpha
# •	colour	
# •	fill	
# •	group	
# •	shape	
# •	size	
# •	stroke

# $$$ Ejercicio: Elige alguna de estas opciones para identificar 
# la variable "group" en el siguiente gráfico.
# Prueba al menos alpha, shape, size y presta atención a los mensajes de advertencia.
dat
ggplot(dat, aes(x=xval, y=yval)) + geom_point()
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + geom_point(aes(shape = group, colour = group))

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
# $$$ RESPUESTA: 
#Final de línea

# $$$ Ejercicio: ¿qué pasa si no incluimos el "+"?
ggplot(dat, aes(x=xval, y=yval)) 
geom_point()
# $$$ RESPUESTA: 
#No se ejecutan a la vez y no lo hace bien la segunda función  

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
# ggplot(___________, aes(x=_______, y=_________)) + geom_point()
# $$$ RESPUESTA:
ggplot(data = heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point()

############ #
#### 3.2. ## Gráfico de dispersión agrupado #### 
################################################ #
# Una variable categórica definirá la estética de los puntos

# $$$ Ejercicio: diferencia los puntos de hombres y mujeres:
# a) usando diferentes colores (pista: colour)
# $$$ RESPUESTA:
ggplot(data = heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(aes(colour = sex))

# b)* usando diferentes formas de puntos (pista: shape)
# $$$ RESPUESTA:
ggplot(data = heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(aes(shape = sex))

# c)* usando tanto diferentes colores como formas
# $$$ RESPUESTA:
ggplot(data = heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(aes(shape = sex, colour = sex))

# d)* usando diferentes tamaños de puntos (pista: size) (¡presta atención a la advertencia!) 
# $$$ RESPUESTA:
ggplot(data = heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(aes(size = sex))

############ #
#### 3.3. ## Gráfico de dispersión con una tercera variable continua #### 
######################################################################### #
# Una variable continua definirá la estética de los puntos

# $$$ Ejercicio: Incluye la variable "weightLb" en el siguiente gráfico:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  geom_point()
# a) usando colores
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour = weightLb)) +  geom_point()

# b)* usando tamaños de puntos (pista: usa alpha=0.2 en geom_point para facilitar la visualización con transparencia)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  geom_point(alpha= 0.2, aes(size = weightLb))

# $$$ Ejercicio: Ahora representa "weightLb" con diferentes tamaños de puntos y 
# "sex" con diferentes colores
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  
  geom_point(alpha= 0.2, aes(size = weightLb, colour = sex))


# OBSERVACIÓN: la precisión en la percepción es mayor para las coordenadas (x,y) que para 
# los colores o tamaños. ¡Elige las variables para cada estética teniendo en cuenta esto!

# $$$ Ejercicio*: representa "weightLb" con diferentes tamaños de puntos y 
# "sex" con diferentes formas
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  
  geom_point(alpha= 0.2, aes(size = weightLb, shape = sex))

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
  geom_point(alpha=0.6, size=5)

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
  geom_point(shape=20, size=5)

## *Superposición alta
####################### #
sa <- ggplot(diamonds, aes(x=carat, y=price)) 
sa + geom_point()

# Solución 1 - transparencia de los puntos: alpha
# $$$ *Ejercicio: implementa esta solución usando transparencia al 90% y 99% 
# No te preocupes si el gráfico tarda en dibujarse: es porque son muchos puntos.
# $$$ RESPUESTA:
sa + geom_point(alpha= 0.1)

# Solución 2 - graficar la "densidad de puntos" en un raster
# $$$ *Ejercicio: grafica la densidad de puntos en el gráfico price-carat
# pista: geom_bin2d(). Es un geom_(), por lo que no se necesita geom_point()
# Por defecto, geom_bin2d() divide el espacio en 30x30=900 celdas
# $$$ RESPUESTA:
sa + geom_bin2d()

# $$$ *Ejercicio: elige el número de cuadrantes en los que se divide cada eje
# usando el argumento "bins"
# $$$ RESPUESTA:
sa + geom_bin2d(bins= 70)

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
# $$$ RESPUESTA: 
#Distribución aleatoria
# $$$ *Ejercicio: modifica los valores de width en geom_jitter()
sc + geom_jitter(width=0.2)
# $$$ RESPUESTA: 
sc + geom_jitter(width=0.3)

# $$$ *Ejercicio: en cuál de los siguientes gráficos puede ser recomendable 
# usar width>0 y height>0?
ggplot(ChickWeight, aes(x=Time, y=weight)) + 
  geom_jitter(width=0, height=0)
ggplot(ChickWeight, aes(x=weight, y=Time)) + 
  geom_jitter(width=0, height=0)
# $$$ RESPUESTA:
ggplot(ChickWeight, aes(x=Time, y=weight)) + 
  geom_jitter(width=0.2, height=0)
ggplot(ChickWeight, aes(x=weight, y=Time)) + 
  geom_jitter(width=0, height=0.2)

# Solución 2: no hagas un diagrama de dispersión ;)
# La variación de los puntos se puede sintetizar de otras maneras
# por ejemplo, un diagrama de cajas agrupado por una variable categórica
# $$$ *Ejercicio: usa "geom_boxplot()" para representar el peso en diferentes momentos
# Consejo: usa factor(Time) o group=Time para especificar que deseas agrupar los datos en categorías de Tiempo
# $$$ RESPUESTA:
ggplot(ChickWeight, aes(x=Time, y=weight, group = Time)) + 
  geom_boxplot()

ggplot(ChickWeight, aes(x=factor(Time), y=weight)) + 
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
ggplot(heightweight, aes(x=ageYear, y=heightIn)) +  geom_point()+
  geom_rug()

############ #
#### 3.6. ## Diagrama de dispersión con línea de regresión ajustada: geom_smooth() #### 
####################################################################################### #
heightweight
sr <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sr + geom_point()

# $$$ Ejercicio: representar los puntos y la línea ajustada
# $$$ RESPUESTA: 
sr + geom_point() + geom_smooth()

# Observación: por defecto usa el método "loess", que combina
# polinomios ajustados localmente
# Observación 2: por defecto define un intervalo de confianza del 95%

# $$$ Ejercicio*: busca en la ayuda de R cuáles son los argumentos de geom_smooth()
# para modificar el método de ajuste y el nivel de confianza.
# Crea un nuevo gráfico ajustando un modelo lineal (lm) con un intervalo de confianza del 99%.
# $$$ RESPUESTA: 
?geom_smooth
sr + geom_point() + geom_smooth(method = "lm", level = 0.99)

# $$$ Ejercicio*: encuentra cómo ocultar el intervalo de confianza
# $$$ RESPUESTA: 
sr + geom_point() + geom_smooth(method = "lm", level = 0)

# $$$ Ejercicio*: colorea la línea en rojo y el intervalo de confianza en rosa
# $$$ RESPUESTA: 
sr + geom_point() + geom_smooth(method = "lm", level = 0.99, colour= "red", fill= "pink")

# $$$ Ejercicio: agrupa los puntos por sexo y ajusta una línea para cada sexo
# $$$ RESPUESTA: dos opciones:

sr + geom_point(aes(colour= sex)) + geom_smooth(method = "lm", level = 0.99, aes(colour= sex))


# $$$ Ejercicio**: imagina cómo representarías una línea ajustada con un modelo diferente a los 
# ofrecidos en las opciones de geom_smooth() (es decir, cómo lo harías manualmente)
# $$$ RESPUESTA: 


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
  geom_point(shape=25, size=2, colour= "black", fill="white")


# Consejo 2: puedes definir formas de puntos con cualquier símbolo del teclado entre ""
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape="?", size=2, colour= "black", fill="white")


# Consejo 3: puedes definir formas de puntos con atajos en un teclado numérico extendido, usando "" 
# Alt+número. Ejemplo: Alt+3 da como resultado un corazón. 
# Ver una lista de posibilidades en: https://typefacts.com/en/articles/keyboard-shortcuts
# ¡Y a jugar! ;)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape="♥", size=5, colour= "red")+
  geom_smooth(method = "lm")


# Consejo 4: entre las formas habituales, el relleno solo funciona para las formas 21-25
# Es decir, 21–25 tienen fill y colour
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + 
  geom_point(shape=22, size=3, colour= "red", fill="pink")+
  geom_smooth(method = "lm")

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
ggplot(tg, aes(x=dose, y=length, group = supp)) + geom_line()

# $$$ Ejercicio*: Ahora diferéncialas por tipo de línea
# (¡publicar en colores es caro!)
# Pista: linetype
# $$$ RESPUESTA:
ggplot(tg, aes(x=dose, y=length)) + geom_line(aes(linetype = supp))

?geom_line
# $$$ Ejercicio*: diferencia los dos grupos por la forma de los puntos
# Consejo: puedes agrandar los puntos para facilitar la discriminación de formas
# $$$ RESPUESTA:

ggplot(tg, aes(x=dose, y=length)) + geom_point(aes(shape = supp, size = supp))

# $$$ Ejercicio*: diferencia los dos grupos por color de los puntos
# Consejo: fill para formas 21-25, colour para otras
# Consejo 2: puedes agrandar los puntos para facilitar la discriminación de colores
# $$$ RESPUESTA:

ggplot(tg, aes(x=dose, y=length)) + geom_point(size = 6, aes( colour = supp))

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
ggplot(uspopage2, aes(x= Year, y= Thousands, colour = AgeGroup))+
  geom_line(size= 0.8)

# $$$ *Ejercicio: representa la variación de Thousands a lo largo de Year_fact, 
# para cada AgeGroup
# Pista: Year_fact es categórico 
# $$$ RESPUESTA:
ggplot(uspopage2, aes(x= Year_fact, y= Thousands, group = AgeGroup, colour = AgeGroup))+
  geom_line(size= 0.8)


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
  geom_ribbon(aes(ymin= Anomaly10y-Unc10y , ymax=Anomaly10y+Unc10y), alpha=0.2, fill="blue") +
  geom_line()

## *IC con dos líneas
###################### #
# $$$ *Ejercicio: intenta crear un gráfico de líneas con el IC representado por dos líneas, 
# considerando que:
# para cada año, el límite inferior del IC se define por: Anomaly10y-Unc10y
# para cada año, el límite superior del IC se define por: Anomaly10y+Unc10y
# Consejo: diferencia la línea principal y las del IC por color o tipo de línea (por ejemplo, colour="red", linetype="dotted")
# $$$ RESPUESTA:
ggplot(clim, aes(x=Year)) +
  geom_line(y= Anomaly10y)+
  geom_line(aes(y= Anomaly10y-Unc10y), colour= "red", linetype= "dotted")+
  geom_line(aes(y= Anomaly10y+Unc10y), colour= "red", linetype= "dotted")

?geom_line
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
  xlim(c(0, max(BOD$Time)))

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
  geom_line(position=position_dodge(0)) +
  geom_point(position=position_dodge(0.4), size=4)
## *Configuración de estética en geom_line(): linetype, size, colour
##################################### #
# $$$ *Ejercicio: modifica los valores de linetype, size, colour. 
# Consejo: puedes definir colores ya sea por código numérico o por nombre
# Consejo 2: Aquí tienes posibilidades de colores (por código y nombre):
# https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html
# Consejo 3: Opciones de linetype: "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dotdash", linewidth=2, colour="coral1")

ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dashed", linewidth=1.5, colour="coral")
# $$$ **Ejercicio: crea un nuevo patrón de linetype
# Consejo 4: puedes definir un patrón de linetype con números en pares: 
# el primero define la longitud del segmento y el segundo la longitud del espacio
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="1199", linewidth=2, colour="coral1")
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="24", linewidth=2, colour="coral1")

ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="2469", linewidth=2, colour="coral1")

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
ggplot(sunspotyear, aes(x = Year, y = Sunspots))+
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
ggplot(tg, aes(x=dose, y=length, colour =supp))+ geom_area(aes(fill= supp))

# $$$ Ejercicio: representa cómo ha variado el tamaño de la población de EE.UU. 
# a lo largo del tiempo para cada grupo de edad, usando áreas apiladas
# Datos:
uspopage <- as_tibble(uspopage)
uspopage
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, group = AgeGroup))+
  geom_area(aes(fill = AgeGroup))

## Apilado Proporcional: proporción de cada grupo: geom_area(position="fill")
################################################## #
# Datos
uspopage

# $$$ Ejercicio: especifica position="fill" como argumento de geom_area()
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area(position = "fill")

## *No Apilado: geom_area(position="identity")
################ #
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_area(position="identity")

# $$$ *Ejercicio: representa cómo ha variado el tamaño de la población de EE.UU. 
# a lo largo del tiempo para cada grupo de edad, usando áreas no apiladas
uspopage
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area(position="identity")


############ #
#### 5.4. ## Estética de los Gráficos de Área #### 
################################################## #

## Configuración de estética en geom_area: colour, fill, alpha
################################### #

### *Transparencia del área: alpha
# $$$ Ejercicio: cambia el valor del argumento alpha de 0 a 1
# ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
#   geom_area(position="identity", alpha=____)
# $$$ RESPUESTA:

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + 
  geom_area(position="identity", alpha= 0.8)


### *Líneas para resaltar el perímetro del área: colour
# $$$ *Ejercicio: dibuja una línea negra sobre las áreas en los siguientes gráficos 
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(alpha=0.4)
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(position="identity", alpha=0.2)
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(alpha=0.4, colour= "black")
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(position="identity", alpha=0.2, colour= "black")

# $$$ *Ejercicio: colorea cada línea con el mismo color que su área
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, colour = AgeGroup)) +
  geom_area(alpha=0.4)
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, colour = AgeGroup)) +
  geom_area(position="identity", alpha=0.2)

# $$$ Ejercicio*: piensa en otra forma de dibujar una línea negra sobre cada área
# Consejo: ¡piensa en construir un gráfico en capas! puedes usar diferentes funciones
# Consejo 2: necesitarás el argumento position="stack" en la nueva capa (la capa de área se apila por defecto)
# $$$ RESPUESTA:
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(alpha=0.4)+
  geom_line(position = "stack")
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(position="identity", alpha=0.2)+
  geom_line()

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
ggplot(pg_mean, aes(x=group, y=weight))+
  geom_col()

## Variable continua en el eje x
################################## #
# Datos
BOD
# $$$ Ejercicio: Representa la demanda en diferentes momentos
# $$$ RESPUESTA:
ggplot(BOD, aes(x=Time, y=demand))+
  geom_col()

# Observación: se representan todos los valores posibles entre el mínimo 
# y el máximo en el eje x, incluyendo time=6 para el cual no hay 
# información de demanda

# $$$ Ejercicio*: Representa solo los valores en el eje x para los cuales 
# hay información en y
# Pista: puedes transformar la variable x en factor usando factor()
# $$$ RESPUESTA:
ggplot(BOD, aes(x=TimeFact, y=demand))+
  geom_col()

ggplot(BOD, aes(x=factor(Time), y=demand))+
  geom_col()


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
ggplot(cabbage_exp, aes(x=Date, y=Weight))+
  geom_col()

# $$$ Ejercicio: Ahora quieres identificar qué parte del peso corresponde 
# a cada grupo de Cultivar
# Sugerencia: Puedes colorear las barras usando el argumento "fill"
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill= Cultivar))+
  geom_col()


# Observación: por defecto, las barras están apiladas (una encima de la otra)

## Gráfico de barras agrupadas - barras lado a lado: geom_col(position="dodge")
###################################### #
# $$$ Ejercicio: Para cada fecha, quieres dos barras, una al lado de la otra: 
# una para cada grupo de Cultivar
# Pista: usa la misma estructura que para el gráfico de barras apiladas, 
# pero añadiendo el argumento position="dodge"
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill= Cultivar))+
  geom_col(position = "dodge")


# $$$ *Ejercicio: ahora diferencia las barras por el color de sus líneas periféricas
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, colour = Cultivar))+
  geom_col(position = "dodge", linewidth = 1, fill= NA)

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
  coord_flip()+
  geom_col()


# Coord_flip() se puede usar con otros geoms:
# $$$ Ejercicio*: cambia el eje del siguiente gráfico de líneas:
dat <- tibble(ID=1:4, xval=2:5, yval=c(3,5,6,9), group=c("A","B","A","B"))
ggplot(dat, aes(x=xval, y=yval)) + geom_line(aes(colour=group))
# $$$ RESPUESTA:
ggplot(dat, aes(x=xval, y=yval)) + geom_line(aes(colour=group))+
  coord_flip()


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
ggplot(upc, aes(x=Abb, y=Change)) + 
  geom_col() 
# $$$ RESPUESTA:
ggplot(upc, aes(x=reorder(Abb, -Change), y=Change)) + 
  geom_col() 


# $$$ Ejercicio: ahora identifica con diferentes colores los estados del Sur y del Oeste
# $$$ RESPUESTA:
ggplot(upc, aes(x=reorder(Abb, -Change), y=Change, fill = Region)) + 
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
  geom_col(width= 0.8)

## *Controlar el espacio entre barras agrupadas (geom_col(position=position_dodge()))
######################################################## #
# $$$ *Ejercicio: modifica el espacio entre las barras en el siguiente gráfico
# modificando position=position_dodge():
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position=position_dodge(0.9))
# $$$ RESPUESTA:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position=position_dodge(1))


## CONFIGURACIÓN de estética en geom_col()
#################################### #
# $$$ *Ejercicio: define un color que te guste para todas las barras en
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col()
# $$$ RESPUESTA:
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(fill= "red")

# $$$ *Ejercicio: ahora define el color de la línea que rodea las barras
# $$$ RESPUESTA:
ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(fill= "red", colour= "black")


# $$$ *Ejercicio: Ahora haz que esta línea sea "discontinua" (Sugerencia: linetype)
# $$$ RESPUESTA:
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_col(fill= "red", colour= "black", linetype= "longdash", linewidth = 1)


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
# # $$$ RESPUESTA:

# csub <- csub|>
#   mutate(positive= Anomaly10y >= 0)

csub <- csub|>
  mutate(positive= if_else(Anomaly10y >= 0, "Positivo", "Negativo"))



ggplot(csub, aes(x=Year, y=Anomaly10y, fill = positive)) +
  geom_col()

ggplot(csub, aes(x=Year, y=Anomaly10y, fill = Anomaly10y>0)) +
  geom_col()
######################################################################################################## #
######                                                                                               ### #
######                     Iniciación práctica a la gestión de datos ambientales con R               ### #
######                              Universidad de Alcalá, 2025-2026                                 ### #
######                                 Profesora Sara Villén Pérez                                   ### #
######                                                                                               ### #
######                               IV) VISUALIZACIÓN DE DATOS EN R                                 ### #                                                                                       #####
######                                                                                               ### #
######################################################################################################## #

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
ggplot(diamonds, aes(x=price)) + geom_bar(stat = "count")


# $$$ Ejercicio: Transforma tu gráfico usando geom_bar(stat="bin")
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price)) + geom_bar(stat = "bin")


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
ggplot(diamonds, aes(x=price, fill = cut))+ geom_histogram(alpha=0.6, position = "identity")

########### #
#### 7.4 ## Estética de los histogramas #### 
############################################ #

## Controlar el número de barras en geom_histogram()
########################################## #
# $$$ Ejercicio: Modifica el parámetro bins a continuación
# ggplot(diamonds, aes(x=carat)) + geom_histogram(bins = ___)
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price, fill = cut))+ geom_histogram(alpha=0.6, position = "identity", bins = 5)


## Controlar el ancho de las barras en geom_histogram()
######################################## #
# $$$ Ejercicio*: Modifica el parámetro binwidth a continuación
# ggplot(diamonds, aes(x=carat)) + geom_histogram(binwidth = ___)
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price, fill = cut))+ geom_histogram(alpha=0.6, position = "identity", binwidth = 1000)

######################################################## #   
#### 8 ### POLÍGONOS DE FRECUENCIA: geom_freqpoly() ####   
######################################################## #
# Un polígono de frecuencia muestra la misma información que un histograma (frecuencia de cada valor),
# pero usa una línea para unir cada valor de frecuencia

########### #
#### 8.1 ## Polígono de frecuencia básico #### 
############################################## #

# $$$ Ejercicio: Adivina cómo completar el siguiente gráfico para hacer un polígono de frecuencia
ggplot(faithful, aes(x=waiting))
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting))+geom_freqpoly()

# $$$ Ejercicio*: Usa el argumento "bins" o el argumento "binwidth" para definir la precisión (como en el histograma)
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting))+geom_freqpoly(bins= 50)


########### #
#### 8.2 ## Polígono de frecuencia múltiple ####
################################################ #

# $$$ Ejercicio: ¿Recuerdas este gráfico? - Transfórmalo en un gráfico de polígono de frecuencia
ggplot(diamonds, aes(x=price, fill=cut)) + 
  geom_histogram(position="identity", alpha=0.3)
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=price, color=cut)) + 
  geom_freqpoly(position="identity")


# $$$ Ejercicio*: Representa la frecuencia de valores de heightIn para cada sexo, usando un polígono
str(heightweight)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, colour = sex))+
  geom_freqpoly(position = "identity")

########### #
#### 8.3 ## Estética del polígono de frecuencia ####
############################################# #

## CONFIGURACIÓN de estética
# $$$ Ejercicio*: Modifica el color y el tipo de línea
ggplot(faithful, aes(x=waiting)) + 
  geom_freqpoly(bins=60, colour= "blue", linetype=3)
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) + 
  geom_freqpoly(bins=60, colour= "green", linetype=1)


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



# Observación: geom_density() representa probabilidades, por lo que el área bajo geom_density() suma 1. 
# Como consecuencia, los valores del eje y son muy pequeños (mucho menores que los valores observados),
# por lo que si graficamos distribuciones observadas y estimadas juntas, no veremos variación en la curva de densidad.

# $$$ Ejercicio**: Inténtalo de nuevo.
# Consejo: reescala el eje y del gráfico de frecuencia a la escala del eje y en el gráfico estimado.
# Para ello, define aes(y=stat(density)) en el gráfico de frecuencia.
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting))+
  geom_histogram(fill= "blue", aes(y=stat(density)))+
  geom_density(fill= "yellow", alpha= 0.4)

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
ggplot(faithful, aes(x=waiting))+
  geom_histogram(fill= "blue")+
  geom_density(fill= "yellow", alpha= 0.4, aes(y=stat(count)))

# Observación: count en geom_density() es densidad * número de puntos,
# mientras que count en geom_histogram() es el número de puntos.

# $$$ Ejercicio**: Ahora reescala ambos ejes y a un máximo de 1.
# Consulta la ayuda de geom_histogram() y geom_density().
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting))+
  geom_histogram(fill= "blue", aes(y=stat(ncount)))+
  geom_density(fill= "yellow", alpha= 0.4, aes(y=stat(scaled)))

# $$$ Ejercicio**: Repítelo usando el otro gráfico de frecuencia 
# (si usaste histograma, ahora usa freqpoly).
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting))+
  geom_freqpoly(colour= "blue", aes(y=stat(density)))+
  geom_density(colour= "red")


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
ggplot(heightweight, aes(x=heightIn, fill = sex))+
  geom_density(alpha= 0.6)

# $$$ Ejercicio**: ¿Recuerdas este histograma?:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_histogram(position="identity", alpha=0.4)
# Ahora superpone el histograma con curvas de densidad para cada sexo
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_histogram(position="identity", alpha=0.4, aes(y=stat(density)))+
  geom_density(alpha=0.3)


## Curvas de densidad apiladas: geom_density(position="stack")
########################################################## #
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_density(alpha=0.4)

# $$$ Ejercicio: Crea una versión apilada del gráfico anterior, usando el argumento position="stack"
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_density(alpha=0.4, position = "stack")


# $$$ Ejercicio: Ahora crea una versión apilada proporcional, usando el argumento position="fill"
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, fill=sex)) + 
  geom_density(alpha=0.4, position = "fill")


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
  geom_density(adjust=0.1) 

# $$$ Ejercicio**: Modifica el argumento adjust en la curva de densidad para encontrar el mejor ajuste
# al polígono de frecuencia
ggplot(faithful, aes(x=waiting)) + 
  geom_freqpoly(aes(y=stat(density))) +
  geom_density(colour="red", adjust=0.7)
# $$$ RESPUESTA:
ggplot(faithful, aes(x=waiting)) + 
  geom_freqpoly(aes(y=stat(density))) +
  geom_density(colour="red", adjust=0.2)


## Configuración de estética: fill, colour, alpha
###################### #

# $$$ Ejercicio: usa fill, colour y alpha para modificar el siguiente gráfico según se indica:
ggplot(faithful, aes(x=waiting)) +
  geom_density() 
# Área azul transparente y línea azul encima
# $$$ RESPUESTA:


# *Solo área azul transparente
# $$$ RESPUESTA:


## Límites del eje x 
##################### #
# $$$ *Ejercicio: amplía los límites del eje x para ver toda la curva de densidad
# Pista: xlim()
# $$$ RESPUESTA:


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
ggplot(diamonds, aes(x=cut, y=price))+
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

# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot()+
  stat_summary(fun= "mean", geom= "point")

# $$$ Ejercicio*: verifica que el boxplot realmente representa la mediana
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_boxplot()+
  stat_summary(fun= "mean", geom= "point")+
  stat_summary(fun= "median", geom = "point", colour= "red")

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
  geom_boxplot(notch=TRUE)

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
  geom_boxplot(outlier.alpha=0.01)        # ... o valores atípicos con mayor transparencia...

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
ggplot(heightweight, aes(x=1, y=ageYear)) +
  geom_violin(colour="brown", fill="orange")

# $$$ Ejercicio: representa el precio de los diamantes en las clases de corte usando un gráfico de violín
diamonds
str(diamonds)
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y= price))+
  geom_violin(fill= "lightblue")

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
ggplot(diamonds, aes(x=cut, y= price))+
  geom_violin(fill= "lightblue")+
  geom_boxplot(fill= "black", width=0.1 , outlier.colour= NA)+
  stat_summary(fun= "mean", colour = "white")


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
  geom_violin(colour="brown", fill="orange", trim = F)
?geom_violin

## *Escala
########## #
# $$$ *Ejercicio: busca en la ayuda qué hace el argumento "scale" y
# prueba diferentes valores en el siguiente gráfico
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin(colour="purple", fill="lightblue")
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=cut, y=price)) +
  geom_violin(colour="purple", fill="lightblue", scale = "count")


## Ajuste
########## #
# $$$ Ejercicio: modifica el argumento adjust en el siguiente gráfico y adivina qué hace:
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_violin(colour="brown", fill="orange", adjust=0.9)
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=sex, y=ageYear)) +
  geom_violin(colour="brown", fill="orange", adjust=0.04)



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
# Observación: el color representa la "altura de la barra"

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
ggplot(diamonds, aes(x=carat, y= price))+
  geom_bin2d()

# Observación: ¿recuerdas? geom_bin2d() es una alternativa para
# la superposición de puntos en un diagrama de dispersión

## Una variable continua y una categórica
############################################### #
# $$$ *Ejercicio: representa el mapa de frecuencia del color y el precio de los diamantes
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=color, y= price))+
  geom_bin2d(linejoin = T)
?geom_bin2d

## Dos variables categóricas
############################## #
# $$$ Ejercicio: representa el mapa de frecuencia de la claridad y el color de los diamantes
# $$$ RESPUESTA:
ggplot(diamonds, aes(x=color, y= clarity))+
  geom_bin2d()


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
#NO

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
  stat_density_2d(aes(alpha =stat(density)), geom="raster", contour=FALSE)


############# #
#### 13.2. ## Mapa de frecuencia + mapa de densidad ####
######################################################## #
# $$$ *Ejercicio: Encuentra una forma adecuada de superponer mapas de frecuencia y densidad
# en el siguiente gráfico:
ggplot(faithful, aes(x=eruptions, y=waiting))
# $$$ RESPUESTA: 
ggplot(faithful, aes(x=eruptions, y=waiting))+
  geom_density2d()+
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
  geom_density_2d()
# $$$ RESPUESTA:

ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour = stat(level)))

## Tamaño de las curvas de nivel (hacerlas más gruesas para ver el color)
################################# #
# $$$ *Ejercicio: aumenta el argumento size para una mejor visualización del color
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), size=0.5)
# $$$ RESPUESTA:
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), size=1.5)


## Ajuste
########## #
# Recuerda: las funciones de densidad implican un nivel de ajuste a los datos

# $$$ Ejercicio: modifica los valores del argumento adjust en este gráfico
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), adjust=c(0.2, 0.1))
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
  geom_density_2d(aes(colour=stat(level)), adjust=0.2)

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


######################################################################################################## #
######                                                                                               ### #
######                     Iniciación práctica a la gestión de datos ambientales con R               ### #
######                              Universidad de Alcalá, 2025-2026                                 ### #
######                                 Profesora Sara Villén Pérez                                   ### #
######                                                                                               ### #
######                               IV) VISUALIZACIÓN DE DATOS EN R                                 ### #                                                                                       #####
######                                                                                               ### #
######################################################################################################## #

library(tidyverse) # Metapaquete tidyverse del que usaremos ggplot2 para gráficos, dplyr para manipulación de datos, tidyr para reorganizar datos
library(gcookbook) # Bases de datos que vamos a usar
library(ggrepel)   # Para etiquetas que se solapan (funcionalidad interesante que no incluye ggplot de serie)
library(hexbin)    # Para mapas de frecuencia hexagonales (funcionalidad interesante que no incluye ggplot de serie)
library(patchwork) # Para figuras combinadas (funcionalidad interesante que no incluye ggplot de serie)


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
p + facet_grid(drv ~ .)

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
ggplot(mpg, aes(x= manufacturer, y=displ))+
  geom_point()+
  facet_wrap(~ trans, nrow=5)

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
p + facet_wrap( ~ class, ncol=2, scales = "free_y")


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
  stat_function(fun = dnorm, args = list(mean = 0, sd = 5))


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
  geom_histogram(aes(y= after_stat(density)))+
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
d + scale_fill_grey(start=1, end=0)

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
dm + scale_colour_manual(values=c("green", "yellow"))

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
#install.packages("colourpicker")
# Ver en Addins > Colour Picker

## Paletas manuales seguras para personas con daltonismo  
################################## #
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
cb_palette2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")

d + scale_fill_manual(values=cb_palette)
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
c("#CD0000", "#00F5FF", "#17ED07")

############# #
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


## Saturación del color (chroma) (por defecto: c = 100) (el máximo depende del matiz y de la luminosidad)
################## #
# $$$ Ejercicio: cambiar el parámetro de saturación
p + scale_fill_discrete(c=100)
# $$$ RESPUESTA:


## Rango de matices (hue): 0 a 360 (por defecto: h = c(0,360))
#################### #
# $$$ Ejercicio: cambiar el rango de matices
p + scale_fill_discrete(h=c(0,360))
# $$$ RESPUESTA:
p + scale_fill_discrete(h=c(0,200))


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
ct + scale_alpha(range = c(0.5, 1))

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
p + scale_y_continuous(limits=c(0,10))
p + ylim(0, 10)
p + expand_limits(y=0:10)

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

sp + scale_y_continuous(breaks=c(150,200,250,300)) 

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
p+coord_flip()

############# #
#### 18.2. ## Gráficos circulares: coord_polar() ####
################################### #
# $$$ Ejercicio: Transforma el siguiente histograma en un gráfico circular
cir <- ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15) +
  scale_x_continuous(limits=c(0,360))
cir
# $$$ RESPUESTA:
cir + coord_polar()

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
  geom_point(shape=24)+
  labs(x= "edad", y= "Altura",
       fill= "Peso")+
  scale_size_continuous(guide = NULL)


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
p+ annotate("text", x=2, y=0.3, parse=TRUE,
            label="frac(1, sqrt(2 * lim(f(x), x%>%0)) + e ^(-x^2 / 2)")

############# #
#### 20.3. ## Líneas: ####
######################### #
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
p
## Líneas horizontales y verticales: geom_hline(), geom_vline()
################################# #
p + 
  geom_hline(yintercept=60) + 
  geom_vline(xintercept=14)

## Líneas diagonales: geom_abline()
################## #
p + geom_abline(intercept=37.4, slope=1.75)

# $$$ Ejercicio: Dibuja una línea 1:1 en el siguiente gráfico
ggplot(heightweight, aes(x=heightIn, y=heightIn, colour=sex)) + geom_point()
# $$$ RESPUESTA:
ggplot(heightweight, aes(x=heightIn, y=heightIn, colour=sex)) + geom_point()+
  geom_abline( slope = 1)


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
p+ annotate("segment", x= 1920, xend = 1940, y= .5, yend= .2,
            arrow= arrow(), colour= "red", size=2)

############# #
#### 20.4. ## Rectángulos: annotate("rect") ####
########################## #

p + annotate("rect", xmin=1950, xmax=1980, ymin=-1, ymax=1, 
             alpha=.1, fill="blue")

# $$$ Ejercicio: Resalta el siglo XIX en verde
# $$$ RESPUESTA:
p+ annotate("rect", xmin = 1801, xmax = 1901, ymin=-1, ymax=1, 
            alpha=.1, fill="blue")

############# #
#### 20.5. ## Barras de error: geom_errorbar() ####
########################## #
ce <- subset(cabbage_exp, Cultivar == "c39")
ce

## En un gráfico de barras
################ #
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_col() +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=0.3)

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
# Gris (predeterminado)
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
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb, fill=weightLb)) +
  geom_point(size=3, shape=24) +
  scale_colour_continuous(name=NULL) +  
  scale_fill_continuous(guide=FALSE) 


## Guardar temas en objetos para usarlos en diferentes gráficos
########################################################## #
# Por ejemplo, para darle el mismo formato a todos los gráficos de un trabajo
t <- theme(plot.subtitle = element_text(vjust = 1), 
           plot.caption = element_text(vjust = 1), 
           panel.background = element_rect(linetype = "dashed"), 
           plot.background = element_rect(fill = "antiquewhite"))

p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb, fill=weightLb)) +
  geom_point(size=3, shape=24) +
  scale_colour_continuous(name=NULL) +  
  scale_fill_continuous(guide=FALSE)
p
p+t

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
ggsave("figuras/mi_figura.pdf", width=8, height=8, unit="cm", dpi=300)
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






