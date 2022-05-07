###MANUAL DE CLASE###


# BLOQUE 1: INTRODUCCIÓN A R Y RSTUDIO -------------------------------------

  # Durante este primer día nos familiarizamos con R y Rstudio. 
  
  # Mientras que Rstudio es una interfaz que emplea R como lenguaje.
  
  # 1.1. DEFINICIÓN DE R -----------------------------------------------------
    
  # R es un lenguaje de programación que en un principio fue creado para 
  # análisis estadístico, pero es mucho más que eso:
    
    # - R es de código abierto (por lo que evoluciona constantemente adaptándose 
    #   a las nuevas necesidades de los usuarios) y de libre acceso 
    #   (por lo que todo el que quiera puede acceder a él, 
    #   lo que ayuda a su evolución).
    
    # - R ayuda al desarrollo del estado del arte al recopilar análisis y técnicas
    #   realizadas en código en investigaciones
    #   de un área de conocimiento concreta.
    
    # - Es flexible y potente, pudiendo manejar bases de datos inmensas
    
    # - Pórtatil y replicable en otras plataformas
    
    # - En el existen ejemplos para casi cualquier análisis, sirviendo de guía o 
    #   inspiración para nuevos investigadores
    
    # - Todo es por y para la comunidad de forma que todas las mejoras de R van 
    #   destinadas a suplir las necesidades de sus usuarios, y los usuarios tratan
    #   de hacer el código más accesible para los demás.
  
  
  # R, además, es muy versátil ya que en él existen una gran cantidad de paquetes,
  # para la visualización, análisis y manipulación de datos.
  
  # R comparte muchas características con la ciencia:
    # - Es gratuito
    # - Es de código abierto
    # - Es colaborativo
  
  # Con lo cual R se puede utilizar para investigaciones
  # científicas, solo debemos asegurarnos de que las investigaciones sean
  # replicables.
  
  
  # 1.2. DEFINICIÓN DE RSTUDIO: ---------------------------------------------
  
  # En cambio, Rstudio es un IDE (es decir, un entorno de desarrollo integrado 
  # (integrated development environment)) gretuito y en código abierto creado 
  # exclusivamente para R.
  
  # Así Rstudio no sería más que una interfaz de R , una "aplicación" que 
  # nos hace más sencillo trabajar con R.
  
  # Rstudio tiene las siguientes características:
              
    # -Se produce un resaltado de la sintaxis, en ocasiones se autocompleta el 
    #  código y la sangía se mantiene sola, estos aspectos ayudan al usuario a 
    #  escribir un código más limpio y a escoger los términos correctos en cada
    #  ocasión.
  
    # -Permite ejecutar el código directamente desde el editor del código fuente
  
    # -Permite saltar rápidamente entre funciones definidas
  
    # - Permite la colaboración 
  
    # -Presenta una potente autoría y una gran capacidad de depuración, de 
    #  forma que es el propio Rstudio el que avisa de posibles errores en el 
    #  código, permite generar documentos dinámicos, etc.
  
  
  # 1.3. R vs. RSTUDIO: ------------------------------------------------------
  
  # Al comparar el entorno de R y Rstudio observamos grandes diferencias:
  
    # - La principal diferencia es que el entorno de R es mucho más sencillo 
    #   que en Rstudio, en R podemos observar una barra de herramientas en la 
    #   parte superior y la ventana principal de la consola y en caso de que 
    #   abriésemos un script observaríamos la ventana de este. En cambio, en 
    #   Rstudio el entorno es claramente más complejo, la barra de herramientas
    #   de la parte superior es mucho más compleja y diferenciamos hasta cuatro
    #   ventanas diferentes, una para los scripts, otra para la consola,
    #   otra para el ambiente de trabajo donde se reflejan las variables 
    #   y datos guardados y una cuarta ventana donde se muestra la ayuda,
    #   gráficos e incluso los archivos del ordenador para abrirlos más 
    #   fácilmente si fuese necesario, todas estas ventanas poseen diferentes
    #   pestañas por las que navegar.
    
    # - Otra diferencia es la clave de colores que podemos observar en ambos
    #   entornos. En R solo hay dos colores, uno para el texto escrito con 
    #   almohadillas (#), es decir, texto no ejecutable; y otro para el 
    #   texto ejecutable. En cambio, en Rstudio existe un código de que colores 
    #   que además de distinguir el texto ejecutable del no ejecutable diferencia
    #   entre los distintos caracteres (por ejemplo, los paréntesis, corchetes y
    #   flechas de asignación son de un color diferente al del resto del texto, 
    #   y el texto entre comillas es de otro color diferente)
    
    # - Otra diferencia importante es que en R no existen avisos de errores, o
    #   sugerencias de código, como sucede en Rstudio.
  
  # Simplemente al observar estas diferencias se pone de manifiesto que Rstudio
  # facilita enormemente el trabajo con R.
  
  
  # 1.4. ¿POR QUÉ USAR EL LENGUAJE DE PROGRAMACIÓN R?: ----------------------------
  
  # R es un lenguaje de programación complejo que se asemeja al propio lenguaje
  # humano y que va mucho más allá del análisis estadístico.
  # Una razón para trabajar con R sería esta, ya que al ser un lenguaje que se 
  # asemeja al humano resulta más intuitivo de utilizar, a pesar de su inmensa
  # complejidad.
  
  # Otra razón es que gracias a sus características permite desarrollar 
  # trabajos reproducibles a nivel científico.

  ##### NOTA #################################################################
  # En la actualidad se pretende fomentar el desarrollo de la ciencia 
  # reproducible, de forma que no solo el manuscrito sino también los datos
  # y el código empleado estén disponibles a la comunidad científica, lo cual
  # aceleraría el avance en el conocimiento.
  # A nivel académico la reproducibilidad es importante al facilitar la 
  # colaboración, la verificación y el avance en la ciencia.
  # Pero a nivel de las empresas también es importante ya que permite la 
  # reutilización del código lo que ahorra tiempo, por ejemplo.
 

  # 1.5. EXPLORACIÓN DE R Y RSTUDIO: ----------------------------------------

  # Hemos realizado una exploración previa de R y Rstudio.
  
  # En R hemos realizado un ejemplo de una operación básica:
  
    10e-78+10e-40
  
  # En Rstudio hemos creado una variable e instalado un paquete:
  
    a <- 45+7
    
    install.packages("lattice")
    library(lattice)
    demo(lattice)



# BLOQUE 2: PROGRAMACIÓN BÁSICA -------------------------------------------

# Veremos conceptos básicos sobre el código de R
    

    # 2.1. PAQUETES Y LIBRERÍAS: ----------------------------------------------

      # 2.1.1. Concepto de función y vector -------------------------------------
    
      # Una función reúne una serie de operaciones que se ejecutan como un todo,
      # y queue se pueden almacenar para usarlas cuando sea necesario
      # siempre tiene una entrada y una salida.
      # Un vector es una consecución de números.
      
      # En clase hemos trabajado con diferentes funciones y hemos diferentes
      #creado vectores para afianzar estos conceptos.
      
      1+1
      a= (1+1) # creación de un objeto
      b <- 3+1 # es mejor nombrar con flechas (primero < y luego -)
      c <- 4+2 # el shortcut es Alt + -
      d<- c (1,2) # para construir un vector se hace con c (concatenar) 
                  # y la serie de números que quieras entre paréntesis 
                  #separados por coma.
      sum(b) # sum es la función de suma, como vemos resume una operación
      sum(d,c,b)
      str (a) # esta función te permite ver la estructura del objeto
      

     # 2.1.2. Uso de paquetes y librerías, exploración de funciones ------------
  
      # Un paquete y una librería son prácticamente lo mismo. 
      # Una librería o paquete es una consecución de funciones que nos
      # permiten trabajar más fácilmente en R.
      
      ### SHORTCUTS INTERESANTES###
      # para poner sección es ctrl + shift + r
      # para guardar ctrl + s
  
      ### INSTALAR UN PAQUETE ###
      
      install.packages("tidyverse") # con esta función se descargan e 
                                    # instalan paquetes
      
      ### USO DE UN PAQUETE ###
      
      # A pesar de haber instalado el paquete solo podremos usar sus funciones
      # si se lo indicamos a R, es decir, debemos llamar al paquete para
      # poder utilizar sus funciones, y para ello usamos al función "library",
      # un simil para entenderlo podría ser entender la función 
      # "install. packages" como comprar un libro en una librería, mientras que
      # la función "library" sería como coger ese libro de la estantería para
      # leerlo.
      
      library(tidyverse)#para cargar librería
      
      # Instalamos y cargamos posteriormente la librería raster:
      
      install.packages("raster")
      
      library (raster)
      
      
      ### CONCEPTO Y EXPLORACIÓN DE UN DATA FRAME ###
      
      # Un dataframe no es más que una tabla de datos, podemos tener datos
      # cualitativos y cuantitativos, cada fila es una observación y las 
      # columnas son las variables.
      
      # Para explorar un data frame podemos utilizar diferentes funciones:

      names(iris) # para decirme las variables de este dato
      
      str(iris) # me dice la estructura de esta variable
      
      petal <- select (iris, Petal.Width) # así me da error, porque
                                          # select es una función que sale en 
                                          # el paquete de raster y de tidyverse,
                                          # le tengo que decir de quién quiero 
                                          # que lo use
      
      # Así para seleccionar que "select" queremos usar utilizaremos 
      # "dplyr" que  es un paquete que contiene funciones para la manipulación 
      # de dataframes, al usar los "::" indicamos que queremos utilizar "select"
      # de este paquete:
      
      petal <- dplyr::select (iris, "Petal.Width")
      
      #para quitar datos del dataframe:
      
      nuevo2 <-  dplyr::select (iris, - "Petal.Width")# para hacer cosas
                                                      # debemos nombrar la tabla
                                                      # y la variable que quiero
      
      dim(iris) # dimensiones de tabla
      nuevo <- 1:10
      dim(nuevo) # Rstudio genera un warning si haces referencia a un objeto 
                 # que no esta previamente definido en el script
      
      ### Eliminar una columna, ¿qué es más reproducible ###
      
      new <- iris[,-4] # quitas las cuarta columna
      # Shorcut de corchetes en mi teclado, Alt gr + A0 (a con la o encima)
      
      new <- iris[,-("Petal.Width")] #con nombre es más reproducible ya
                                     # que la posición puede cambiar
      
      new2 <-  iris [,1:3]
      
      new3 <-  iris [,c(1:3)]# de estas dos maneras es correcto los : son los 
                             # rangos, izquierda son filas y derecha columnas
      
      new4 <-  iris[1:10,1:3] # elijó las primeras 10 filas (observaciones), 
      # y las 3 primeras columnas 
      # Usando rangos solo es válido con posiciones, los nombres son caracteres
      # y no los localiza, para hacerlo con nombres tiene que escribir todos 
      # los nombres de esas columnas
  
      col_13 <- iris[1:10,1:3]
      col_13_names <- iris[, c("Sepal.Length","Sepal.Width","Petal.Length")]
      
      # el signo del dolar $, es para buscar dentro de la tabla, una variable
      # específica
      
      # Con "mean" calculamos la media:
      
      mean(col_13)# da error, porque col_13 es un dataframe no un vector,
      # tiene 4 vectores
      #na= son valores que desconoce, que no están disponibles
      
      mean (col_13$"Petal.Lenght") # aquí estoy haciendo la media de una 
                                   # variable o columna por lo que si funciona
      
      # Con la función "data" podemos cargar dataframes aportados por 
      # los paquetes en el ambiente de trabajo:
       
      data(iris)

     # 2.2. AYUDA --------------------------------------------------------------
      
      # La ayuda de Rstudio es un elemento sumamente práctico que nos permite
      # encontrar información sobre las funciones y ejemplo de su uso.
    
      # Existen varias formas de usar la ayuda a través de la consola. 
      # Podemos usar una interrogación:
        
        ?mean # al ejecutarlo se abre una página donde se describe su
              # funcionamiento y se pone un ejemplo de cómo se utiliza
      
        ?? weighted.mean # busca las páginas de ayuda donde se menciona el
                         # término
        help.search("weighted.mean") # realiza la misma función que ??
        
        # podemos encontrar información sobre paquetes también:
        
        help (package = "dplyr")
      
      
      # Además de las funciones str y summary que nos dan información sobre
      # una variable, podemos usar "class" para saber el tipo de variable:
        
        class (iris)
        
      ### EJEMPLO REPRODUCIBLE ###
      
      # Si aún habiendo usado estas funciones sigues sin resolver tus problemas
      # o dudas, puedes acudir a foros online para que otros usuarios te ayuden, 
      # pero para que te puedan ayudar debes crear un ejemplo reproducible.
        
      # Un ejemplo reproducible es un ejemplo del código que te esta dando
      # problemas, que es simplificado, pero con todas las partes para que 
      # se reproduzca tu problema y con diferentes datos.
      # A este respecto, existen paquetes de R como reprex para generar 
      # estos ejemplos.
        

    # 2.3. DIRECTORIO DE TRABAJO ----------------------------------------------

      #A la hora del directorio de trabajo existen varias funciones:
        
        getwd() # para saber cual es nuestro directorio de trabajo
        
        setwd() # para seleccionar el directorio de trabajo, de esta manera
                # se selecciona el directorio en el que nos encontramos
                #se puede hacer con la pestañita, pero mejor no , ya que si
                # se hace en el script queda registrado
        
        setwd("C:/Users/tsuba/OneDrive/Escritorio/3º Biología
              /CIENCIA DE DATOS- C2/0-MANUAL DE CLASE")
        # Entre comillas y con barras
        # SHORCUT DE MI TECLADO: para la barra al revés Alt gr + tecla de grados
        
        ### PROYECTOS ###
        # Recomiendan trabajar con proyectos, cada proyecto lo suyo es que sea
        # de un proyecto de análisis.
        # PROYECTOS: son como una "especie" carpeta de trabajo, es realmente 
        # una forma de organización.
        
        ### NOTAS ###
        # "select" es para columnas, "filter" hace los mismo pero para las
        # observaciones o filas
        # se suele poner el nombre de la variable
        
        
        ### PIPA, FUNCIONAMIENTO Y CONCEPTO ###
        
        # La Pipa o %>%  es un carácter que en el código me indica dentro
        # de..., ej aproximado:
        
        iris%>%
          dplyr::select (
            "Species", "Sepal.Width", "Petal.Lenght")%>%
          filter(iris [,1:3])
        
        #Funciona como el del $, pero con más cosas
        
        
        ### ¿CÓMO CREAR UNA FUNCIÓN? ###
        
        #Algunos usuarios por su propia comodidad generan sus propias funciones,
        # para crear una función se sigue la siguiente estructura:
        
        # nombre <- function (dataframe, x=variable){df[,x]}
        
        # Aquí las llaves indican lo que hacen las funciones
        
        # Importante: las funciones que creas aquí son tuyas, para que se hagan
        # públicas las tienes que poner en github o publicar un paquete público
        # de R.
        
        # SHORCUT DE MI TECLADO: las llaves se ponen con Alt gr + A con dieresis)
        

    # 2.4. VECTORES -----------------------------------------------------------


      # 2.4.1. Concepto de vector y propiedades ---------------------------------

      # Un vector es conjunto de datos del mismo tipo, 
      # dispuestos en serie (uno detrás de otro),
      # es una sola columna, una  única variable, si es de una
      # dimensión sería un escalar.
        
      # Los vectores presentan las siguientes propiedades o características:
        
        # - Tipo de dato, así los vectores pueden ser:
        
                # - Numéricos (pudiendo ser número enteros o dobles (decimales))
                # - Factor (pudiendo tener varios niveles)
                # - Caracteres
                # - De fecha  
                # - Boleano (de unos y ceros / verdadero-falso)
        
        # - Tamaño/ longitud
        
        # - Atributos/metadatos adicionales
        
        # Con los vectores se pueden realizar operaciones básicas donde: 
        # + se utiliza para sumas, - para restas, * para multiplicación
        # (en mi teclado es shift + tecla de exponente), / para división
        # y ^ para exopnentes (en mi teclado es shift + A con o).
        
        
        ### 2.4.1.A. Ejemplo 1: entender las propiedades de los vectores #####################
        
        # Para averiguar las propiedades de un vector se pueden usar diferentes
        # funciones.
        
        # Creamos un vector de altura
    
        altura <- c(1.65,1.70, 1.45, 1.56, 1.75) 
       
        # Para saber propiedades:
        
        typeof(altura) # Tipo
        
        length(altura) # Tamaño
        
        # NOTA: el asistente inteligente puede salir automático o buscarlo con
        # el tabulador y bajar con flechas para seleccionar
        
        attributes(altura) # Atributo, NOTA: null indica que no hay nada que
                           # enseñar
        
        # Una función que nos los enseña todo seria:
        
        str(altura) # Muestra el tipo y longitud
        
        summary(altura) # También nos muestra el tipo (en caso de una variable
                        # numérica calcula los percentiles y media, en una
                        # variable tipo caracter indica el tipo de dato y
                        # el tamaño o longitud del vector)
                        
        # A continuación vamos a hacer una distribución normal de media 1.70
        # y desviación estándar 0.15, para lo cual debemos buscar la función
        # adecuada y utilizamos la ayuda.
        
        ??norm # Sabemos que el término norm se usa para normal, así que 
               # buscamos todas las entradas con este término, al final vemos
               # que en el paquete stats se menciona norm para una distribución
               # normal y al pinchar vemos distintas funciones y las exploramos
      
        ?dnorm # un compañero comenta que puede ser esta, así que la exploramos,
               # al explorarla vemos que no nos sirve
        
       # En realidad, la función adecuada es rnorm y es la que usamos, ya que
       # esta usa el número de observaciones, ya que dnorm usa un vector 
       # de cuantiles.
        
        altm <- rnorm (5, mean=1.74, sd=0.15) # Si no pongo  mean y sd sigue
                                              # funcionando igual por el orden 
                                              # de estos argumentos
        
        altm <- rnorm (5, 1.74, 0.15) # Como es comentado funciona igual
        
        ### INCISO, ¿PONER O NO LOS ARGUMENTOS? ###
        
        # Depende de la situación. Si tenemos que enseñar nuestro código a
        # alguien o si tenemos pensado revisar el script más adelante, lo mejor
        # es ponerlo, ya que la lectura es más sencilla y no tenemos que
        # memorizar. En cambio, si es para algo rápido sobre lo que probablemente
        # no volvamos a trabajar, lo mejor es no ponerlo, ya que nos ahorra 
        # tiempo.
        
        # Ahora con "summary" comparamos como se ajusta nuestro vector a la
        # distribución normal:
        summary(altura)
        summary(altm) 
        
        # Al comparar observamos que nuestra media es menor
        # a la media de la distribución normal, somos más bajitos
        
        
        ### NOTA DE CLASE con aclaración personal ###
        
        # Existen dos tipos de programación, una programación funcional
        # y una programación orientada a objetos. La primera está más orientada
        # a funciones, donde el usuario expresa lo que quiere hacer, haciendo que
        # la programación sea más intuitiva y fácil de leer, R sería un lenguaje
        # de programación funcional. 
        
        
        ### 2.4.1.B. Ejemplo 2: trabajo con figuras ############################
        
        # Ahora vamos a recordar cómo realizar gráficos y figuras y cómo 
        # guardarlas. Para ello trabajaremos con los vectores que hemos creado
        # anteriormente sobre la altura (altura y altm).
        
        # Algunas funciones para generar gráficos son:
        
        plot(altura) # genera un gráfico de dispersión
        boxplot(altura) # caja de bigotes
        hist(altura) # histograma
        
        # Lo que queremos es generar una sola figura que contenga, un gráfico
        # de dispersión, una caja de bigotes y un histograma para las variables
        # altura y altm, y así poderlas comparar más fácilmente.
         
        # Como queremos guardar dicha figura, lo primero que haremos será usar 
        # la función "tiff".
        # Cabe mencionar es una función que permite guardar una figura o imagen
        # en formato tiff, este es un formato del paquete racster, en el que
        # definimos la cantidad de píxeles por unidad de medida. Este código 
        # debe de ejecutarse antes de la figura o imagen que deseamos guardar.
        
        # Para saber como funciona "tiff" utilizamos la ayuda y a continuación 
        # lo usamos.
        
        ?tiff 
        x11() # añadido posteriormente ya que nos daba error, esto permite
              # abrir una nueva ventana si te da error al ejecutar par
       
        tiff(filename = "ejemplo",# aquí puedes eligir una subcarpeta poniendo
                                  #la dirección
             width = 100, height = 250, units = "mm",
             res= 300) # si no pones comillas los caracteres se consideran 
                       # objetos
        
        # Como queremos que todos los graficos se ejecuten a la vez y no salgan
        # en una sola imagen usamos par.
        ?par
        
        par(mfrow= c(3,2))# Aquí estamos definiendo la disposición que queremos,
                          # así definimos que queremos 3 filas con 2 columnas
                          # Aunque originalmente definimos 2 filas y 3 columnas
                          # a mi parecer con esta disposición se entiende mejor.
        plot(altura, # aquí con type podrías elegir el tipo de gráfico pero por
                     # defecto es un gráfico de puntos
             col = "aquamarine4", # "col" define el color
             ylab = "Altura (m)",
             main = "Altura de clase", # "main" es el título
             pch = 16 )  # "pch" define el tipo de punto,
                         # en total hay 25 tipos, el tamaño de los puntos
                        # también se puede cambiar con "cex"
        plot(altm, col = "darkcyan", ylab = "Altura (m)",
             main = "Altura de España", pch = 16)
        boxplot(altura, main = "Altura de clase", col = "lightpink") 
        boxplot(altm, main = "Altura de España", col = "coral2" )
        hist(altura, ylab = "Altura (m)", main = "Altura de clase",
             col = "palegreen3")
        hist(altm, ylab = "Altura (m)", main = "Altura de España",
             col = "seagreen4")
        
        dev.off() # para cerrar gráfico, y poder hacer otro nuevo 
        # Tras ejecutar "dev.off" sale el mensaje de null device
      
        ### FORMATOS DE IMÁGENES ###
      
        # Para guardar imágenes hay dos formatos posibles en formato
        # vectorial y en formato foto.
        

        # 2.4.2. USO DE VECTORES PARA OBTENER DATAFRAMES --------------------------

        #### 2.4.2.A. Creación de vectores con "rep" ############################
        
        # A través de la repetición de los datos un vector podemos generar
        # vectores de mayor tamaño.
        altura <- c(1.65,1.70, 1.45, 1.56, 1.75)
        
        altura2 <-  rep(altura, 30) # Produce la repetición de los valores del
                                    # vector altura, en este caso 30 veces
      
        # A continuación vamos a trabajar con otra variable, la variable genero,
        # donde anotaremos nuestro género en clase:
        
        genero <- rep(c("mujer", "hombre"), c(3,2)) # con "rep" estamos 
                                                    # consiguiendo repetir 
                                                    # "mujer" 3 veces y 
                                                    # "hombre" 2 veces
        
        # Pero hasta ahora la variable género es considerada como un vector
        # de tipo caracter, pero para nosotros es un factor con 2 niveles o
        # categorías nominales: mujer y hombre. Por ello, vamos a trasformar
        # la variable genero en un factor usando la función "as.factor":
        
        genero_clase <-  as.factor (genero)#como factor
        
        # Observamos las diferencias entre las variable genero y genero_clase:
        
        typeof(genero)
        str(genero)
        summary(genero)
        typeof(genero_clase)
        str(genero_clase)
        summary(genero_clase)
        
        
        #### 2.4.2.B. Función "seq" y creación de dataframes #################################
        
        # Para unir tablas estas deben tener datos del mismo tipo.
        
        # La función "seq" genera secuencias aleatorias de números
        # de una determinada cantidad de observaciones. En el ejemplo de abajo
        # definimos que queremos una secuencia de números entre el valor mínimo
        # y el valor máximo del vector altura, teniendo un total de 10000
        # observaciones.
        
        seqalt <- seq( from =min(altura), to = max (altura),
                       length= 10000)
        
        View(seqalt) # Ahora observamos el vector creado con esta función 
                     # (primera letra en mayúsculas)
        
        boxplot(seqalt, main= "Altura (muestra aleatoria)", 
                col = "cornflowerblue") # generamos un boxplot para visualizar 
                                        # más facilmente los datos
        
        # Ahora procedemos a generar una matriz de datos a partir de este 
        # vector númerico doble. Para ello, simplemente rompemos el vector 
        # seqalt en una matriz de cuatro columnas usando la función "matrix":
        
        ?matrix # Para saber utilizar esta función consultamos a la ayuda
        
        alt <-  matrix(data= seqalt, nrow =100,
                       ncol=4, byrow=F) # Como defino el número de filas que
                                        # quiero que tenga en realidad estoy 
                                        # acortando el dataframe
        
        # Por ello, a continuación vamos a realizar el dataframe completo:
        
        alt <-  matrix(data= seqalt, 
                       ncol=4, byrow=F) # Si quito el nrow, R es inteligente
                                        # y me parte todo de la forma adecuada
                                        # una matriz es un vector con 
                                        # dimensiones, entonces byrow me dice
                                        # que me lo parte siguiendo filas o 
                                        # columnas, de forma default es 
                                        # columnas que es como se hace bien
                                        # una matriz tiene el mismo numero de
                                        # filas, es un dataframe no es 
                                        # una matriz
        
        View(alt) # Observamos la matriz que hemos generado
        
        ### AHORA VAMOS A GENERAR UN DATAFRAME ###
        
        # Vamos a generar un dataframe compuesto por dos variables, por un lado
        # el dato numérico de la altura registrado, y por otro el rango de 
        # valores al que pertenece dicho valor.
        
        # En primer lugar, vamos a hacer un vector tipo factor o caracter
        # que me rompan en 3 partes iguales la muestra, los puntos de división 
        # que vamos a establecer son: X < cuantil 25, 25 < x < 75 (entre el 
        # cuantil 25 y el 75) y x > cuantil 75.
        
        # Para ello primero buscaremos los cuantiles:
        quantile (seqalt)
        quantile( seqalt, 0.25)
        quantile(seqalt, 0.75)# estos cuantiles son mis puntos de corte
        
        # Con los puntos de corte crearemos un vector en el que se definen
        # los intervalos en los que se dividirá el vector seqalt, este nuevo 
        # vector se llama intervalos.
        
        ?cut
        intervalos <- cut (seqalt, breaks= c(min(seqalt), # le pongo los puntos 
                                                          # de mínimo, medio y 
                                                          # máximo
                                     quantile(seqalt, 0.25),
                                     quantile(seqalt, 0.75),
                                     max(seqalt)))
        
        data <-  data.frame (seqalt, intervalos)# puede ser así haciéndolo por
                                                # separado o todo junto como
                                                # abajo
        
        data <-  data.frame (seqalt, 
                             kk=cut (seqalt, breaks= c(min(seqalt),
                                                       quantile(seqalt, 0.25),
                                                       quantile(seqalt, 0.75),
                                                       max(seqalt)), 
                                     labels = c("<0.25", "0.25-0.75", ">0.75")))
        
        
        summary(data) # vemos un resumen del dataframe
        
        boxplot(data$seqalt~data$kk,
                col = "lavander") # representamos los datos de cada intervalo
                                  # el primero es la y , y el segundo es la x
        
      # 2.4.3. DIFERENCIA ENTRE LISTA Y VECTOR ----------------------------------

        # La principal diferencia entre un vector y una lista, es que un vector
        # es una serie de datos del mismo tipo, mientras que una lista puede 
        # estar compuesta por elementos de distinto tipo.
        
        # NOTA: El shortcut para ir al paréntesis correspondiente Ctrl + p
        

      # 2.4.4. OPERACIONES BÁSICAS CON VECTORES ---------------------------------

        ### 2.4.4.A. Funciones matemáticascon vectores ##################################
        
        # Hay numerosas funciones que podemos usar:
        
            # - log(x): logaritmo natural
            # - exp(x): exponencial
            # - max(x): máximo
            # - min(x): mínimo
            # - round(x,n): redondear decimales
            # - cor(x): correlación
            # - sum(x): suma
            # - mean(x): media
            # - median(x): mediana
            # - quantile(x): cuantil
            # - rank(x): ranking de elementos
            # - var(x): varianza
            # - sd(x): desviación estándar
        
        ### 2.4.4.B. Operadores lógicos y relacionales ########################
        
           # > : mayor que
           # < : menor que
           # > = : mayor o igual que
           # < = : menor o igual que
           # = = : exactamente igual a
           # ! = : No igual a 
           # ! : lógico NO
           # & : lógico Y
           # | : lógico O (ctrl + 1)
           # && : Y con IF
           # || : O con IF
           # %in% : que contiene

      # 2.5. OTRO TIPO DE OBJETOS -----------------------------------------------

        # Además de vectores, existen otro objetos que almacenan información y
        # con los que ya hemos trabajado.
        
        # Están las matrices, las matrices son vectores con atributos de
        # dimensión, es decir, se trata de un objeto homogéneo (formado
        # por un mismo tipo de dato, como un vector)y formado por dos 
        # dimensiones (observaciones y variables, es decir, filas y columnas,
        # los vectores están formados únicamente por observaciones, son una
        # fila)
        
        # También encontramos listas, un objeto formado por una dimensión (solo
        # observaciones) y de datos diferentes (heterogéneos).
        
        # Por último, están los dataframes, que son objetos heterogéneos 
        # (formado por diferentes tipos de datos) con 2 dimensiones (filas y
        # columnas).
        
        # Estos tipos de objetos se pueden interconvertir entre ellos,
        # a través de diferentes funciones:
        
          # - De vector a un vector más largo: c(x,y)
          # - De matriz a vector: as.vector(mymatrix)
          # - De vector a matriz: cbind(x,y), rbind(x,y)
          # - De dataframe a matriz: as.matrix(myframe)
          # - De vector a dataframe: data.frame(x,y          
          # - De matriz a dataframe: as.data.frame(mymatrix)

    # 2.6. RECOMENDACIONES DE PROGRAMACIÓN ------------------------------------

       # En este apartado vamos a ver recomendaciones para la escritura del
       # código.
        
       ### INCISO: REVISTAS CIENTÍFICAS Y CITACIÓN ###
        # Una revista científica recoge artículos de revisión, investigación,
        # etc; que mantiene un formato concreto.
        # Todas las publicaciones científicas están revisadas.
        # Las fuentes fiables de información muestran datos como: autores, 
        #revista, editor, etc.
        # Se pueden citar y referenciar conocimiento previo, metodologías
        # y bases de datos.
        # También se debe citar el software y paquete usado.
        # las citas textuales solo se pone apellido y año.
        
        # Repositorio: sitio web donde se almacenan muchos datos.
        
        # ¿Cómo buscar información?: podemos busacr información de muchas 
        #maneras, en bases de datos, usando palabras clave, buscando a un autor
        # específico, etc. Algunas bases de datos son easy web of knowdelge y 
        # scopus, incluso en buscadores como google scholar.
        
        # A la hora de buscar un artículo, el criterio para seleccionar los 
        # artículos más relevantes es guiarse por:
           # 1. mayor número de citas
           # 2. Filtrar el área de conocimiento del artículo para escoger 
           # artículos de áreas relevantes en nuestro estudio
           # 3. Intentar seleccionar artículos de revisión, ya que resumen
           # el conocimiento actual sobre el tema
           # 4. Escoger artículos recientes ya que tratan los últimos 
           # descubrimientos del tema.
        
        #Si encontramos un artículo que nos inspire debemos guardarlo ya que
        # puede resultar útil para investigaciones futuras.
        

     # 2.6.1. RECOMENDACIONES DE PROGRAMACIÓN Y FLUJO DE TRABAJO EN R ----------

        
        #1). Comprobar que en "global options"  no esté marcado 
        # "R restore data", ya que si se restaura, al trabajar con
        #  datos muy grandes hace que tarde más en abrirse Rstudio.
        
        
        #2).Generar un proyecto organizado, ¿Cómo?: 
        
        # - Generando carpetas para los tipos de archivos, por ejemplo:
        
              # - Datos, donde se recogiesen los dataframe que voy a usar en un 
              # proyecto
              #- una carpeta para los scripts
              # - una carpeta de output (figuras, tablas y resultados)
              # - una carpeta de informe (con un rmarkdown)
        
          # Se pueden numerar las carpetas para que sigan un orden lógico en el
          #que los usarías o generarías. En m,i caso yo realizaría primero la 
          # carpeta de scripts, posteriormente, datos, resultados y finalmente 
          # el informe; en cambio la profesora seguiría el orden de primero
          # los datos, segundo scripts, despues resultados y finalmente informe.
          # La creación de una gran cantidad de subcarpetas supone un problema
          # y es que el directorio de trabajo se tendría que cambiar más veces
          # ya que  tienes que especificar esas subcarpetas en el directorio de 
          #trabajo (directorio de trabajo es el sitio donde se guarda el script).
        
        #- Nombres de los archivos, asegurandonos de elegir un nombre que sea
        # memorable (fácil de recordar) y que nos ayude a identificar el 
        # contenido o tipo de archivo.
            # A.EJEMPLO:
            # ¿Qué nombre es el más correcto para un script que ajusta modelos?
            #  1. ajuste-modeloR 
            #  2. a-modR 
            # El mejor sería el primero ya que es completo y fácil de recordar,
              # el segundo, pasado un tiempo ya no te acuerdas.
              
          # ¿Qué es mejor hacer un script único con secciones o con varios 
          # script? En principio si esta organizado da igual, si se rompe  y
          # los script Deben ejecutarse secuencialmente, deben numerarse 
          # para evitar errores.
        
        ### INCISO: REGLA DE ORO PARA ESCRIBIR ###
            
        # Para escribir un documento lo más apropiado es un tema, un párrafo
        # Debemos pensar estructura y nombres para nuestro proyecto de prácticas
        
        
        #3). El código debe tener una buena sintaxis y que sea fácil de leer:
        
            #- Documenta el código/ comenta el código para que los demás lo
            # entiendan o lo entiendas tu tiempo después. No hay que comentar
            # todo, decirlo de forma resumida y que tenga estructura.
            
            #- Usar nombres memorable y correctos:para que nombres sean 
            # correctos hay que tener en cuenta que r es sensible a mayúscula
            # y minúscula. Las palabras deben de estar separadas por un guión 
            # bajo, y asignar nombres claros.
            
            ## EJEMPLO ## 
            #¿Cuál sería correcta?
            #mi_vector -> correcto
            #MiVector
            #vector.1
            
            ### CUESTIÓN DE CLASE ###
            # ¿Para qué usarías nombre y verbo?
            # El nombre para variables y el verbo para resultados de una función.
            # Consejo: evitar nombres que sean comunes de funciones y variables 
            # Para ampliar estas recomendaciones mira en la página web:
            # https://style.tidyverse.org/
        
                  ######## REGLAS DE NUESTRA SINTAXIS ####################################
                  
                  # 1. Poner espacio después de la coma, no antes:
                      # x[, 1] -> SÍ
                      # x[,1] -> NO
                  
                  # 2. Sin espacio antes/ paréntesis:
                  # mean(x, na.rm = T) 
                  # mean (x, na.rm = T)
                  # ¿Por qué? pues porque son los argumentos de la función es 
                  # como si fuese una palabra, por ello no se pone espacio.
                  
                  
                  # 3. Antes y después de una flecha de asignación, = , 
                  # tabulación, etc; deja un espacio, ayuda a leer.
                      # a <- 1
                      # a<-1
                  
                  # 4. Corregir sintaxis del manual, la tabulación son los 
                  # espacios que quedan al escribir entre varias lineas:
                      #a <-  c(1,
                      #       2, # estos espacios
                      #      3)
                  
        
        #4). Estructura el código y piensa las secciones:
        
        #Ejemplos de secciones:
        
        # 1. Directorio de trabajo (si no fuese un proyecto)
        # 2. Instalación los paquetes y librerías que vayas a usar (al principio)
        # 3. Cargar datos
        # 4. Analizar los datos
        # 5. Obtener y guardar los resultados
      
        
        # NOTA : tabulación es la tecla encima de mayúsculas (flechitas) y
        # alt+ tabulación permite pasar entre ventanas.
        
        
        ######### INCISO: ¿QUÉ ES GITHUB? ##########################################
        
        # Git es como hacer proyectos, pero en vez de tenerlo en el programa 
        # lo tienes en la nube, así tienes control de versiones mucho
        # más fácilmente, además de que se puede compartir.
        

    # 2.6.2. EJEMPLO COMPLETO -------------------------------------------------
    
        # Para poner en práctica todas estas recomendaciones realizaremos un 
        # ejemplo completo. En este ejemplo vamos a seguir el hilo de mi 
        # práctica y realizaremos un estudio de la biodiversidad de aves según
        # tipo de uso del suelo.
    
        # Aquí vamos a usar la librería dplyr y ggplot2, en caso de no estar 
        # instaladas las debemos instalar:
        
        install.packages(c("dplyr", "ggplot2"), dep=T) 
        #dep, es dependencies and construction, significa que te instalará
        #y descagará lo que le haga falta para los paquetes
        
        library("dplyr") # lo llamas
        
        # Generamos un dataframe con datos aleatorios
        
        x <- data.frame(id = seq(from = 1, to =100),
                        uso = rep(c("A", "B"), times = 50),
                        riqueza = rnorm (100, mean = 3, sd = .5))
        
        ?write.csv
        
        write.csv(x, file= "C:/Users/tsuba/OneDrive/Escritorio/
                  3º Biología/CIENCIA DE DATOS- C2/0-MANUAL DE CLASE/
                  1-datos brutos/diversidad_usos.csv")
        # Guardamos el dataframe como un csv, en la dirección se debe cambiar
        # la posición de la barra.
        
        ### INCISO: EXTENSIONES DE LOS ARCHIVOS ###
        
        # Cada archivo tiene una determinada extension y la extensión me 
        # indica el tipo de archivo:
        #  - txt es separado por /  # es un texto plano
        #  - csv es separado por ,
        #  - xls pueden tener diferentes hojas para unos datos determinados
        #  - access es mdb es como un punto mas que excel
        #  - excel tiene varios problemas, por ejemplo, un mnº limitado de 
        #    columnas
        
        # Ahora leemos el dataframe (lo cargamos)
        ?read.csv
        datos <- read.csv("C:/Users/tsuba/OneDrive/Escritorio/
                  3º Biología/CIENCIA DE DATOS- C2/0-MANUAL DE CLASE/
                  1-datos brutos/diversidad_usos.csv")
        
        View (datos)
        # Al observar los datos vemos que se ha generado una nueva columna
        # esta columna llamada X se corresponde con datos id 
        # que se han generado automáticamente en la tabla.
        
        # Vemos las características del dataframe
        
        str(datos) # la mejor forma
        summary (datos) # te indica hasta los NA (sin datos)
        
        # ¿Cómo podemos quitar la columna autogenerada que sobra?, para ello
        # saremos el código de abajo, donde - resta una columna.
        
        datos1 <- datos[,-1] # quito la primera columna que es un autonumérico
        
        
        
        # A continuación vamos a analizar nuestro dataframe
        
        names (datos1) # para saber mis variables
        
        # Haremos la media y sd (desviación típica) de riqueza
        # para cada nivel del uso del suelo.
        
        mean(datos1$riqueza)
        # podemos usar tapply(), esta agrupa los datos de un vector de
        # acuerdo a otra variable categórica y una función.
        
        # Quiero ver la media de la riqueza en función del uso del suelo:
        
        summary(datos1$uso) # me lo considera como algo lineal, entonces 
                            # me da error el tapply
        summary(as.factor(datos1$uso)) # ahora que lo veo como factor 
                                       #  y si me lo coge
        
        medaves <- tapply(datos1$riqueza, datos1$uso, FUN = mean)
        
        sdaves<- tapply(datos1$riqueza, datos1$uso, FUN = sd)   # todo esto hay
                                                                # que ir 
                                                                # guardándolo en
                                                                # carpetas 
                                                                # específicas, 
                                                                # corregir, 
                                                                # cambiando el
                                                                # directorio del
                                                                # código
        
        resumen_aves <- data.frame (medaves, sdaves)
        write.csv(resumen_aves, "C:/Users/tsuba/OneDrive/Escritorio/
                    3º Biología/CIENCIA DE DATOS- C2/0-MANUAL DE CLASE/
                    1-datos brutos/diversidad_usos.csv/resumen_aves.csv")
        
        # EJEMPLO: hacer un boxplot de riqueza en función del suelo A y B y 
        # guardarlo en una carpeta de resultados.
        
        ?boxplot
      
        tiff(filename = "C:/Users/tsuba/OneDrive/Escritorio/
               3º Biología/CIENCIA DE DATOS- C2/0-MANUAL DE CLASE/
               1-datos brutos/diversidad_usos.csv/plot_aves.tiff",
             width= 700,height = 100, units="mm", res = 300 )
        aves <-  boxplot(riqueza ~ uso, data= datos1, xlab = "Usos del suelo",
                         ylab = "riqueza de aves", col = "indianred2")
        
        
        

# BLOQUE 3: Trabajo reproducible y control de versiones -------------------

# En este apartado hablaremos sobre flujo de trabajo, tipos de datos, github y Rmarkdown, centrándonos especialmente en los archivos rmd.
 

# 3.1. Obtención y tratamiento de datos: flujo de trabajo y tipos  --------

# A la hora de trabajar debemos seguir siempre un flujo de trabajo donde:
        
   # 1. Partimos de una pregunta o problema
        
   # 2. Plantearnos que datos necesitamos usar y donde los podemos conseguir
        
   # 3. Trabajar con los datos donde:limpieza, análisis y visualización
        
   # 4. Mostrar los resultados
        
        
# **¿Y qué son los datos?**
        
# No son más que una colección de: hechos, observaciones, medidas, etc.
        
        
# **Tipos de datos** 

# Existen muchas clasificaciones diferentes dependiendo de la aproximación que sigamos:
  
  #- Datos cuantitativos y cualitativos
        
  #- Datos crudos y procesados
        
  #- En función del soporte: analógico, digital, vídeo-audio, etc.
        
  #- En función de su formato: tablas, vectores, matrices, mapas, etc.
        
  #- En función de sus dimensiones:
        
        #- Vector: 1 dimensión
        
        #- Matriz: 2 dimensiones (se pueden organizar en dataframes o en tibble)
        
        #- Arreglos: varias dimensiones
        
        #- Listas
        
  #- En función de la temática: geográficos, estadísticos, científicos, financieros, meteorológicos, etc.
        
        
# Los archivos que contienen los datos pueden tener extensiones muy diferentes, por ejemplo: txt, pdf, tiff, xml, csv, etc
        
# **MIRA LA PRESENTACIÓN DE CLASE PARA ENLACES DE REPOSITORIOS Y BASES DE DATOS**       
    

# 3.2. GitHub -------------------------------------------------------------

# Es una plataforma online donde se comparte código de R entre desarrolladores,
# cualquier usuario tiene acceso a los códigos y paquetes publicados en esta plataforma,
# y puede crear sus propios repositorios. Además, github también permite establecer un control
# de versiones en los proyectos de forma que ayuda a que los trabajos sean más reproducibles.
        
# De manera que esta plataforma permite que el trabajo con R sea más colaborativo y
# reproducible, además de ayudar al desarrollo de R como lenguaje de programación.
        
        
# Git y GitHub para el control de versiones
        
# En este aspecto Git es un sistema de control de versiones  abierto que funciona como el control de cambios 
# de Word. Mientras que GitHub funciona a modo de almacén para nuestros proyectos en internet. Git y GitHub fueron
# creados en un principio para grupos de programadores pero se puede aplicar a cualquier grupo de trabajo.
        
# **¿Por qué usar un control de versiones?**
        
# Porque todos los cambios se quedan almacenados permitiéndonos volver a versiones
# anteriores del documento y recuperar cualquier elemento de una versión anterior. Además,
# también se registra quién realizó los cambios, en qué fecha y por qué, de manera que facilita
# el trabajo en grupo.
        
# Repositorios

# Los repositorios permiten almacenar documentos y proyectos en GitHub, y pueden ser tanto
# públicos (donde cualquier usuario puede acceder) o privado.

# Los repositorios se organizan en ramas o "branches", donde cada rama permite un desarrollo de trabajo diferente
# a mi entender, las ramas funcionan a modo de subcarpetas, tenemos una rama principal ("main"), donde se trabaja con el proyecto.
# Se pueden generar ramas independientes (otras carpetas), donde se este trabajando con otros proyectos distintos o con partes
# diferentes del proyecto, y posteriormente estas ramas pueden fusionarse con la principal (meter una carpeta dentro de otra).

# Las nuevas ramas se generan por medio de los "pull and request"

# Issues
        
# El apartado de issues funciona a modo de un foro, donde una persona abre un "issue" o tema para preguntar algo o para
# pedir ayuda, y las personas que tienen compartido ese repositorio pueden acceder a ese "issue" y tratar de responder a la duda
# o resolver el problema concreto.
        
        
# 3.3. Archivos rmd -------------------------------------------------------

#Los archivos rmd son un tipo de documentos con una extensión propia de
#rmarkdown que actúan como intermediarios entre un script y un output
#definitivo, por ejemplo un archivo html.
        
#En un archivo rmd tenemos dos elementos principales, código ejecutable o
#chunk, el cual se engloba en una especie de recuadro ejecutable; y un
#elemento de texto que no es ejecutable y que funciona de la misma manera
#que un procesador de texto. Al inicio del archivo rmd existe un cuadro
#de opciones (que se conoce como Yaml, Yaml header o preámbulo; y se
#escribe en lenguaje Yaml), donde se pueden definir aspectos del output,
#como por ejemplo el tipo de documento que se generará (pdf, html, word,
#etc) e incluso se pueden definir parámetros estéticos, como el tipo y
#color de letra en temas personalizados.
        
#Cuando un archivo rmd se "teje" o "knit" (es un compilador, es decir, compila una serie de instrucciones y las traduce a otro lenguaje como puede ser html) se genera el output final definido en el Yaml. En el output se
#ejecutan todos los chunks (código) y el texto adquiere un formato
#concreto que se define en el documento rmd, donde, por ejemplo, el
#número almohadillas (#) al inicio del texto define el tamaño de los
#títulos.
          
#En este bloque hemos estado trabajando directamente sobre un archivo
#rmd.
          
#Con rmarkdown también se pueden crear presentaciones powerpoint y
#archivos shiny (que se caracterizan por ser documentos dinámicos e
#interactivos).
          
# **Sobre Rmarkdown**
            
#Rmarkdown es un lenguaje de marcado (al igual que lo es html, no es un lenguaje de programación). Rmarkdown carece de funciones aritméticas.
#Un lenguaje de marcado es un lenguaje con marcas y etiquetas que sirven de dictado para que un software siga unas determinadas especificaciones.
          
       
# BLOQUE 4.Obtención y tratamiento aplicado a la ciencia de datos ----------------

#A lo largo de este bloque aprenderemos a manejar los datos ya visualizarlos por
#medio de diferentes tipos de gráficos. En este bloque trabajaremos mayoritariamente
#con el paquete `ggplot`de `tidyverse`.
        
        
# 4.1. Tiddyverse ---------------------------------------------------------


## 4.1.1. ¿Qué es tidyverse? -----------------------------------------------

# `tidyverse` es una colección de paquetes de R que engloba un total de 8 paquetes con funciones diferentes:
# - readr: que se utiliza para leer datos
# - dplyr: para la manipulación de datos
# - tibble
# - tidyr
# - purrr
# - stringr
# - forcats
# - ggplot2: para la visualización de datos

# Con `tidyverse` se pueden realizar todas las funciones de `rbase` (que es el conjunto de
# funciones que vienen preinstaladas al descargar R), pero de forma más intuitiva
# y simplificada. La gran ventaja del uso de `tidyverse`, aparte de que simplifica
# enormemente el código, es que todos los paquetes que contiene y todas las funciones
# de dichos paquetes tienen una sintaxis similar (haciendo que la nomenclatura sea consistente), lo cual hace que sea mucho más intuitivo
# y sencillo de utilizar que `rbase`. 

# Por lo general la sintaxis de `tidyverse` se caracteriza por utilizar "_" en vez de "."

# Otras ventajas con respecto `rbase` son que las funciones de `tidyverse` son mucho más rápidas
# que las de `rbase`, añade automáticamente los formatos de fecha y tiempo, y que muestra una
# barra de carga para mostrar el estado de carga de los datos.

# Aquí vamos a centrarnos en el trabajo con `dplyr`.
        
### 4.1.2. Resolución de un problema ambiental con tidyverse ----------------------------

# A lo largo de esta clase vamos a intentar resolver un problema ambiental, pero antes de comenzar
# vamos a cargar varias librerías:
        
library(here)
library(patchwork)
library(viridis)
library(ggpmisc)
library(tidyverse)# Al cargar este paquete nos salen conflictos, esto se debe a
                  # que funciones de un paquete están enmascaradas por funciones
                  # de otro paquete, simplemente a la hora de usar dichas funciones
                  # debemos tener en cuenta de que paquete la queremos utilizar y definirlo
        

# El problema ambiental a resolver es el siguiente:

# En el ayuntamiento de Alcalá de Henares Hay discrepancias entre los políticos
# sobre la influencia del ser humano en el cambio climático. Algunos son piensan que
# que el cambio climático no existe, otros piensan que el cambio climático si existe pero
# que no se debe a la influencia humana y otros piensan que el sr humano si influye en el cambio climático
# ya que existen innumerables evidencias científicas que lo corroboran.

# Para resolver este problema hemos necesitado los siguientes datos:
        
# -   Datos climáticos sobre Alcalá: <https://verughub.github.io/easyclimate/index.html>
          
# -   Datos sobre las emisiones de CO~2~ en España: <https://edgar.jrc.ec.europa.eu/report_2020#data_download>
        
# Para resolver este problema ambiental trabajaremos con todos y cada uno de los paquetes de tidverse.

# El planteamiento para resolverlo es el siguiente:
        
# - Vamos a representar comoa medida que aumenta el CO~2~ aumenta la temperatura,
# de esta manera el CO~2~ es la variable independiente y temperatura es la dependiente.
# De esta manera mostramos que existe el cambio climático.
        
# - Para evidenciar el efecto del ser humano en el cambio climático podemos
# ver como varían la temperatura y niveles de CO~2~ con el tiempo
# y luego disgregar los datos de CO~2~ en sectores para ver su origen.
        

#### A. Descargar los datos --------------------------------------------------

# Antes de comenzar a utilizar `tidyverse` debemos descargar los datos climáticos
# y de CO~2~ para Alcalá de Henares, para esto utilizaremos el paquete `easyclimate`:

#Primero debemos instalar el paquete
        
install.packages("easyclimate")# no podemos, porque no esta en el cran, que es donde
                               # están las librerías oficiales de R, por eso tenemos
                               # que instalarlo con remotes
        
install.packages("remotes")# como carecíamos de este paquete lo hemos tenido que instalar
 
remotes::install_github("VeruGHub/easyclimate")# así es como se instala, entre comillas
                                               # es la dirección del repositorio de
                                               # Github donde se encuentra
        
# Cargamos la librería easylimate

library(easyclimate)

# Definimos las coordenadas de la región de donde queremos descargarnos los datos 

coords <- data.frame(
          lon = -3.35,
          lat = 40.48
          )

# Se representa un mapa, aunque esto no lo hemos realizado en clase, y esto 
# simplemente nos señala en un mapa la posición de Alcalá de Henares.       
ggplot() +
  borders(regions = c("Spain", "Portugal", "France")) +
  geom_point(data = coords, aes(x = lon, y = lat)) +
  coord_fixed(xlim = c(-10, 2), ylim = c(36, 44), ratio = 1.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

# Con este código descargariamos los datos de temperatura de Alcalá de Henares 
# con el paquete easyclimate, no ejecutamos este código porque estos datos 
# de temperatura y los datos de CO2 ya los tenemos descargados.

# temp <- get_daily_climate(
#   coords,
#   period = 1950:2020, 
#   climatic_var = c("Tmin", "Tmax")
#   )
        
# save(temp, file = "00-raw/temp.RData")
# load(here("00-raw", "temp.RData"))
        

#### B. Uso de readr ---------------------------------------------------------

# Este paquete nos permite cargar datos.  (usa lo de tablespill en tres apartados)

##### Datos de CO~2~ -----
# Comenzamos cargando los datos de CO~2~

# A la hora de cargar los datos hemos visto varios errores, que hemos ido resolviendo
# sobre la marcha. 

# En primer lugar hemos trabajado con el paquete de rbase

co_data <- read.csv("intro_tidyverse-dplyr-main/01-data/co2.csv")
#  Cuando estabamos trabajando en clase no lo encontraba, porque el archivo rmd 
# en el que trabajamos estaba en otra carpeta diferente a la de datos

# Para resolverlo en clase usamos ".." que indica que salimos de la carpeta donde 
# nos encontramos con el rmd.

# Posteriormente hemos pasado a trabajar con tidyverse.

# Con tidyverse podemos utilizar el paquete here, que nos facilita mucho el trabajo
here()
co_data <- read_csv(here("intro_tidyverse-dplyr-main", "01-data", "co2.csv"))# pongo "," porque en el paquete here se especifica así, además al poner el guión bajo estoy usando la función read del paquete readr de tidyverse

##### Datos de temperatura ----

# Aquí hemos utilizado la función `read_delim` donde tu marcas el elemento que
# delimita los datos en vez de utilizar `read_csv`:

temp <-  read_delim (here("intro_tidyverse-dplyr-main", "01-data", "temalcala.csv"), delim =";")


##### ¿Qué diferencia existe entre la función `read` de `rbase` y la de `tidyverse`? ----

# La principal diferencia es que esta función en `tidyverse` es mucho más rápida
# que con `rbase`, pero al mismo tiempo es más exigente con el código utilizado
# dando errores muchas más veces pero ayudando a mejorar tu código al mismo tiempo.
# También puede dar problemas con tildes, y sale una barra de progreso de carga de los datos.       


#### C. Exploración de los datos ---------------------------------------------

# Tras cargar los datos realizamos una exploración de los mismos.
        
# Para la exploración de los datos usamos `str` y `summary`

str(co_data)
summary(co_data)
str(temp)
summary(temp)

# También  podemos realizar la exploración con `View`, esta función 
# está bien para ver los datos pero no nos muestra toda la información.

View(co_data)
View(temp)

# Podemos ver únicamente una muestra de los datos, para ello podemos usar `head`,
# donde nos permite ver  los primeros diez datos de los dataframes.

head(co_data)
head(temp)

# Finalmente también podemos utilizar `glimpse`, esta función hace lo mismo que 
# `str` pero de forma más ordenada.

glimpse(co_data)
glimpse(temp)
        

#### D. Uso del paquete Tibble -----------------------------------------------


##### Concepto de tibble------

# `tibble` es una reinterpretación  moderna de lo que es un dataframe, al igual
# que todas las funciones de `tidyverse` es más exigente con el código que `rbase`.
# Otra diferencia notable es que `tibble` presenta un formato mucho más consistente
# que un dataframe de `rbase`.

# Un dataframe se puede devolver en forma de un vector o de otro dataframe(lo cual hace que sea inconsistente)
# Mientras que un tibble siempre se devuelve en el código en forma de tibble.

# **Ejemplo:**

# Podemos guardar un columna de un dataframe en forma de un vector:
v1 <-  co_data [,1]
class(v1)# vemos que es un vector

# Posteriomente podemos pasar este vector a dataframe:

v1 <- as.dataframe (co_data [,1])
class(v1)# vemos que es un dataframe 

# **Ejemplo de la creación de un tibble**:

# Para su creación usamos la función `tibble`:

t1 <- tibble (
  x = 1,
  y = 1
  )


#### E. Uso de tidyr: datos tidy o tidy data---------------------------------------------

#(usa tablespill)
    
##### Características de los datos tipo tidy:-----

# Cualquier dato tipo tidy debe cumplir:

# 1.  Cada columna es una sola variable
# 2.  Cada fila es una observación
# 3.  Cada celda es una única medida

# Como podemos apreciar el formato tidy de los datos ayuda a simplificar y a ordenar
# nuestros datos de trabajo, es un paso esencial cuando se trabaja con grandes bases de datos.
# Este formato de los datos nos ayuda a trabajar de forma más eficiente, limpia y reproducible
# con nuestros datos.

# Así el paquete `tidyr` de `tidyverse` compila una serie de funciones que nos permiten
# organizar y limpiar nuestros datos para que adquieran un formato tidy.

##### ¿Está nuestro dataframe co_data en formato tidy? ----

# Para saberlo haremos una exploración de este datafram y usaremos la función `glimpse`:

glimpse (co_data)

# Como se puede apreciar, nuestro dataframe co_data no cumple las características para ser un dato tipo tidy,
# ya que en el encontramos variables innecesarias que se pueden simplificar,
# por ejemplo los datos de año esta dispuestos como una variable cada uno, cuando
# se podría generar una sola variable llamada año que recogiese todos los años muestreados
# y los relacionase con los valores de CO~2~.

# El formato en el que se encuentra actualmente estos datos se conoce como un
# formato ancho, donde existen muchas variables. Por ello, para que sea un dato tidy
# debemos pasarlo a un formato largo, donde existen menos variables.

# En el formato largo haremos que solo existan 3 variables:
# - una para los niveles de CO~2~ 
# - otra para los años 
# - una final para los países 

# Para conseguir pasar de un formato ancho a un formato largo debemos utilizar la
# función `pivot_longer` ( dentro de `pivot` existen principalmente dos funciones
# `pivot_longer` que pasa a un formato largo y `pivot_wider` que pasa a un formato ancho).

co_data2 <-  co_data %>%
  pivot_longer(
    cols = "1970":"2019",# señalamos el rango de columnas que vamos a cambiar
    names_to = "co_year", # a donde va a ir esas columnas (la nueva variable)
    values_to = "co_value"# a donde van los valores de esas columnas de cada año (a donde van los datos de CO2 por año)
    )


##### Inciso sobre la pipa ( %>%  o |:>)---

# La pipa  coge un resultado previo y lo pasa a la siguiente función.
# Ya existía en otros lenguajes de programación. En R apareció de repente en 2014,
# debido a que su uso facilita la lectura y escritura de código de forma considerable
# Su shortcut es: `ctrl + shift + m`


#### F. Uso de dplyr: manipulación de datos ----------------------------------

#(usa tablespill)

# Mediante el uso de `dplyr` seguiremos trabajando los datos a través de diferentes funciones.

##### Filter y select ----

# `filter` permite seleccionar filas, mientras que `select` permite seleccionar columnas.

# **Datos de CO~2~:**

# Una vez que hemos pasado los datos de CO~2~ a formato tidy vamos a visualizarlos con `glimpse`

glimpse (co_data2)

# Pero al observar los datos vmos que tenemos información de todos los países,
# a nosotros solo nos interesa España para nuestro problema ambiental, por lo que 
# usaremos `filter` para seleccionar aquellos datos que sean de España y lo guardaremos:

co_data_s <- co_data2 %>% 
  filter(country_name == "Spain and Andorra")

# **Datos de temperatura:**

# Ahora trabajaremos con los datos de temperatura, para ello primero comprobamos la estructura
# de los datos con `str`:

str(temp)

# Puede que lo de temperatura no este en formato tidy, pero lo dejamos así
# porque vamos a usar la temperatura media. La columna de id no es necesaria 
# porque todos los datos son de Alcalá de Henares, por lo que vamos a eliminarla.
# Para quitar columnas en tidyverse no se borran columnas, sino  que se seleccionan
# las  columnas que quieres utilizando la función `select`.

temp_sel <- temp %>% 
  select(!ID_coords) # podríamos hacerlo con - pero lo correcto es !, estamos seleccionando
                     # todas menos la columna del id

##### Mutate ---

# Como hemos mencionado anteriormente lo que queremos es utilizar es la temperatura media
# de Alcalá de Henares, para ello vamos a generar una nueva variable en la que se refleje
# la temperatura media, que se calculará como la suma de las temperaturas máximas y mínimas
# entre 2. Además tengo otro inconveniente con respecto a la temperatura y es que en el apartado de fecha
# me aparecen los días, meses y años, pero solo nos interesan los años, ya que agruparemos la temperatura en función de estos.
# Para realizar todos estos cambios usaremos la función `mutate`:


temp_f <- temp_sel %>% 
  mutate(
    Tmean = (Tmax+Tmin)/2, 
    date = as.Date(date),
    month = format(date, format = "%m"),
    year = format(date, format = "%Y")
  )

# En la primera parte del código estamos definiendo la nueva variable de temperatura media (`Tmean`)
# para ello, primero ponemos el nombre de la nueva variable y luego que datos o valores contiene,
# en este caso definimos quequeremos que se calcule a partri de la media entre la temperatura máxima y mínima.
# En cuanto a la fecha le indica que nos extraiga los meses y días con "%m" y "%Y", redefiniendo la variable en el proceso.

# Con la función mutate podemos generar nuevas variables desde 0, indicando todos los valores de 
# esa nueva variable.

##### Group_by y summarise ----

# A pesar de todo el trabajo que llevamos hasta ahora todavía no hemos terminado de preparar los datos de temperatura
# ya que necesitamos que los datos de temperatura media este organizados por año, ya que para un mismo año
# tenemos diferentes valores de temperatura media. Para conseguir esto utilizaremos las funciones
# `group_by` y `summarise`.

# En primer lugar agruparemos los valores de temperatura media por año utilizando la función `group_by`.
# Pero tras esta operación sequiré teniendo varios datos de temperatura media por año, 
# por ello lo que haremos será resumir estos datos haciendo la media de las temperaturas medias para cada año,
# esto lo conseguiremos utilizando la función `summarise`. Así aplicaremos el siguiente código:

temp_mean <- temp_f %>% 
  group_by(year) %>% 
  summarise(
    Tmean.year = mean(Tmean)
  )

##### Join ----

# Como para nuestro problema ambiental queremos tener los datos de CO~2~ y los de temperatura en un mismo tibble
# utilizaremos la función `join` que me permite unir las columnas varios dataframes o varios tibbles en uno solo
# por diversos métodos, donde se selecciona la variable común por la que se quieren unir. Existen principalmente cuatro tipos de `join`:

# - `full_join`: consiste en unir todas las columnas de los dataframes o tibbles en uno solo
# - `left_join`: toma todas las columnas del primer dataframe o tibble y las unirá solo con las columnas que tengan filas coincidentes del segundo dataframe o tibble
# - `right_join`: toma todas las columnas del segundo dataframe o tibble y las unirá solo con las columnas del primer dataframe o tibble que tengan filas que coincidan en ambos
# - `inner_join`: que solo unirá las columnas de los dos dataframes o tibbles que tengan filas coincidentes.

# Estos concepto se entienden mejor con la siguiente imagen:
![](images/join.jpg)
# en el rmd dale a insertar imagen

# Utilizaremos un `full_join` para unir los datos de CO~2~ y de temperatura por el año:

co_temp <- full_join(
  temp_mean, co_data_s, by = c("year" = "co_year")
  )

# En este código hemos dicho que la columna "year" de temperatura es igual que la columna
# " co_year" de los datos de CO~2~, y hemos unido las dos bases por medio de esta columna.

# Ahora que se han unido nos queda organizar un poco la información, si nos fijamos en nuestro nuevo
# tibble `co_temp` veremos que existen varios valores de temperatura repetidos por sectores donde se ha medido el nivel de CO~2~.

#lo que sucede es que la temperatura anual media en Alcalá de Henares es igual para cualquiera de los sectores
# pero no lo son los niveles de CO~2~ emitidos, por lo que para los datos de CO~2~debemos realizar la suma
# de los niveles de dióxido de carbono por sector,por ello usaremos la función `summarise`.
# Mientras que en el caso de la temperatura debemos eliminar los
# valores repetidos, por ello nos quedaremos con los primeros valores de temperatura que aparecen por año
# utilizando la función `first`. Estas funciones las aplicaremos de forma simultánea, pero hay un inconveniente
# y es que no se puede realizar la suma de los niveles de CO~2~ por sector tal cual debido a que esta variable
# está definida como un factor, por ello, antes de nada utilizaremos `mutate` para transformarla en numérica.

co_temp_all <- co_temp %>% 
  mutate(
    co_value = as.numeric(co_value),
    year = as.numeric(year)
    ) %>% # empleamos mutate  pasando co_value y year a una variable numérica
  group_by(year) %>% 
  summarise(
    co_all = sum(co_value),
    Tmean.year= first(Tmean.year)
    )# realizamos la suma de los niveles de CO2 y tomamos los primeros valores de temperatura por año



#### G. Visualización de los resultados con ggplot --------------------------

# Ahora que ya hemos terminado de organizar y depurar la información solo nos queda
# visualizar los resultados, aquí es donde entra el paquete `ggplot`, que permite producir
# diferentes tipos de gráficos de forma relativamente sencilla. Debido a que este paquete es
# anterior al resto de paquetes de `tidyverse` no funciona con la pipa ( %>% ), sino que
# emplea el símbolo "+".

# Más adelante, en el apartado de visualización de datos detallaremos más a fondo el funcionamiento y sintaxis de `ggplot`,
# por lo que en este apartado solo veremos unas nociones básicas de su funcionamiento.

# Haremos una nube de puntos sobre los datos de la temperatura media anual.

# Para ello, primero lamamos a ggplot, definimos la base de datos que vamos a usar
# y definimos cuales son la variable x e y (aes hace referencia a aesthetics y sirve para indicar
# dentro de...), luego indicamos que tipo de representación queremos realizar.

ggplot(co_temp, aes(x = year, y = Tmean.year))+ 
  geom_point()

# Al observar la nube de puntos generada vemos que algunos puntos (que son las observaciones)
# se solapan, esto se debe a que estamos utilizando la base de datos antigua donde hay datos de temperatura
# repetidos por año, simplemente debemos usar la base de datos más reciente donde habíamos realizado un `summarise`.

ggplot(co_temp_all, aes(x = year, y = Tmean.year)) +
  geom_point()


#### H. Paquete stringr ----------------------------------------------

# Este paquete de `tidyverse` contiene funciones que facilitan el trabajo con string.

# Los string son vectores compuestos por palabras. Este tipo de vectores se emplean en el
# minado de texto, donde extraemos datos de forma automatizada de documentos.

# **Ejemplo:**
co_temp_s <- co_temp %>% 
  mutate(
    other = str_detect(Sector, "Other")
    )
# Aquí hemos utilizado una función del paquete `stringr` paa que genere una nueva variable
# llamada `other` donde se determina si pertenecen a datos de un sector que lleve la 
# palabra "other" (TRUE) o no (FALSE).

##### I. Uso del paquete forcats ----------------------------------------------

# Este paquete compila una serie de funciones que facilita el trabajo con factores.
# Todas las funciones de este paquete empiezan por la partícula "fct".
# Existen funciones como `relevel` que reordena los vectores según un orden definido.

# **Ejemplo:**

co_temp_fs <- co_temp %>% 
  mutate(
    Sector = fct_relevel(Sector, "Other industrial combustion", after = Inf),
    Sector = fct_relevel(Sector, "Other sectors", after = Inf)
  )
# Aquí hemos utilizado la función `fct_relevel` para reorganizar los datos de forma que
# aquellas filas donde el sector sea Other industrial combustion  y 
# donde el sector sea  Other sectors sagan como último y penúltimo respectivamente por año.


##### J. Uso del paquete purrr ------------------------------------------------

# Con las funciones del paquete de `purrr` puedo conseguir crear funciones en bucle
# y de forma automatizada, por ejemplo podemos aplicar funciones de `purrr`, para que 
# apliquen un determinado tema a las figuras y gráficos de `ggplot` y hacer que se ejecute en bucle sobre los elementos de una lista.

# Con respecto a la sintaxis, la mayoría de funciones de `purrr` suelen contener la partícula "map"

# ** Ejemplo:**

# En rbase podríamos generar la siguiente función:
select_julen <-  function (df, x) # aquí df y x son argumentos
  {df[,x]}

# Con purrr podríamos generar la misma función y aplicarla en bucle.



## 4.2. Representación gráfica con ggplot ----------------------------------

# En este apartado nos centraremos en la generación de diferentes tipos de gráficos
# con `ggplot`

### 4.2.1. Tipos de gráficos:

# Los gráficos pueden ser:

#- histogramas: permite visualizar los datos, nos puede indicar la distribución
# de los datos, el eje de la **Y** es count (cuenta/ cantidad). **X ** es una variable continua.

# - barplot (gráfico de barras): aquí la variable **X** puede ser discreta y la variable **Y** no es un count
 
#- scatterplots (nubes de puntos): la línea de tendencia se conoce como smooth
  
#- boxplot (caja de bigotes): es un gráfico de dispersión y nos permite de saber
  # como de alejados están los datos, la linea del medio es la mediana


### 4.2.2. Sintaxis de ggplot

#### A. Librerías y datos a utilizar, observación de los datos

# Para trabajar con `ggplot`, además de necesitar el paquete de `tidyverse`, utilizaremos 
# otros paquetes que nos permitirán personalizar los gráficos. Por ello, cargamos las siguientes
# librerías:

library(tidyverse)
library(lubridate)
library(ggthemes)
library(wesanderson)

#También vamos a cargar los datos con los que vamos a trabajar, en estos datos se recoge
# información sobre los seismos y niveles de SO~2~ por región sobre la erupción de la Palma
# que ha sucedido este mismo año, en el primer dataframe. En el segundo dataframe se recoge información
# más detallada sobre los seísmos (como la profundidad o magnitud de estos), además de otra información como
# la temperatura.

count_cumbre_vieja <- read_csv("cumbre_vieja_visualdata-main/count_cumbre_vieja.csv")

data_cumbre_vieja <- read_csv("cumbre_vieja_visualdata-main/data_cumbre_vieja.csv")

# De momento trabajaremos con el primer dataframe, `count_cumbre_vieja`.

# Antes de comenzar a visualizar los datos en forma de gráficos debemos observar 
# el dataframe, para ello podemos utilizar por ejemplo, la función `View`:

View(count_cumbre_vieja)

#Vemos que tenemos cuatro variables:

# - Fecha en la que se produjeron los seísmos

# - La región de la Palma donde se registraron

# - El número de terremotos

# - Los niveles de SO~2~

# Con respecto al dataframe de `count_cumbre_vieja` podemos plantearnos ver las siguientes relaciones entre las variables, entre otras:

# - Número de terremotos ~ Niveles de SO~2~

# - Número de terremotos ~ Región

# - Número de terremotos ~ Fecha

# - Niveles de SO~2~ ~Fecha

#### B. Sintaxis de ggplot 

# `ggplot` tiene 4 elementos principales:

# - La función de `ggplot`
# - Los datos a utilizar
# - Las variables definidas, donde empleamos `aes`
# - El tipo de gráfico que queremos realizar y que sique la forma `geom_tipo`

# Un esquema general de la estructura de un gráfico `ggplot` sería el siguiente:

ggplot(datos, aes(var_x, var_y)
       geom_tipodegrafico (loquequeremos,# donde his_hitograma,point_nube de puntos
                           alpha= gradodetransparencia,
                           size= tamañodelpunto,
                           color= color,
                           fill= fondo))

# `geom_smooth`:es la línea de tendencia de una nube de puntos y puede seguir diferentes
# modelos que detallaremos más adelante 

#### C. Nubes de puntos (usa el tablespill)

##### Relación número de terremotos ~ fecha

terremoto <- ggplot(data= count_cumbre_vieja,
                    aes(x=date, y= number))+
  geom_point(color= "indianred",
             size= 5,
             alpha= 0.5)+
  geom_smooth(color= "skyblue",
              fill= "steelblue",
              method= "lm")+
  labs(x= "fecha",
       y= "Número de terremotos",
       title= "Erupción la Palma",
       subtitle = "2021")+
  theme_classic() 

# `method` permite definir el tipo de método por el que se obtiene la línea de tendencia
# el tipo de método se ponemos entre comillas. El método a usar depende de lo que queremos,
# aqui es mejor utilizar el método default (que es el método "loess"), pero por probar utilizamos
# "lm", es decir, linear model.

# `labs` indica las etiquetas, puede ser títulos y subtitulos o nombres de los ejes,
# se debe poner entre comillas ya que sino R lo considera un objeto.

# `theme` o tema permite definir el aspecto del gráfico, lo mejor es dejarlo para el final


# Probamos ahora con el método "loess"

ggplot(data= count_cumbre_vieja, 
       aes(x=date, y= number))+
  geom_line(linetype= "dashed", 
            color= "steelblue")+
  geom_point(color= "skyblue", 
             size= 5,
             alpha= 0.5)+
  labs(x= "fecha",
       y= "Número de terremotos",
       title= "Erupción la Palma",
       subtitle = "2021")+
  theme_classic() 

# Como vemos, para este caso es mejor usar el método "loess" ya que nos permite ver
# la gran variación del número de terremotos por fecha, mientras que el método "lm" no nos aporta
# nada de información. También existe un `method` que es el modelo aditivo ("gam").

# Como podemos apreciar en `ggplot` se pueden agregar figuras sobre figuras al hacer varios 
# `geom` consecutivos, esto facilita mucho la generación de gráficos y simplifica el código.

# Podemos incluso guardar un gráfico como un objeto y posteriomente añadirle más elementos, por ejemplo:

terremoto2 <- terremoto +
  geom_line(linetype= "dashed",
            color= "darkblue") 


##### Diferenciación por regiones

# Dentro de un mismo gráfico podemos incluir información de una tercera variable, en nuestro caso
# donde vemos la relación entre el número de seísmos y la fecha, podemos realizar una diferenciación
# por regiones de estos datos.

# Para conseguir esta diferenciación hay dos estrategias:

# - Generar una diferenciación por color, donde se genere un código de color que nos permita
# diferenciar los datos por regiones ( es decir, darle un color distinto a cada región)

# - Separar los datos en diferentes gráficos (donde existirá un gráfico para cada región)

# **Diferenciación por color**

# Para ello bastan con indicar en `geom_point` que queremos colorear por región:

terre_region <- ggplot(data= count_cumbre_vieja,
                       aes(x=date, y= number))+
  geom_point(aes(color= Region), 
             size= 2,
             alpha= 0.5)+
  labs(x= "fecha", 
       y= "Número de terremotos",
       title= "Erupción la Palma", 
       subtitle = "2021")+
  theme_classic()

# Podemos también generar la línas de tendencia por regiones y personalizar más el gráfico.

terre_region2 <- ggplot(data= count_cumbre_vieja,
                        aes(x=date, y= number))+
  geom_point(aes(color= Region), 
             size= 2,
             alpha= 0.5)+
  geom_smooth(aes(color= Region,
                  fill= Region)
  )+ 
  labs(x= "fecha", y= "Número de terremotos",
       title= "Erupción la Palma", subtitle = "2021")+ 
  theme_classic()

# Si nos fijamos en el gráfico vemos que nos faltan las líneas de tendencia de dos regiones,
# esto se debe a que solo hay una observación para esas regiones y con un punto no se puede generar una línea

#  Si nos damos cuenta podemos observar que el color usado en el `fill` no coincide con el de 
# las líneas de tendencia, esto se debe a que la paleta de colores que usa `ggplot` usa los colores en orden,
# y al  no poder representar dos regiones los colores de esas regiones se usan en las demás.

# El color se puede definir manualmente para evitar estos pequeños errores.
# En este caso al solo tener dos regiones con línea de tendencia lo mejor es separar los datos en varias
# gráficas independientes.

# **¿Por qué usar `color` o `fill`?**: `color` se utiliza para colorear contornos, líneas y puntos
# (elementos de una dimensión), mientras que `fill` se emplea para colorear áreas


# **Generación de diferentes gráficos por regiones**:

# Para ello usamos la función `facet_wrap`:

terre_region3 <- terre_region2+
  facet_wrap(.~Region)

#Aquí no hay que poner aes, ya que aes se pone con los geom
# Podemos poner el área de tendencia de un color solo o en gris (en gris es por
# defecto y para ello bastaría con quitar el fill)

# Como consejo para cualquier estudio, cuando hagamos un scatterplot lo mejor es utilizar un linear model
# ("lm") al principio, para ver si se ajusta a un modelo lineal, si vemos que el ajuste no es bueno
# podemos probar con los demás métodos.


##### Relación número de terremotos ~ niveles de SO~2~

terre_so2 <- ggplot(data= count_cumbre_vieja,
                    aes(x=number,
                        y= so2))+
  geom_point(color= "coral1",
             size= 2, 
             alpha= 0.5)+
  geom_smooth(method="lm")+
  theme_classic()
# No se ajusta bien a un modelo lineal

##### Trabajo con data_cumbre_vieja, relación fecha ~ magnitud de terremotos

# Ahora comenzamos a trabajar con el segundo dataframe y vamos a ver la relación entre
# la fecha y la magnitud de los terremotos.

ggplot(data=data_cumbre_vieja,
       aes(x= date, 
           y= Magnitude))+
  geom_point(aes(size= Magnitude),
             alpha= 0.25)+
  geom_smooth()

# Aquí hemos representamos la relación entre la fecha de los terremotos y la magnitud de estos,
# hemos definido que le tamaño de los puntos varie en función de la magnitud de los terremotos
# para que la visualización resulte más sencilla. Aunque esta diferenciación sería más correcta
# hacerla si la magnitud no estuviese ya representada en el gráfico, porque da información
# redundante.

# Podemos realizar la diferenciación del tamaño de los puntos en función de la profundidad de los seísmos,
# donde a menor profundidad más grandes, porque mas se notan por la población.
# A este respecto nos surge un inconveniente, y es que R por si solo hará más grande los
# puntos con mayor valor de profundidad, para cambiar esto basta con invertir el valor de la profundidad
# en el código al poner un "-" delante.
# esto de cambiar el tamaño se hace mejor con variables continuas, pero con categoricas seria con colores o facets

mag_pro <- ggplot(data=data_cumbre_vieja,
                  aes(x= date, 
                      y= Magnitude))+
  geom_point(aes(size= -Depth, color= -Depth),
             alpha= 0.25)+
  geom_smooth()+
  scale_color_gradientn(colors = rev(wes_palette("Zissou1",)))+
  theme_classic()

# Hemos invertido la escala de colores para que vaya acorde con la profundidad y que refleje la intensidad con la que sentiríamos los seísmos

#### D. Boxplots (pon tablespill)

# Los boxplots se componen de una sola variable y pueden representarse varios en base
# a otra variable categórica, nos permiten ver la dispersión de los datos.

##### Magnitud

# Por defecto los boxplots se orientan según el eje y

ggplot(data=data_cumbre_vieja,
       aes(y= Magnitude))+
  geom_boxplot()

# Podemos ver que sucede si realizamos una diferenciación por regiones:

ggplot(data=data_cumbre_vieja,
       aes(y= Magnitude, x= Region))+
  geom_boxplot(color= "black",
               aes(fill= Region))+
  theme_light()+
  theme(legend.position= "none")

# Hemos quitado la leyenda porque los nombres de las regiones ya aparecen. La región de Tazacorte
# se representa con una única línea debido a que solo tiene una observación.
# Las personalizaciones del tema ( como la posición de la leyenda, por ejemplo)
# van lo último porque sino se solapan y da fallos, por ejemplo si lo de la leyenda
# no fuese lo último,  me aparecería porque forma parte del tema que hemos usado.

#### E. Barplots o gráficos de barras

# Vamos a relacionar por medio de un barplot el número de terremotos con la región.
# Si hacemos el barplot tal cual nos dará un error, diciéndonos que solo podemos 
# utilizar una variable, para solucionar este fallo basta con usar el parche 
# `stat= "identity"`. Para diferenciar las regiones colorearemos de forma diferencial
# el barplot según la región (usamos la función `fill` porque estamos coloreando un área)

ggplot(data= count_cumbre_vieja,
       aes(x= Region,
           y= number))+
  geom_bar(stat = "identity",
           aes(fill= Region))


# Para que fuese estadísticamente correcto habría que poner barras de error.

#### F. Histogramas

# Un histograma nos indica la frecuencia con la que se repite un dato y además, nos permite
# ver la distribución de datos.

# Vamos a realizar un histograma sobre la magnitud de los terremotos.

his_1 <- ggplot(data= data_cumbre_vieja,
                aes(x = Magnitude))+
  geom_histogram(fill = "forestgreen",
                 bins = 200,
                 binwidth = 0.1)+
  theme_clean()+
  labs(y = "Frecuencia", 
       x = "Magnitud", 
       title = "Histograma 1")
his_1

# `bins` es el numero de barras del gráfico y `binwidth` permite ajustar el grosor de las barras

##### G. Diagramas de densidad

# por último podemos realizar diagramas de densidad para ver también la distibución de los datos,
# vamos a realizar un diagrama de densidad con la magnitud. 

den_1 <- ggplot(data= data_cumbre_vieja,
                aes(x = Magnitude))+
  geom_density(fill = "#E88873", 
               color = "#E05C3E",
               alpha = 0.5)+
  theme_clean()
den_1





# BLOQUE 5: visualización de datos, GIS y mapas ---------------------------

# A lo largo de este bloque trabajaremos en la creación de mapas y en el uso de 
# R como GIS


## 5.1. R como GIS -------------------------------------------------------


### 5.1.1. ¿Qué son los sistemas GIS o SIG? ---------------------------------

# Los sistemas GIS o SIG son sistemas de información geográfica, es decir,
# son programas informáticos que nos permiten trabajar con datos geográficos
# y tienen muchísimas utilidades en una gran variedad de campos.
# Estos integran información digital la analizan y la representan.
# Los sistemas GIS funcionan por capas, suelen ser mapas o atributos mapeables,
# hay distintos tipos de capas como poligonales, lineales, de puntos, etc.
# Todas las capas deben ser del mismo tipo.

# En estos sistemas podemos distinguir entre dos tipos principales de datos:

# - Datos en formato raster (fotografía)

# - Datos en formato vectorial(esta información está codificado de diferente forma)


# **¿Cuál es mejor, raster o vectorial?**

# Al trabajar con raster vamos a tener problemas en función de la resolución de la imagen por
# lo que los datos de tipo vectorial son más fieles a la realidad. Por lo tanto, a este respecto
# es mejor utilizar datos de tipo vectorial, pero hay ocasiones donde nos es indiferente cuál usar.


# **Tipos de archivos que usan los datos vectoriales**

# El tipo de archivo que más se usan en capas vectoriales son los archivos shapefile (shp),
# estos se componen de cinco archivos:
#- Tabla df: se trata de una tabla asociada a los atributos que quiero representar con la capa vectorial
#- prj: la proyección geográfica
#- shp: listas anidadas
#- xml: metadatos
#- shx: escala rotación

# **Tipos de archivos que usan los datos raster**

# Como hemos mencionado anteriormente los archivos tipo raster son imágenes y en consecuencia trabajamos con
# archivos de extensiones tiff, jpeg, etc. Hay otros tipos de archivos raster que se denominan
# **net cdf**, y se emplean para datos climáticos.

# Existen diferentes tipos de programas que funcionan como GIS, tenemos por ejemplo, QGIS, ArcGIS o esri.

# **Proyecciones y sistemas de coordenadas**

# Todos estos programas se basan en sistemas de coordenadas y proyecciones geográficas.

# Una proyección geográfica consiste en transferir la esfera de la tierra a un plano.
# Hay diferentes tipos de proyecciones geográficas:

# - Cilíndrica, que es la proyección más habitual
# - Accital, que va desde los polos polos
# - Cónica, esta es útil cuando trabajamos a nivel local.

# Con respecto a los sistemas de coordenadas geográficas, estos se componen de
# latitud (norte-sur) y longitud (este-oeste), y todos estos van de - 180º a +180º.
# Existen muchos sistemas diferentes, uno de los más usados es el sistema UTM.
# En el sistema UTM se divide la superficie terrestre en cuadrículas de un determinado tamaño
#  que siempre son iguales, se utiliza este sistema cuando área tiene importancia.


### 5.1.2. Utilización de R como GIS ----------------- 

#(usa tablespill)

# En este apartado veremos un ejemplo de como podemos utilizar R como GIS.

#### A. Preparación del entorno de trabajo -----

# En primer lugar borramos el ambiente de trabajo
rm(list=ls()) 

# Definimos el directorio de trabajo

setwd("C:/Users/tsuba/OneDrive/Escritorio/MANUAL")

# Y a continuación cargamos las librerías que vamos a utilizar 

library(raster)
library(maptools)
library(sp)
library(sf)
library(rgdal)
library(dismo)
library(spam)
library(rworldmap)
library(rworldxtra)
library(jsonlite)
library(rgeos)
library(mapSpain)
library(ggmap)
library(ggplot2)
library(maps)
library(mapdata)
library(marmap)
library(mapproj)
library(slippymath)
library(ggspatial)

#### B. Obtención de los datos de países para generar mapas ----

# Utilizando el paquete `mapSpain` vamos a descargarnos datos geográficos del mundo en alta resolución, 
# para poder representar después un mapa, sobre el cual iremos trabajando.

mapamundo <- getMap(resolution="high") 

# A continuación vemos los datos que tenemos usando `head`

head(mapamundo) 

# Y lo visualizamos usando `plot`

plot(mapamundo)

# Como observamos en la imagen se trata de un mapa de todo el mundo, pero a nosotros nos interesa
# únicamente España, puesto que a los datos descargados podemos acceder como si se tratase de una tabla
# para quedarnos con España, basta con seleccionar la fila en la que se encuentre el valor de España:

mapaspain <- mapamundo["Spain",]

# Podemos incluso seleccionar varios países, por ejemplo, vamos a seleccionar
# Italia y Francia
mapaitaliafrancia <- mapamundo[c("Italy", "France"),]
plot(mapaitaliafrancia)

# Visualizamos el mapa de España:
plot(mapaspain)

# Como podemos apreciar en la imagen además de la península, nos muestra las Islas Baleares y Canarias
# Vamos a recortar el mapa para descartar las Islas Canarias, y quedarnos únicamente con la península
# y las islas baleares. Para esto vamos a utilizar la función `crop`, en donde definiremos
# un `extent` con las coordenadas del área con el que te quieres quedar (un `extent` no es más
# que un rectángulo donde definimos las latitudes (valores de la **x**) y longitudes (valores de la **y**) mínimas y máximas, en este mismo orden, que definen
# el área del mapa que e interesa)

mapaspain <- crop(mapaspain,
                  extent(-10,
                         5,
                         35,
                         45)) 

# Volvemos a visualizarlo:
plot(mapaspain)

# Al igual que con cualquier otro plot podemos personalizar nuestro mapa, cambiando por ejemplo
# el color del polígono y su contorno.

plot(mapaspain,
     col="lightblue",
     border="white")


#### C. Otras formas de visualizar mapas con R ---- (Usa tablespill)

##### Utilizando el paquete maps, aproximaciones simples

# Por medio del paquete `maps` podemos generar mapas algo más completos que los anteriores
# de forma relativamente sencilla.

# **Mapa sin fondo**

# Comenzamos abriendo una ventana para dibujar el mapa usando la función `mapDevice`,
# a continuación usamos la función `map`y definimos varios elementos:
# - Definimos la zona del mundo de donde queremos que nos descargue el mapa, en nuestro caso descargaremos un mapa del mundo (escogemos la base de datos de donde se descargarán los datos geográficos)
# - Definimos el color de relleno del polígono
# - Definimos las coordenadas que queremos que se representen
# - Escogemos la  proyección (en nuestro caso escogemos la proyección albers que es una proycción en metros), la resolución y el color del borde del polígono y su grosor

mapDevice() 
map("world",
    fill=TRUE,
    col="lightblue", 
    boundary=F,
    interior=F,
    ylim=c(-60, 65),
    mar=c(0,0,0,0),
    projection='albers',
    par=c(0,0),
    wrap=T,
    resolution=1,
    border="aliceblue",
    myborder=0)

# **Mapa con fondo**

# Podemos añadir un fondo por medio de la función `bg`, realizaremos un mapa de 
# Europa y el Norte de África con fondo azul
map("world", 
    fill=TRUE,
    col="#91849A",
    bg="#454973",
    boundary=F,
    interior=F,
    ylim=c(30, 65),
    xlim=c(-5,25), 
    mar=c(0,0,0,0),
    projection='albers',
    par=c(0,0),
    wrap=T,
    resolution=0,
    border="#D9BBF9",
    myborder=0)

##### Utilizando ggplot

# Podemos utilizar el paquete `maps` y `ggplot` de forma conjunta para generar mapas más personalizados

# Primero debemos generar un capa donde definamos los polígonos que queremos representar

mapWorld <- borders("world",
                    colour="#999DC2",
                    fill="#575D90",
                    ylim=c(-60,60),
                    xlim=c(-180,180))

# A continuación sobreponemos al mapa anterior una capa de ggplot

mp <- ggplot() +   mapWorld

# Personalizamos el tema de ggplot

mp + theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())


##### Utilizando el paquete rworldmap

# Primero debemos obtener un mapa, para ello usamos `getMap`

trymap<-getMap() 

# En este mapa podemos generar "burbujas" a las que podemos asociar los datos que queramos

mapBubbles(dF=trymap,
           colourPalette="rainbow",
           oceanCol="aliceblue",
           landCol="#999DC2",
           borderCol="#999DC2",
           ylim=c(-49.5,70),
           lwd=0.1,
           add = T)

# A continuación añadimos líneas de contorno

maps::map("worldHires",
          add=T,
          col="#BFC1D9",
          lwd=1.5)


#### C. Uso de shapefiles ----

# Además de obtener los mapas desde R, podemos obtenerlos directamente desde páginas web
# y cargar los shapefiles en R.

# Hemos ido a la siguiente dirección web <http://www.naturalearthdata.com/>, y desde
# donde descargamos dos shapefiles:

# - En el apartado de  cultural descargamos el archivo countries de admin0 en escala media

# - En el apartado de physical descargamos el archivo land


# Comenzamos cargado los dos shapefiles gracias a la función `st_read`

land <- st_read("DataScienceUAH-main/ne_50m_land.shp") 

boundars <- st_read("DataScienceUAH-main/ne_50m_admin_0_countries.shp")

# Representamos gráficamente land

plot(land)

install.packages("raster")

install.packages("rgal")

install.packages("marmap")

# Al ejecutar este código observamos que se generan varios mapas diferentes en
# función de los atributos del shapefile que hemos descargado

# De todos los datos del shapefile solo nos interesa la geometría, que es lo que vamos a dibujar.
# Además ajustaremos las coordenadas para dibujar únicamente Europa.

plot(land$geometry,
     col="grey",
     lty=0,ylim=c(30,60),
     xlim=c(-5,35))


# También podemos obtener el mismo mapa pro con las fronteras de los países al utilizar `boundars`

plot(boundars$geometry,
     col="grey",
     border="white",
     ylim=c(30,60),
     xlim=c(-5,35))

#### D. Batimetría, utilización de marmap ----

# A estos mapas podemos añadir la batimetría (profundidad del fondo marino),
# para esto tenemos el paquete `marmap`


# Con el siguiente comando generamos una capa raster para la batimetría 
# y escogiendo la región de la que queremos la batimetría por medio de un `extent`

BATHYMET<-getNOAA.bathy(
  lon1= 70,
  lon2= -45,
  lat1= 75,
  lat2= 20,
  resolution=8)

# **Añadir la batimetría a un mapa**

# Lo primero que debemos hacer es generar una paleta de colores que marque la profundidad del agua,
# nosotros hemos generado la siguiente paleta:

blues <- c( "#21295C",
            "#1B3B6F",
            "#065A82",
            "#1C7293")

# A continuación representamos la batimetría
plot(BATHYMET,
     image = TRUE,
     land = TRUE,
     lwd = 0.05,
     lty=0,
     ylim=c(25,70),
     xlim=c(-8,35),
     bpal = list(c(0, max(BATHYMET), "#1C7293"),
                 c(min(BATHYMET),0,blues)), add=F)

# Aquí `bpal` específica como se aplica la escala de colores al mapa

# Por encima de la batimetría dibujamos la tierra

plot(boundars$geometry,
     col="#1C7C54",
     border="#73E2A7",
     ylim=c(30,65),
     xlim=c(-8,35),
     add=T)



#### E. Relieves, utilización de hillshade ----

# Por medio del paquete `hillshade` vamos a añadir el relieve

# Vamos a utilizar el mapa que habíamos obtenido al principio

mapaspain

# En primer lugar vamos a obtener los datos del relieve con `getData`, de forma que obtendremos
# los datos de altura de España, al poner `mask = T` conseguimos que nos de solo los datos de España 
# y que no nos de datos de los países circundantes. Posteriormente dibujamos el relieve.

altSpain <- getData('alt',
                    country='ESP',
                    mask=T)
plot(altSpain)

str (altSpain)

# Al ver la estructura de los datos comprobamos que se trata de un archivo raster que sigue el
# sistema de coordenadas crs


# Como no nos agrada la escala de colores que han utilizado vamos a cambiarla.

alt2= altSpain
res(alt2)= 0.02166666 # aquí disminuímos la resolución


# Con este código estoy definiendo un nuevo mapa donde utilizo los datos de altitud de
# `altSpain` y reduzco la resolución. Posteriormente representamos este nuevo mapa.

alt3<-resample(altSpain,
               alt2,
               method='bilinear')
plot(alt3)


# Sobre este mapa donde muestra el gradiente de alturas añadiremos la pendiente,
# el aspecto del terreno y el sombreado de las colinas:

slope <- terrain(alt3, opt='slope') # mapa de pendientes
aspect <- terrain(alt3, opt='aspect') # mapa de aspecto
hill <- hillShade(slope, aspect, 30, 250, normalize=T) # sombreado de colinas

# Finalmente cambiamos el color, podríamos añadirle color generando una nueva capa, 
# pero para que no se perdiese la forma del relieve tendríamos que darle transparencia.

plot(hill, 
     col=adjustcolor(grey(0:100/100),0.8),
     legend=FALSE,
     add=F)

lines(Spain,lwd=1.5)

# Así conseguimos el aspecto del relieve del mapa.

### 5.1.3. Ejemplos de su uso -------

# (Usa el tablespill)

#### Ejemplo 1: censo de la población española ----

# En este ejemplo vamos a trabajar con los datos del censo de las poblaciones españolas

# Primero cargamos datos del censo de población del INE, que viene disponible por 
# defecto en el paquete `mapSpain`, cargaremos datos de la población mundial del año 2019.

census <- pobmun19

# A continuación vamos a descargar y guardar el código de las comunidades autónomas, usando otra
# vez el paquete `mapSpain`, y veremos su estructura con `str`.

codelist <- esp_codelist
str(codelist)

# Como podemos observar, `codelist` contiene un listado de todas las localidades de España y de todas las
# comunidades autónomas.

# Ahora unimos las dos tablas utilizando el comando `merge`

census <-
  unique(merge(census, codelist[, c("cpro", "codauto")], all.x = TRUE))


# Agregamos los valores por comunidad autónoma
census_ccaa <-
  aggregate(cbind(pob19, men, women) ~ codauto, data = census, sum)

# Calculamos porcentajes y los guardamos como nuevas variables en nuestro dataframe 
# añadimos una variable donde el porcentaje se represente con respecto a 100 en vez de con respecto
# a 1, añadiendo a la vez el símbolo % detrás del número por motivos estéticos.

# En mujeres:

census_ccaa$porc_women <- census_ccaa$women / census_ccaa$pob19
census_ccaa$porc_women_lab <-
  paste0(round(100 * census_ccaa$porc_women, 2),"%")

# En hombres:

census_ccaa$porc_men <- census_ccaa$men / census_ccaa$pob19
census_ccaa$porc_men_lab <-
  paste0(round(100 * census_ccaa$porc_men, 2), "%")

# Observamos que se nos han creado las variables correspondientes
head(census_ccaa)


# Queremos representar estos porcentajes en un mapa, para ello vamos a unir nuestro
# dataframe con un shapefile donde se representen todas las comunidades autónomas de España
# para ello seguimos usando la librería `mapSpain`:

# Obtenemos el shapefile:

CCAA_sf <- esp_get_ccaa()
plot(CCAA_sf$geometry)

# El shapefile incluye canarias de la misma manera como se representan en los atlas

# Unimos nuestro dataframe con el shapefile usando `merge`:

CCAA_sf <- merge(CCAA_sf, census_ccaa)

# Recuadramos las isla canarias para que quede más bonito:
Can <- esp_get_can_box()
plot(Can, add=T)


# La representación de este mapa también la podemos realizar con ggplot, utilizando
# `geom_sf` para representar shapefiles:

ggplot(CCAA_sf) +   
  geom_sf(aes(fill = porc_women), 
          color = "grey70",
          lwd = .3
  ) +       
  geom_sf(data = Can, color = "grey70") +   # añadimos el recuadro de Canarias
  geom_sf_label(aes(label = porc_women_lab),  # añadimos las etiquetas de porcentajes
                fill = "white", alpha = 0.5,
                size = 3,
                label.size = 0
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "Blues", rev = TRUE),  # cambiamos la escala de colores
    n.breaks = 10,
    labels = function(x) {
      sprintf("%1.1f%%", 100 * x)
    },
    guide = guide_legend(title = "Porc. women")  # añadimos la leyenda
  ) +
  theme_void() +  # utilizamos un tema para que quede más bonito
  theme(legend.position = "right") # cambiamos la posición de la leyenda


#### Ejemplo 2: censo de la población española para una provincia ----

# añadimos el porcentaje de mujeres a la tabla census:

census$porc_women <- census$women / census$pob19
census$porc_men <- census$men / census$pob19


# Obtenemos capas shapefile de municipios y provincias, vamos a representar Ávila
shape <- esp_get_munic_siane(region = "Avila", epsg = 3857)
provs <- esp_get_prov_siane(epsg = 3857)


# Representamos para ver que aspecto tiene y exploramos las variables:

plot(shape$geometry) # Mapa de Ávila por municipios

plot(provs$geometry) # Mapa de España por provincias

head(shape)
head(census)


# Unimos datos a escala de municipio con el comando `merge`:

shape_pop <- merge(shape,census,
                   by = c("cpro", "cmun"),
                   all.x = TRUE)

plot(shape_pop$geometry)

# obtenemos información raster para el fondo del mapa (tesela correspondiente al
# extent de la capa shape_pop)

tile <- esp_getTiles(shape_pop,
                     type = "IGNBase.Todo",
                     zoom = 10,
                     bbox_expand = .1)

plot(tile) # la imagen incluye las carreteras, relieves, etc de Ávila


# Dibujamos el mapa completo

# Obtenemos el extent (es decir, los limites de Ávila) geográfico con la función ext del paquete `terra`

lims <- as.double(terra::ext(tile)@ptr$vector)

# Dibujamos con ggplot

ggplot(remove_missing(shape_pop, na.rm = TRUE)) +
  layer_spatraster(tile) +
  geom_sf(aes(fill = porc_women), color = NA) +
  geom_sf(data = provs, fill = NA) +
  scale_fill_gradientn(
    colours = hcl.colors(10, "RdYlBu", alpha = .4),
    n.breaks = 8,
    labels = function(x) {
      sprintf("%1.0f%%", 100 * x)
    },
    guide = guide_legend(title = "", )
  ) +
  coord_sf(
    xlim = lims[c(1, 2)],
    ylim = lims[c(3, 4)],
    expand = FALSE
  ) +
  labs(
    title = "Proporción de mujeres en Ávila por municipio (2019)",
    caption = "Source: INE"
  ) +
  theme_void() +
  theme(
    title = element_text(face = "bold")
  )



#### Ejemplo 3: representación de cuencas hidrográficas ----

# Con el paquete `mapSpain` obtenemos las cuencas hidrográficas:

hydroland <- esp_get_hydrobasin(domain = "land")
hydrolandsea <- esp_get_hydrobasin(domain = "landsea")

plot(hydroland$geom)

ggplot(hydroland) +
  geom_sf(data = hydrolandsea, fill = "#C6E0FF", alpha = .4) +
  geom_sf(fill = "#C6E0FF", alpha = .5, color= "#42858C") +
  geom_sf_text(aes(label = rotulo),
               size = 3, check_overlap = TRUE,
               fontface = "bold",
               family = "sans"
  ) +
  coord_sf(
    xlim = c(-9.5, 4.5),
    ylim = c(35, 44)
  ) +
  theme_void()

# Así hemos representado todas las cuencas hidrográficas de España, exceptuando Canarias


#### Ejemplo 4: representación de altitud y batimetría ----

# Descargamos datos de batimetría y altimetría utilizando `mapSpain`:

hypsobath <- esp_get_hypsobath()

# Hay que corregir un error en los datos de origen y usamos `Remove`:

hypsobath <- hypsobath[!sf::st_is_empty(hypsobath), ]


# En la representación gráfica emplearemos los colores utilizados en los atlas, los cuales hemos obtenido
# a través de wikipedia en el siguiente enlace: <https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps>

# Establecemos la paleta de colores:
# Para la batimetría:

bath_tints <- colorRampPalette(
  rev(
    c(
      "#D8F2FE", "#C6ECFF", "#B9E3FF",
      "#ACDBFB", "#A1D2F7", "#96C9F0",
      "#8DC1EA", "#84B9E3", "#79B2DE",
      "#71ABD8"
    )
  )
)

# Para la altitud:

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


# Ordenamos en nuestro dataframe los niveles de altura para nos coloree de menor
# altura a mayor altura siguiendo la paleta de colores que hemos definido:

levels <- sort(unique(hypsobath$val_inf))


# Establecemos los parametros de la paleta:

br_bath <- length(levels[levels < 0])

br_terrain <- length(levels) - br_bath

pal <- c(bath_tints((br_bath)), hyps_tints((br_terrain)))


# Hacemos la representación para las islas Canarias

ggplot(hypsobath) +
  geom_sf(aes(fill = as.factor(val_inf)),
          color = NA
  ) +
  coord_sf(
    xlim = c(-18.6, -13),
    ylim = c(27, 29.5)
  ) +
  scale_fill_manual(values = pal) +
  guides(fill = guide_legend(
    title = "Elevation",
    direction = "horizontal",
    label.position = "bottom",
    title.position = "top",
    nrow = 1
  )) +
  theme(legend.position = "bottom")



# Y ahora dibujamos el mapa para la Península y Baleares 

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



### 5.1.4: extracción de datos biológicos de la distribución de las  especies a partir de GBIF ------

# Podemos querer trabajar con distribuciones de especies, una forma rápida  de obtener estos datos
# con R es a partir de GBIF.


#### ¿Qué es GBIF? ----
# GBIF es una red online donde se permite el acceso libre y abierto a información de la biodiversidad
# de países y regiones del todo el mundo para la investigación científica. Lo más interesante
# es que ahora se puede acceder a está red desde R, facilitando aún más la obtención de datos.


#### Trabajo con GBIF ----

# Vamos a extraer datos de la distribución y presencia para el lince ibérico (*Lynx pardinus*)

lynx <- gbif("Lynx", "pardinus")  # hacemos una búqueda en GBIF    
lynx

# La base de datos es muy grande, así que realizaremos un `subset` (equivale a select`de `tidyverse`) de
# los mismos donde nos queramos con las coordenadas y el país donde se encuentra y ahora
# convertimos nuestros datos de distribución del lince en un dataframe con coordenadas geográficas

# Podemos incluso especificar  que solo seleccione datos de un determinado  extent, pero en el caso del
# lince ibérico esto carecería de sentido ya que se trata de una especie endémica de la península ibérica
# y en concreto de España.

pres.lynx <- subset(lynx, select=c("country", "lat", "lon"))
head(pres.lynx)    

# Descartamos posibles errores, por eso hacemos otro `subset` planteando una condición lógica
pres.lynx <- subset(pres.lynx, pres.lynx$lat<90)


# Mapeamos la distribución del Lince sobre el mapa anterior de España (donde habíamos representado
# la altimetría y batimetría)

dev.off()
spainhypbat +
  geom_sf(data = provs, colour='white', fill = "white", alpha = .001) +
  geom_point(data=pres.lynx, aes(x = lon, y = lat), color = "#FF6B6B", alpha= 0.6) +
  coord_sf(
    xlim = c(-9.5, 4.4),
    ylim = c(35.8, 44)
  )+
  labs ( y= "Latitud", x= "Longitud")

# Como se puede observar la mayor presencia del lince se encuentra en la zona sur de la Península
# hacia sierra morena y en la región inferior de Portugal, también hay bastantes presencias entorno
# al sistema central.

# Vamos a generar otro mapa de presencias usando mapas anteriores y usamos `rbase``:`

newmap <- getMap(resolution = "high")

par(mar=c(0.5,0.5,0.5,0.5)) # ajustamos los márgenes
plot(newmap, col="lightgrey", border="white", xlim=c(-7,3), ylim=c(34,45))
points(pres.lynx$lon,pres.lynx$lat,# con esto metemos la presencia del lince en puntos
       pch=19,cex=1,col=adjustcolor("#FF6B6B",0.4))
text(0,45,substitute(italic("Lynx pardinus")),cex=2) # ajustamos la posición del título


# Vamos a crear un `SpatialPointsDataFrame` (es decir, un dataframe con coordenadas)
# a través de `coordinates`, donde tengo que definir mis valores de **x** e **y**:

coordinates(pres.lynx) <- c("lon", "lat")    
plot(pres.lynx)
pres.lynx

# Exploramos el sistema de coordenadas
projection(pres.lynx) 

# No hay ningún sistema de coordenadas, así que vamos a utilizar `CRS` para definir uno.
# Vamos a asignar una proyección geográfica basada en el datum WGS84 que suele utilizarse como referencia por defecto

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# Donde +proj= proyección long-lat, el elipsoide (ellps = WGS84) y datum es el sistema de referencia


# Asignamos el sistema de coordenadas a nuestros datos:

proj4string(pres.lynx) <- crs.geo # para añadir esta proyección `ggmap` y `ggplot` lo hacen automáticamente
                                  # pero hay fuentes de datos que son complejas donde estos no
                                  # funcionan y entonces se debe hacer manualmente como en este caso
str(pres.lynx)


## Vamos a crear  un subconjunto a partir del mapa del mundo, solo para España:

spainmap=newmap["Spain",]

plot(spainmap)

str(spainmap)

# Con @ puedo explorar lo que tiene, por ejemplo polygons son polígonos,
# proj4string es la proyección, de hecho para quedarme con solo la península en
# vez de recortar el extent como hemos hecho anteriormente puedo acceder a los polígonos
# y seleccionar únicamente el polígono de la península:

spainmap@polygons[[1]]@Polygons[[1]]

PeninSpain<-SpatialPolygons(list(
  Polygons(list(
    spainmap@polygons[[1]]@Polygons[[1]]),ID=1)),
  proj4string = CRS(projection(spainmap)))

# Podemos explorar más los polígonos:

length(spainmap@polygons[[1]]@Polygons) # hay 17 polígonos

# Podemos dibujar diferentes subpolígonos, por ejemplo el segundo:
plot(SpatialPolygons(list(
  Polygons(list(
    spainmap@polygons[[1]]@Polygons[[2]]),ID=2)
  ),
  proj4string = CRS(projection(spainmap))
  )
  )

# Si quisiéramos seleccionar varios polígonos, dentro de la lista habría que repetir
# la línea donde seleccionamos los polígonos una detrás de otra.

# Ahora vamos a representar en la península la distribución del lince:

plot(PeninSpain) # dibujamos el polígono peninsular

# Encima dibujamos el lince:

plot(pres.lynx, pch=20, col="cyan4")
plot(PeninSpain, add=T)

# Añadimos el fondo gris:

plot(PeninSpain, add=F,col="lightgrey",border="grey")
plot(pres.lynx, pch=20, col="cyan4",add=T)


# Guardardamos el shapefile de puntos:

td <- file.path(getwd(), "data") #creamos una nueva carpeta donde guardarlo

writeOGR(pres.lynx, dsn=td,# dt es la dirección fisica, en este caso para no escribir toda la dirección hacemos un directorio y lo guardamos en td 
         layer="lince", driver="ESRI Shapefile")# como un shapefile tiene muchos archivos con esto consigo generar el shapefile sin ir archivo por archivo


# Guardamos el shapefile de polígonos para ello debemos convertir el objeto a
# PeninSpain a un objeto SpatialPolygonsDataFrame:

PeninDF<-SpatialPolygonsDataFrame(PeninSpain,data=data.frame(ID=1))
writeOGR(PeninDF, dsn=td, 
         layer="Espanapenin", driver="ESRI Shapefile")


# Cargamos los shapefiles guardados:

PeninDF <- readOGR("data/Espanapenin.shp")
plot(PeninDF)

presencias.lynx <- readOGR("data/lince.shp")
plot(presencias.lynx, add=T)



#### Observar la presencia de lince en los parques nacionales españoles ----

# Primero cargamos el archivo shapefile de los parques nacionales:

pns<-readOGR("DataScienceUAH-main/data/Red_PN_LIM/Limites_PyB.shp")

plot(pns, col='red', add=T) # así no me dibuja nada y me indica que se encuentran 
                            # en diferentes escalas

# si le doy add = F se me dibujan  solo los polígonos
plot(pns, col='red', add=F)

# Comprobamos la proyección, para ver que pasa:
projection(pns) # la proyección es diferente, n lo anterior seguimos una proyección
                # WGS y en los parques nacionales tenemos una proyección UTM

# Cambiamos la proyección de los parques de UTM a WGS con `spTransform`,
# así hacemos una transformación espacial:

pns.geo<-spTransform(pns,crs.geo)

# Comprobamos la proyección:
projection(pns.geo)

# Añadimos los parques:
plot(PeninDF)
plot(presencias.lynx, add=T)
plot(pns.geo,add=T)


# Ahora vamos a limpiar un poco nuestros datos, en primer lugar nos vamos a quedar
# solo con el nombre de cada parque y vamos a eliminar el parque de las islas Baleares:


pns.geo@data$NOM_PARQUE

pnsSpain<-pns.geo[-1,1]

# Ahora vamos a superponer las dos capas y extraer información de una a partir de la otra, hacemos así una
# extracción geográfica:

pres.lynx
pns.geo

lynxinpark <- over(pns.geo,pres.lynx)# aquí saca en que parques hay linces
lynxinpark <- over(pres.lynx,pns.geo)# aquí me da el nº de observaciones de linces 
                                     # que caen en parques nacionales

# Eliminamos los NA haciendo un subset:
lynxinpark.filter <- subset(lynxinpark, !is.na(SHAPE_Leng))

# Eliminamos los nombres de los parques donde hay NA de la presencia del lince:

lynxinpark.filter$NOM_PARQUE

table(lynxinpark$NOM_PARQUE[!is.na(lynxinpark$NOM_PARQUE)])


### 5.1.5. Extracción de información apartir de raster, uso del paquete `raster` -----


# Vamos a trabajar con temperaturas, primero nos decargamos datos de las temperaturas máximas
# mundiales en el mes de enero y julio:

tempmax <- getData("worldclim", var="tmax", res=10)   

plot(tempmax[[1]],main="Temperatura máxima Enero")
plot(tempmax[[7]],main="Temperatura máxima Julio")

# Cargamos archivos raster de las temperaturas y precipitaciones del 15 de enero y del 15 de julio de 2007 en España:

temp.Spain.jan07<-raster("DataScienceUAH-main/data/temp.Spain.jan07.tif")
temp.Spain.jul07<-raster("DataScienceUAH-main/data/temp.Spain.jul07.tif")

# Mapeamos los raster cargados:
plot(temp.Spain.jan07,main="Temperatura máxima 15 Enero 2007")
plot(temp.Spain.jul07,main="Temperatura máxima 15 Julio 2007")

# Podemos cortar archivos raster con el comando `crop`. 
# Esto puede hacerse en base a un `extent` indicando coordenadas
# máximas y mínimas (xmin, xmax, ymin, ymax):

extent.spain <- extent(-9.35, 4.45, 35.95, 43.85)
temp.jan.world.cropped<-crop(tempmax[[1]],extent.spain) # desde un raster del mundo, cortamos
plot(temp.jan.world.cropped)


# extraemos valores a partir del archivo raster, vamos a dibujar los parques nacionales sobre
# el raster de temperatura:
plot(temp.Spain.jan07)
plot(pns.geo, add=T)

tempjanPN <- extract(temp.Spain.jan07,pns.geo)
names(tempjanPN) <- pns.geo$NOM_PARQUE

# Así ahora tenemos una lista de los parques con las temperaturas máximas que tuvieron el 15 de enero de 2007
# Posteriormente, por medio de un loop podríamos asignar los valores de temperatura máxima al shapefile
# de los parques.

## 5.2. Mapas de puntos con ggplot: ----------------------------------------------

# Vamos a generar un mapa de la Isla de la Palma utilizando una base de datos que recopila información de la erupción de 2021, realizaremos mapas de puntos y para ello necesitaremos las siguientes librerías:

library(tidyverse)
library(lubridate)
library(ggthemes)
library(wesanderson)
library(sf)
library (leaflet)
library (ggmap)
library(htmlwidgets)
library(RColorBrewer)

# Vamos a trabajar con la temperatura así que cargamos datos de la temperatura:
thermal_traits <- read_csv("cumbre_vieja_visualdata-main/thermal_traits_complete.csv")
str(thermal_traits)

# Al final no podemos utilizar esta base por que existe un error (creo que no tenemos los datos de longitud), así que trabajaremos con la base de datos que recopila
# información de los seísmos:
data_cumbre_vieja <- read_csv("cumbre_vieja_visualdata-main/data_cumbre_vieja.csv")

# Vamos a representar un mapa de la palma, sobre el que iremos añadiendo cosas:
mapa_1 <- ggplot(data= data_cumbre_vieja, aes(x = Longitude, y= Latitude))+
  borders("world",
          colour = "gray",
          fill = "gray")+
  geom_point(alpha = 0.35,
             size = 3)+
  theme_clean()
mapa_1

# Hemos creado un mapa del mundo, así que vamos a hacer un zoom con las  coordenadas:

mapa_zoom <- ggplot(data= data_cumbre_vieja, aes(x = Longitude, y= Latitude))+
  borders("world",
          colour = "gray",
          fill = "gray")+
  coord_sf(xlim = c(-18, -17),
           ylim = c(28, 29),
           expand = FALSE)+
  geom_point(alpha = 0.35,
             size = 3)+
  theme_clean()
mapa_zoom

# Como hacemos un zoom tan grande estamos perdiendo mucha resolución en el mapa,
# si quisieramos hacer un mapa más bonito habría que hacer un shapefile con todas
# las coordenadas de canarias, pero este mapa nos sirve para trabajar.

# Si queremos guardar una figura realizada con ggplot podemos utilizar `ggsave`,
# siguiendo el siguiente esqueleto:

ggsave ("archivoconlaextension.jpg", width = 16, height = 16, units = "cm",
        dpi = 300)
# Donde dpi es la resolución y por defecto es 300, unidades si son grandes pues ayuda a no perder resolución con zoom

### Creación de un mapa interactivo, leaflet y ggmap ----

# Generamos una paleta de colores:

mycolors <- c(2,3,4,5,6)# paleta de colores personal donde se han
                        # seleccionado los colores con la posición
mypalette <- colorBin(palette="YlOrBr", 
                      domain=data_cumbre_vieja$Magnitude, 
                      na.color="transparent", 
                      bins= mycolors) # seleccionamos los colores de la paleta YlOrBr
                                      # según su posición con el vector mycolors

# domain me permite indicar que quiero colorear con la paleta

# Creamos un texto para que salga con el html:
mytext <- paste(
  "Date: ", data_cumbre_vieja$date,"<br/>", 
  "Magnitude: ", data_cumbre_vieja$Magnitude, "<br/>", 
  "Depth: ", data_cumbre_vieja$Depth, 
  sep="") %>%
  lapply(htmltools::HTML)
# paste pega texto y con  las líneas de debajo le indico que con cada dato se pegue 
# la información correspondiente y con lapply lo hacemos interactivo


# Ahora definimos el mapa:

lapalma_earthquakes_maps <- leaflet(data = data_cumbre_vieja) %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, # ~ es en función de...
                   ~Latitude,
                   stroke = FALSE,
                   fillColor = ~mypalette(Magnitude), 
                   fillOpacity = 0.8, 
                   color = ~mypalette(Magnitude), 
                   popup = ~Magnitude, # que nos salga el texto sobre la imagen
                   label = mytext,
                   labelOptions = labelOptions( 
                     style = list("font-weight" = "normal", padding = "3px 8px"), 
                     textsize = "13px", 
                     direction = "auto"))

# Con estas líneas podemos añadir una imagen raster de satélite para darle
# un aspecto más realista a nuestro mapa:

addProviderTiles('Esri.WorldImagery') %>%
  addLegend(pal=mypalette, values=~Magnitude, opacity=0.9, title = "Magnitude", position = "bottomleft")
saveWidget(lapalma_earthquakes_maps,
           file=paste0(getwd(),"/lapalma_earthquakes_maps.html"))

# Aplicamos todo a la vez:

lapalma_earthquakes_maps <- leaflet(data = data_cumbre_vieja) %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, #~ es en función de...
                   ~Latitude,
                   stroke = FALSE,
                   fillColor = ~mypalette(Magnitude), 
                   fillOpacity = 0.8, 
                   color = ~mypalette(Magnitude), 
                   popup = ~Magnitude,#que nos salga el texto sobre la imagen
                   label = mytext,
                   labelOptions = labelOptions( 
                     style = list("font-weight" = "normal", padding = "3px 8px"), 
                     textsize = "13px", 
                     direction = "auto")) %>%
  addProviderTiles('Esri.WorldImagery') %>%
  addLegend(pal=mypalette, values=~Magnitude, opacity=0.9, title = "Magnitude", position = "bottomleft")

# Guardamos nuestro mapa como html:

saveWidget(lapalma_earthquakes_maps,
           file=paste0(getwd(),"/lapalma_earthquakes_maps.html"))


# ¿Cómo dejar un mapa interactivo accesible en GitHub?

# 1.Debemos guardar el mapa como "index.html"

# 2.Subirlo a nuestro repositorio GitHub

# 3.Acceder a "settings", donde pincharemos sobre "pages" y establecemos un url
# para nuestro mapa a partir de la rama principal del repositorio.

# **¿Y si tenemos varios mapas?**
  
# Puesto que el nombre de nuestro archivo  siempre va a ser "index.html" tenemos dos opciones:
  
# - Generar un repositorio diferente para cada mapa (lo cual es un poco contraproducente)

# - Generar capas, lo que hacemos es añadir en el comando de `leaflet` con `add` los
# mapas como si fueran capas y configurar la leyenda para que nos salgan pestañas en
# las que podamos seleccionar que mapa queremos ver










