#librerias

library("units")
library("ggthemes")
library("ggplot2")
library("viridis")
library("scales")
library("RColorBrewer")


install.packages("CGPfunctions")

install.packages("RColorBrewer")

str(datos)

###CARGA DE DATOS

datos <- read.csv("emisiones_madrid_Cdatos.csv", sep = ";")
View(datos)

#cambio de unidades

for(i in 1:nrow(datos)){
  ifelse(datos[i,"atm_inventario_cont_unidades"] == "g",
         datos[i,"atm_inventario_cont_cantidad"]<- datos[i,"atm_inventario_cont_cantidad"]/1000,
         NA)
  ifelse(datos[i,"atm_inventario_cont_unidades"] == "t",
         datos[i,"atm_inventario_cont_cantidad"]<- datos[i,"atm_inventario_cont_cantidad"]*1000,
         NA)
}
datos

grupos <- datos[, "atm_inventario_grupo_cont"]
anos <- datos[, "atm_inventario_año"] 
sectores <- datos[, "atm_inventario_sector"]
gas_emision <- datos[, "atm_inventario_cont_desc"]
unidad <- datos[, "atm_inventario_cont_unidades"]
cantidad_de_emision <- datos[, "atm_inventario_cont_cantidad"]







#emision total por sectores en funcion del año(sin contar tipo de gas)
df_años_sectores_gas_cantidad <- datos[, c(1, 3, 4, 6)]
df_años_sectores_gas_cantidad

emisionTot_porGrupo <- aggregate(cantidad_de_emision ~ anos + grupos,
                                 df_años_sectores_gas_cantidad,
                                 sum)
emisionTot_porSectores <- aggregate(cantidad_de_emision ~ anos + sectores,
                                    df_años_sectores_gas_cantidad,
                                    sum)
emisionTot_porCompuesto <- aggregate(cantidad_de_emision ~ anos + gas_emision,
                                    df_años_sectores_gas_cantidad,
                                    sum)
emisionTot_porCantidad <- aggregate(cantidad_de_emision ~ sectores,
                                     df_años_sectores_gas_cantidad,
                                     sum)
emisionTot_porCantidad_gases <- aggregate(cantidad_de_emision ~ gas_emision,
                                    df_años_sectores_gas_cantidad,
                                    sum)
emisionTot_porCantidad
emisionTot_porCompuesto
emisionTot_porSectores
#grafico de lineas
summary(as.factor(emisionTot_porSectores$sectores))

gl_emisionTot_porSectores <- ggplot(emisionTot_porSectores, aes(x = anos, y = cantidad_de_emision, color = sectores)) +
  geom_line(size = 1.5)+
  geom_point(size = 5)+
  scale_y_continuous(breaks=seq(0, 900000000, 50000000))+
  scale_x_continuous(breaks = c(1990:2017))+
  labs(subtitle="Emision de gases efecto invernadero por sectores", 
       y = "Cantidad de emision (kg)", 
       x = "Años", 
       caption = "Fuente: midwest")+
  scale_fill_brewer(palette = "Set2")
 

gl_emisionTot_porSectores


gl_emisionTot_porCompuesto <- ggplot(emisionTot_porCompuesto, aes(x = anos, y = cantidad_de_emision, color = gas_emision)) +
  geom_line(size = 1.5)+
  geom_point(size = 5)+
  scale_y_continuous(breaks=seq(0, 900000000, 50000000))+
  scale_x_continuous(breaks = c(1990:2017))+
  labs(subtitle="Emision de gases efecto invernadero en funcion de los compuestos", 
       y = "Cantidad de emision (kg)", 
       x = "Años", 
       caption = "Fuente: midwest")+
  scale_color_viridis(discrete = TRUE, option = "H")+
  scale_fill_viridis(discrete = TRUE)+
  theme_economist()


gl_emisionTot_porCompuesto

gl_emisionTot_porGrupo <- ggplot(emisionTot_porGrupo, aes(x = anos, y = cantidad_de_emision, color = grupos)) +
  geom_line(size = 1.5)+
  geom_point(size = 5)+
  scale_y_continuous(breaks=seq(0, 900000000, 50000000))+
  scale_x_continuous(breaks = c(1990:2017))+
  labs(subtitle="Emision de gases efecto invernadero por grupos", 
       y = "Cantidad de emision (kg)", 
       x = "Años", 
       caption = "Fuente: midwest")+
  scale_fill_brewer(palette = "Set2")


gl_emisionTot_porGrupo

###barras
graficos_emisionTot_porCantidad <- ggplot(emisionTot_porCantidad, aes(x = sectores, y = cantidad_de_emision, fill = sectores))+
  geom_col()+
  labs(subtitle="Emision de gases total por sector", 
       y = "Cantidad de emision (kg)", 
       x = "", 
       caption = "Fuente: midwest")+
  scale_fill_brewer(palette = "Set3")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "white"))  
  
graficos_emisionTot_porCantidad

graficos_emisionTot_porCantidad_gases <- ggplot(emisionTot_porCantidad_gases , aes(x = gas_emision, y = cantidad_de_emision, fill = gas_emision))+
  geom_col()+
  labs(subtitle="Emision de gases total por gas", 
       y = "Cantidad de emision (kg)", 
       x = "", 
       caption = "Fuente: midwest")+
  scale_colour_viridis_c()+
  theme_classic()+
  theme(axis.text.x = element_text(color = "white")) 



graficos_emisionTot_porCantidad_gases


graficos_emisionTot_porGrupos <- ggplot(emisionTot_porGrupo , aes(x = grupos, y = cantidad_de_emision, fill = grupos))+
  geom_col()+
  labs(subtitle="Emision de gases total por grupo", 
       y = "Cantidad de emision (kg)", 
       x = "", 
       caption = "Fuente: midwest")+
  scale_color_viridis(discrete = TRUE, option = "E")+
  scale_fill_viridis(discrete = TRUE)+
  theme_calc()+
  theme(axis.text.x = element_text(color = "white")) 

graficos_emisionTot_porGrupos
