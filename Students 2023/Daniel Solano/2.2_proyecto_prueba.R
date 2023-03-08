####1. Limpieza del environment####
rm(list=ls())

####2. Cargar datos####

datos1 <- read.csv(file="~/Temporada 2022-2023/Asignaturas/2o cuatrimestre/Ciencia de datos/script/data/dataAurelio.csv",
         sep=";")



####3. Ver datos####

View(datos1)
summary(datos1)

plot(jitter(datos1$x.coord.1m[1:15000]), jitter(datos1$y.coord.1m[1:15000]),
     col=adjustcolor(1,0.5))
  dev.off()
  
####4. Jugar con ellos####
datos.clean <- subset(datos1, select=c("x.coord.1m", "y.coord.1m", "Species", "Sex", "day", "month", "year", "visit"))
  summary(datos2)
datos.clean.agg <-
  aggregate(cbind(visit) ~   x.coord.1m +y.coord.1m+ Species +Sex +year +month, data = datos.clean, sum)
write.csv(datos.clean.agg, "~/Temporada 2022-2023/Asignaturas/2o cuatrimestre/Ciencia de datos/script/data/data.clean.agg.csv")
datos.clean.agg <-
read.csv("~/Temporada 2022-2023/Asignaturas/2o cuatrimestre/Ciencia de datos/script/data/data.clean.agg.csv")


datos.clean.agg.y11 <- subset(datos.clean.agg, year == 11)

datos.clean.agg.y11.m1 <- subset(datos.clean.agg.y11, month == 1)

#plot(datos.clean.agg.y11.m1$x.coord.1m, datos.clean.agg.y11.m1$y.coord.1m, col="white")
points(datos.clean.agg.y11.m1$x.coord.1m,datos.clean.agg.y11.m1$y.coord.1m,
       pch=19,cex=1,col=adjustcolor("black",0.15))

datos.clean.agg.y11.m1.AS <- subset(datos.clean.agg.y11.m1,Species == "AS")
datos.clean.agg.y11.m1.AF <- subset(datos.clean.agg.y11.m1,Species == "AF")
datos.clean.agg.y11.m1.CG <- subset(datos.clean.agg.y11.m1,Species == "CG")


datos.clean.agg.y11.m1.AS.F <- subset(datos.clean.agg.y11.m1.AS, Sex == "F")
datos.clean.agg.y11.m1.AS.M <- subset(datos.clean.agg.y11.m1.AS, Sex == "M")
datos.clean.agg.y11.m1.AF.F <- subset(datos.clean.agg.y11.m1.AF, Sex == "F")
datos.clean.agg.y11.m1.AF.M <- subset(datos.clean.agg.y11.m1.AF, Sex == "M")
datos.clean.agg.y11.m1.CG.F <- subset(datos.clean.agg.y11.m1.CG, Sex == "F")
datos.clean.agg.y11.m1.CG.M <- subset(datos.clean.agg.y11.m1.CG, Sex == "M")

points(datos.clean.agg.y11.m1.AF$x.coord.1m,datos.clean.agg.y11.m1.AF$y.coord.1m,
       pch=19,cex=1,col=adjustcolor("red"))
points(datos.clean.agg.y11.m1.AS$x.coord.1m,datos.clean.agg.y11.m1.AS$y.coord.1m,
       pch=19,cex=1,col=adjustcolor("black"))

rangoAS.ch <- chull(datos.clean.agg.y11.m1.AS[,c("x.coord.1m", "y.coord.1m")])
rangoAS.ch <- c(rangoAS.ch, rangoAS.ch[1])
polygon(datos.clean.agg.y11.m1.AS[rangoAS.ch, c("x.coord.1m", "y.coord.1m")])

rangoAF.ch <- chull(datos.clean.agg.y11.m1.AF[,c("x.coord.1m", "y.coord.1m")])
rangoAF.ch <- c(rangoAF.ch, rangoAF.ch[1])
polygon(datos.clean.agg.y11.m1.AF[rangoAF.ch, c("x.coord.1m", "y.coord.1m")])

coords.CH.AS <- as.data.frame(datos.clean.agg.y11.m1.AS[rangoAS.ch,c("x.coord.1m", "y.coord.1m")])
p1 = Polygon(coords.CH.AS)
ps1 = Polygons(list(p1),1)
sps1 = SpatialPolygons(list(ps1))
convex.AS <- st_as_sf(sps1, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

coords.CH.AF <- as.data.frame(datos.clean.agg.y11.m1.AF[rangoAF.ch,c("x.coord.1m", "y.coord.1m")])
p2 = Polygon(coords.CH.AF)
ps2 = Polygons(list(p2),1)
sps2 = SpatialPolygons(list(ps2))
convex.AF <- st_as_sf(sps2, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

AS_AF_intersect <- st_intersection (convex.AS, convex.AF)

plot(AS_AF_intersect,border="blue",add=T, lwd=2)

area.AS = st_area(convex.AS)
area.AF = st_area(convex.AF)
area.AS.AF = st_area(AS_AF_intersect)
area.AS.AF.total = (area.AS - area.AS.AF) + (area.AF - area.AS.AF)/ area.AF
prop.solapamiento.AS.AF = area.AS.AF/area.AS.AF.total*100
prop.solapamiento.AS = area.AS.AF/area.AS*100
prop.solapamiento.AF = area.AS.AF/area.AF*100

tabla.solapamientos.AS.AF <- array(NA, dim=c(12,7))
colnames(tabla.solapamientos.AS.AF) <- c("area.AS", "area.AF", "area.AS.AF",
                                         "area.AS.AF.total", "prop.solapamiento.AS.AF",
                                         "area.solapamiento.AS", "area.solapamiento.AF")
rownames(tabla.solapamientos.AS.AF) <- c("AS.vs.AF")

tabla.solapamientos.AS.AF [1,1] = area.AS
tabla.solapamientos.AS.AF [1,2] = area.AF
tabla.solapamientos.AS.AF [1,3] = area.AS.AF
tabla.solapamientos.AS.AF [1,4] = area.AS.AF.total
tabla.solapamientos.AS.AF [1,5] = prop.solapamiento.AS.AF
tabla.solapamientos.AS.AF [1,6] = prop.solapamiento.AS
tabla.solapamientos.AS.AF [1,7] = prop.solapamiento.AF

#for (i in 1:12(datos.clean.agg$month)) {}
#subsets = data.frame(Species1 = c("AS","AS","AF","AF","CG","CG"),
                     #Sex = c("F", "M","F", "M","F", "M"))

subsets = expand.grid(Species1 = c("AS","AF","CG"),
                      Sex1 = c("F", "M"),
                      Species2 = c("AS","AF","CG"),
                      Sex2 = c("F", "M")
                     )
str(subsets)
subsets = subsets[-c(1,8,15,22,29,36),]
subsets$Species1 = as.character(subsets$Species1)
subsets$Species2 = as.character(subsets$Species2)
subsets$Sex1 = as.character(subsets$Sex1)
subsets$Sex2 = as.character(subsets$Sex2)


nyear = 10:13
nmonth = 1:12
nsubs = nrow(subsets)

tabla.solapamientos = array(NA, dim = c(12,5,4,30))

for (i in nyear) {#i=12
  
  datos.clean.agg.i = subset (datos.clean.agg, year == i)
  year.i = which(nyear==i)
  
  
  for (j in nmonth) { #j=1
    
    datos.clean.agg.i.j = subset(datos.clean.agg.i, month == j)
    
    
    
    if(nrow(datos.clean.agg.i.j)>0){
      for (k in 1:nsubs) { #k=3
      
      print(paste(i,j,k))
      
      datos.clean.agg.i.j.k.1 = subset(datos.clean.agg.i.j, Species == subsets[k,1] & Sex == subsets[k,2])
      datos.clean.agg.i.j.k.2 = subset(datos.clean.agg.i.j, Species == subsets[k,3] & Sex == subsets[k,4])
      
      
      
      if(nrow(datos.clean.agg.i.j.k.1) > 0 & nrow(datos.clean.agg.i.j.k.2) > 0){
        
     # datos.clean.agg.i.j.k.1 <- arrange(datos.clean.agg.i.j.k.1, x.coord.1m)
      datos.clean.agg.i.j.k.1[
        order(datos.clean.agg.i.j.k.1[,"x.coord.1m"], datos.clean.agg.i.j.k.1[,"y.coord.1m"]),
      ]
      rango.k.1.ch <- chull(datos.clean.agg.i.j.k.1[,c("x.coord.1m", "y.coord.1m")])
      rango.k.1.ch <- c(rango.k.1.ch, rango.k.1.ch[1])
      
      #plot(datos.clean.agg.i.j.k.2[,"x.coord.1m"],datos.clean.agg.i.j.k.2[,"y.coord.1m"])
      #datos.clean.agg.i.j.k.2 <- arrange(datos.clean.agg.i.j.k.2, x.coord.1m)
      datos.clean.agg.i.j.k.2[
        order(datos.clean.agg.i.j.k.2[,"x.coord.1m"], datos.clean.agg.i.j.k.2[,"y.coord.1m"]),
      ]
      rango.k.2.ch <- chull(datos.clean.agg.i.j.k.2[,c("x.coord.1m", "y.coord.1m")])
      rango.k.2.ch <- c(rango.k.1.ch, rango.k.2.ch[1])
      
      #polygon(datos.clean.agg.i.j.k.1[rango.k.1.ch, c("x.coord.1m", "y.coord.1m")])
      #polygon(datos.clean.agg.i.j.k.2[rango.k.2.ch, c("x.coord.1m", "y.coord.1m")])
      
      coords.k.1 <- as.data.frame(datos.clean.agg.i.j.k.1[rango.k.1.ch,c("x.coord.1m", "y.coord.1m")])
      coords.k.1 <- subset(coords.k.1,!is.na(x.coord.1m))
      p.k.1 = Polygon(coords.k.1)
      ps.k.1 = Polygons(list(p.k.1),1)
      sps.k.1 = SpatialPolygons(list(ps.k.1))
      convex.k.1 <- st_as_sf(sps.k.1, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)
      
      coords.k.2 <- as.data.frame(datos.clean.agg.i.j.k.2[rango.k.2.ch,c("x.coord.1m", "y.coord.1m")])
      coords.k.2 <- subset(coords.k.2,!is.na(x.coord.1m))
      p.k.2 = Polygon(coords.k.2)
      ps.k.2 = Polygons(list(p.k.2),1)
      sps.k.2 = SpatialPolygons(list(ps.k.2))
      convex.k.2 <- st_as_sf(sps.k.2, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)
      
      #plot(convex.k.1)
      #plot(convex.k.2, add=T,col="red")
      
      k.1k.2_intersect <- st_intersection (convex.k.1, convex.k.2)
      
      area.k1 = st_area(convex.k.1)
      area.k2 = st_area(convex.k.2)
      
      
      tabla.solapamientos[j,1,year.i,k] = area.k1
      tabla.solapamientos[j,2,year.i,k] = area.k1.k2 = st_area(k.1k.2_intersect)
      tabla.solapamientos[j,3,year.i,k] = area.k1.k2.total = (area.k1 - area.k1.k2) + (area.k1 - area.k1.k2) + area.k1.k2
      tabla.solapamientos[j,4,year.i,k] = prop.solapamiento.k1.k2 = area.k1.k2/area.k1.k2.total*100
      tabla.solapamientos[j,5,year.i,k] = prop.solapamiento.k = area.k1.k2/area.k1*100
    
      }
    }}
  }
}








#datos2 <- subset(datos1[1:15000,], select=c("x.coord.1m", "y.coord.1m", "Species", "Sex", "day", "month", "year", "visit"))
#summary(datos2)
#
#datos2$x.y.coord.1m <- paste(datos2$x.coord.1m, datos2$y.coord.1m)
#
#head(datos2)
#
#datos3 <-
#  aggregate(cbind(visit) ~   x.coord.1m +y.coord.1m+ Species +Sex +year +month, data = datos2, sum)
#
#View(datos3)

####Mapa de UK####
mapamundo <- getMap(resolution="high") 

mapuk <- mapamundo["United Kingdom",]

plot(mapuk, col="darkgrey", border="white", xlim=c(-8,3), ylim=c(50,55))

points(datos2$x.coord.1m,datos2$grid.y.10m,
       pch=19,cex=1,col=adjustcolor("cyan4",0.15))

####Graficas####
#datos.clean.agg <- datos.clean.agg %>%
  mutate(month = as.numeric(month))
datos.clean.agg <- datos.clean.agg %>%
  mutate(year = as.factor(year))

ggplot(data = datos.clean.agg, aes(x = month, y = visit))+
  geom_point(aes(color = year))+
  geom_line(aes(color = year))+
  theme_few()
  
print(datos.clean.agg)

datos.clean.agg.plot <- datos.clean.agg |>
  group_by(month, year, Species, Sex) |>
  summarise(visitas = sum(visit))

ggplot(data = datos.clean.agg.plot, aes(x = month, y = visitas))+
  geom_point(aes(color = year))+
  geom_line(aes(color = year))+
  facet_wrap( ~ Species)+
theme_few()

ggplot(data = datos.clean.agg.plot, aes(x = month, y = visitas))+
  geom_point(aes(color = Species, shape = Sex, size = 2))+
  geom_line(aes(color = Species))+
  facet_wrap( ~ year)+
  scale_shape_manual(values = c("♀","?", "♂"))+
  theme_few()

ggsave(filename = "visitas.por.mes.png", height = 18, width = 18, units = "cm")
?theme(axis.text.x = )
