#'########
# PASO 6: extraer datos biol�gicos p.ej. distribuci�n de especies (GBIF) ####
#'########


## Extraer datos de presencias para el lince ib�rico
# extraemos datos de gbif
lynx <- gbif("Lynx", "pardinus")
View(lynx)


# hacemos un subset de esos datos y lo convertimos en un data.frame 
# con coordenadas
pres.lynx <- subset(lynx, select=c("country", "lat", "lon"))
head(pres.lynx)


# Descartamos posibles errores:
pres.lynx <- subset(pres.lynx, pres.lynx$lat<90)

# Mapeando las ocurrencias de Lince sobre uno de los mapas primeros
par(mar=c(0.5,0.5,0.5,0.5)) ## ajustando los m�rgenes
plot(iberia, col="lightgrey", border="white", xlim=c(-10,3), ylim=c(34,45))
points(pres.lynx$lon,pres.lynx$lat,
       pch=19,cex=1,col=adjustcolor("cyan4",0.15))
text(0,45,substitute(italic("Lynx pardinus")),cex=2)

# crear SpatialPointsDataFrame
#coordinates(pres.lynx) <- c("lon", "lat")
#plot(pres.lynx, add=TRUE)
#pres.lynx

# crear un sf object
preslynx.sf <- st_as_sf(pres.lynx, coords = c("lon", "lat"), crs = 4326) 

plot(preslynx.sf, add=T)

## podeis crear una envolvente convexa (convex hull)

rangolince.ch<-chull(pres.lynx[,c("lon","lat")])

rangolince.ch<-c(rangolince.ch,rangolince.ch[1])



# la dibujamos

polygon(pres.lynx[rangolince.ch,c("lon","lat")],col=adjustcolor("red",0.3),add=T)

# crear un sf object

coords.CH.lince = as.data.frame(pres.lynx[rangolince.ch,c("lon","lat")])



p = Polygon(coords.CH.lince)

ps = Polygons(list(p),1)

sps = SpatialPolygons(list(ps))

plot(sps)




#coordinates(coords.CH.lince) <- c("lon", "lat")

convex.lince <- st_as_sf(sps, coords = c("lon", "lat"), crs = 4326) 

plot(convex.lince,add=T)





# salvar

st_write(convex.lince,"../data/convexhull_lince.shp")