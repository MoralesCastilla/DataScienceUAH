datos.clean.agg <-
  read.csv("~/Temporada 2022-2023/Asignaturas/2o cuatrimestre/Ciencia de datos/script/data/data.clean.agg.csv")

datos.clean.agg.y11 <- subset(datos.clean.agg, year == 11)

datos.clean.agg.y11.m1 <- subset(datos.clean.agg.y11, month == 1)

datos.clean.agg.y11.m1.AS <- subset(datos.clean.agg.y11.m1,Species == "AS")
datos.clean.agg.y11.m1.AF <- subset(datos.clean.agg.y11.m1,Species == "AF")
datos.clean.agg.y11.m1.CG <- subset(datos.clean.agg.y11.m1,Species == "CG")


datos.clean.agg.y11.m1.AS.F <- subset(datos.clean.agg.y11.m1.AS, Sex == "F")
datos.clean.agg.y11.m1.AS.M <- subset(datos.clean.agg.y11.m1.AS, Sex == "M")
datos.clean.agg.y11.m1.AF.F <- subset(datos.clean.agg.y11.m1.AF, Sex == "F")
datos.clean.agg.y11.m1.AF.M <- subset(datos.clean.agg.y11.m1.AF, Sex == "M")
datos.clean.agg.y11.m1.CG.F <- subset(datos.clean.agg.y11.m1.CG, Sex == "F")
datos.clean.agg.y11.m1.CG.M <- subset(datos.clean.agg.y11.m1.CG, Sex == "M")

datos.clean.agg.y11.m1.AS.M <- subset(datos.clean.agg.y11.m1,Species == "AS" & Sex == "M")


####1.Rango ch####

rangoAS.F.ch <- chull(datos.clean.agg.y11.m1.AS.F[,c("x.coord.1m", "y.coord.1m")])
rangoAS.F.ch <- c(rangoAS.F.ch, rangoAS.F.ch[1])

rangoAS.M.ch <- chull(datos.clean.agg.y11.m1.AS.M[,c("x.coord.1m", "y.coord.1m")])
rangoAS.M.ch <- c(rangoAS.M.ch, rangoAS.M.ch[1])

rangoAF.F.ch <- chull(datos.clean.agg.y11.m1.AF.F[,c("x.coord.1m", "y.coord.1m")])
rangoAF.F.ch <- c(rangoAF.F.ch, rangoAF.F.ch[1])

rangoAF.M.ch <- chull(datos.clean.agg.y11.m1.AF.M[,c("x.coord.1m", "y.coord.1m")])
rangoAF.M.ch <- c(rangoAF.M.ch, rangoAF.M.ch[1])

rangoCG.F.ch <- chull(datos.clean.agg.y11.m1.CG.F[,c("x.coord.1m", "y.coord.1m")])
rangoCG.F.ch <- c(rangoCG.F.ch, rangoCG.F.ch[1])

rangoCG.M.ch <- chull(datos.clean.agg.y11.m1.CG.M[,c("x.coord.1m", "y.coord.1m")])
rangoCG.M.ch <- c(rangoCG.M.ch, rangoCG.M.ch[1])

####2.Polygon####

polygon(datos.clean.agg.y11.m1.AS.F[rangoAS.F.ch, c("x.coord.1m", "y.coord.1m")])
polygon(datos.clean.agg.y11.m1.AS.M[rangoAS.M.ch, c("x.coord.1m", "y.coord.1m")])
polygon(datos.clean.agg.y11.m1.AF.F[rangoAF.F.ch, c("x.coord.1m", "y.coord.1m")])
polygon(datos.clean.agg.y11.m1.AF.M[rangoAF.M.ch, c("x.coord.1m", "y.coord.1m")])
polygon(datos.clean.agg.y11.m1.CG.F[rangoCG.F.ch, c("x.coord.1m", "y.coord.1m")])
polygon(datos.clean.agg.y11.m1.CG.M[rangoCG.M.ch, c("x.coord.1m", "y.coord.1m")])

####3.Coords####

coords.CH.AS.F <- as.data.frame(datos.clean.agg.y11.m1.AS.F[rangoAS.F.ch,c("x.coord.1m", "y.coord.1m")])
pAS.F = Polygon(coords.CH.AS.F)
psAS.F = Polygons(list(pAS.F),1)
spsAS.F = SpatialPolygons(list(psAS.F))
convex.AS.F <- st_as_sf(spsAS.F, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

coords.CH.AS.M <- as.data.frame(datos.clean.agg.y11.m1.AS.M[rangoAS.M.ch,c("x.coord.1m", "y.coord.1m")])
pAS.M = Polygon(coords.CH.AS.M)
psAS.M = Polygons(list(pAS.M),1)
spsAS.M = SpatialPolygons(list(psAS.M))
convex.AS.M <- st_as_sf(spsAS.M, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

coords.CH.AF.F <- as.data.frame(datos.clean.agg.y11.m1.AF.F[rangoAF.F.ch,c("x.coord.1m", "y.coord.1m")])
pAF.F = Polygon(coords.CH.AF.F)
psAF.F = Polygons(list(pAF.F),1)
spsAF.F = SpatialPolygons(list(psAF.F))
convex.AF.F <- st_as_sf(spsAF.F, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

coords.CH.AF.M <- as.data.frame(datos.clean.agg.y11.m1.AF.M[rangoAF.M.ch,c("x.coord.1m", "y.coord.1m")])
pAF.M = Polygon(coords.CH.AF.M)
psAF.M = Polygons(list(pAF.M),1)
spsAF.M = SpatialPolygons(list(psAF.M))
convex.AF.M <- st_as_sf(spsAF.M, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

coords.CH.CG.F <- as.data.frame(datos.clean.agg.y11.m1.CG.F[rangoCG.F.ch,c("x.coord.1m", "y.coord.1m")])
pCG.F = Polygon(coords.CH.CG.F)
psCG.F = Polygons(list(pCG.F),1)
spsCG.F = SpatialPolygons(list(psCG.F))
convex.CG.F <- st_as_sf(spsCG.F, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

coords.CH.CG.M <- as.data.frame(datos.clean.agg.y11.m1.CG.M[rangoCG.M.ch,c("x.coord.1m", "y.coord.1m")])
pCG.M = Polygon(coords.CH.CG.M)
psCG.M = Polygons(list(pCG.M),1)
spsCG.M = SpatialPolygons(list(psCG.M))
convex.CG.M <- st_as_sf(spsCG.M, coords = c("x.coord.1m", "y.coord.1m"), crs = 4326)

####4.Intersection####

AS_AF_F_intersect <- st_intersection (convex.AS.F, convex.AF.F)
AS_AF_M_intersect <- st_intersection (convex.AS.M, convex.AF.M)
AS_CG_F_intersect <- st_intersection (convex.AS.F, convex.CG.F)
AS_CG_M_intersect <- st_intersection (convex.AS.M, convex.CG.M)
AF_CG_F_intersect <- st_intersection (convex.AF.F, convex.CG.F)
AF_CG_M_intersect <- st_intersection (convex.AF.M, convex.CG.M)

####5.Area####

#####5.1.Individual####
area.AS.F = st_area(convex.AS.F)
area.AS.M = st_area(convex.AS.M)
area.AF.F = st_area(convex.AF.F)
area.AF.M = st_area(convex.AF.M)
area.CG.F = st_area(convex.CG.F)
area.CG.M = st_area(convex.CG.M)

#####5.2.A pares####
area.AS.AF.F = st_area(AS_AF_F_intersect)
area.AS.AF.F.total = (area.AS.F - area.AS.AF.F) + (area.AF.F - area.AS.AF.F)/ area.AF?

area.AS.AF.M = st_area(AS_AF_M_intersect)
#area.AS.AF.M.total = (area.AS.M - area.AS.AF.M) + (area.AF.M - area.AS.AF.M)/ 

area.AS.CG.F = st_area(AS_CG_F_intersect)
#area.AS.CG.F.total = (area.AS.F - area.AS.CG.F) + (area.CG.F - area.AS.CG.F)/ 

area.AS.CG.M = st_area(AS_CG_M_intersect)
#area.AS.CG.M.total = (area.AS.M - area.AS.CG.M) + (area.CG.M - area.AS.CG.M)/ 

area.AF.CG.F = st_area(AF_CG_F_intersect)
#area.AF.CG.F.total = (area.AF.F - area.AF.CG.F) + (area.CG.F - area.AF.CG.F)/ 


#####5.3.Proportion####
prop.solapamiento.AS.AF.F = area.AS.AF.F/area.AS.AF.F.total*100
prop.solapamiento.AS.F = area.AS.AF.F/area.AS.F*100
prop.solapamiento.AF.F = area.AS.AF.F/area.AF.F*100

prop.solapamiento.AS.AF.M = area.AS.AF.M/area.AS.AF.M.total*100
prop.solapamiento.AS.M = area.AS.AF.M/area.AS.M*100
prop.solapamiento.AF.M = area.AS.AF.M/area.AF.M*100

prop.solapamiento.AS.CG.F = area.AS.CG.F/area.AS.CG.F.total*100
prop.solapamiento.AS.F = area.AS.CG.F/area.AS.F*100
prop.solapamiento.CG.F = area.AS.CG.F/area.CG.F*100

prop.solapamiento.AS.CG.M = area.AS.CG.M/area.AS.CG.M.total*100
prop.solapamiento.AS.M = area.AS.CG.M/area.AS.M*100
prop.solapamiento.CG.M = area.AS.CG.M/area.CG.M*100

prop.solapamiento.AF.CG.F = area.AF.CG.F/area.AF.CG.F.total*100
prop.solapamiento.AF.F = area.AF.CG.F/area.AF.F*100
prop.solapamiento.CG.F = area.AF.CG.F/area.CG.F*100

prop.solapamiento.AF.CG.M = area.AF.CG.M/area.AF.CG.M.total*100
prop.solapamiento.AF.M = area.AF.CG.M/area.AF.M*100
prop.solapamiento.CG.M = area.AF.CG.M/area.CG.M*100

####6.Tabla####

tabla.solapamientos.AS.AF.F <- array(NA, dim=c(12,7))
colnames(tabla.solapamientos.AS.AF) <- c("area.AS.F", "area.AF.F", "area.AS.AF.F",
                                         "area.AS.AF.F.total", "prop.solapamiento.AS.AF.F",
                                         "prop.solapamiento.AS.F", "prop.solapamiento.AF.F")

#encotrar automatica filas repetidas