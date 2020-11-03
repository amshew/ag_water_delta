rm(list=ls()) # Caution: this clears the Environment

library(raster)
# clear and set wd ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stars, # spatiotemporal data handling
  terra, # raster data handling
  raster, # raster data handling
  sf, # vector data handling
  dplyr, # data wrangling
  stringr, # string manipulation
  lubridate, # dates handling
  data.table, # data wrangling
  tidyr, # reshape
  tidyUSDA, # download USDA NASS data
  keyring, # API key management
  FedData, # download Daymet data
  daymetr, # download Daymet data
  ggplot2, # make maps
  tmap, # make maps
  future.apply, # parallel processing
  CropScapeR, # download CDL data
  prism,# download PRISM data
  rgal,
  rspatial
)   
library(pacman)
library(cdlTools)
library(prism)
library(rgdal)
library(raster)
library(sp)

#Inverse distance weighted
library(rgdal)
library(gstat)
library(sp)
library(dismo)
year <- 2010
AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonkoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]

plot(AR.Delta)

AR.Delta_d<-aggregate(AR.Delta, dissolve = TRUE)

plot(AR.Delta)
plot(AR.Delta_d)
extent(AR.Delta)
crs(AR.Delta)
var_type="Fall"
IDW<-function(year, var_type){
  
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("Well_depth_",var_type,"_", year,".csv") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  Data <- read.csv(paste0( folder_name, file_name))
 head(Data) 
 # Data<-read.csv("Data")
 # head(Data)
  
  
  
  dsp <- SpatialPoints(Data[,2:1], proj4string=CRS("+proj=longlat +datum=NAD83"))
  dsp <- SpatialPointsDataFrame(dsp,Data)
  summary(dsp$well_depth)
  
  cuts <- c(0,60,100,140,200,280)
  blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
  pols <- list("sp.polygons", AR.Delta, fill = "lightgray")
  
  plot(AR.Delta)
  spplot(dsp, 'well_depth', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
  
  dta <- spTransform(dsp,TA)
  cata <- spTransform(AR.Delta, TA)

  v <- voronoi(dta)
  #plot(v)
  ca <- aggregate(cata)
  vca <-raster::intersect(v, ca)
  spplot(vca, 'well_depth', col.regions=rev(get_col_regions()))
  r <- raster(cata, res=1000) 
  vr <- rasterize(vca, r, 'well_depth')
  plot(vr)
  gs <- gstat(formula=well_depth~1, locations=dta)
  idw <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
  idwr <- mask(idw, vr)
  #plot(idwr)
  #show(idwr)
  #5-fold cross validation
  RMSE <- function(observed, predicted) {
    sqrt(mean((predicted - observed)^2, na.rm=TRUE))
  }
  set.seed(5132015)
  kf <- kfold(nrow(dta))
  rmse <- rep(NA, 5)
  for (k in 1:5) {
    test <- dta[kf == k, ]
    train <- dta[kf != k, ]
    gs <- gstat(formula=well_depth~1, locations=train)
    p <- predict(gs, test)
    rmse[k] <- RMSE(test$well_depth, p$var1.pred)
  }
  rmse
  
   mean(rmse)
  
  
  write.csv(rmse, paste("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_county/RMSE_",var_type, year,".csv"), row.names = T)
  
  
  writeRaster(idwr, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_county/IDW_",var_type, year, ".tif"),  overwrite = T) 
  
  
}


#--- run the above code in parallel ---#
future_lapply(
  2010:2019,
  function (x) IDW(x, "Fall")
)


future_lapply(
  2010:2019,
  function (x) IDW(x, "Spring")
)

future_lapply(
  2010:2019,
  function (x) IDW(x, "DWT")
)


#IDW at HUC 12


#HUC12
HUC12<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/WBD_HU12_USGS/WBD_HU12_USGS.shp",
                stringsAsFactors = FALSE)
crs(HUC12)
plot(HUC12)
extent(HUC12)
#change projection
HUC12 <- spTransform(HUC12, CRS("+proj=longlat +datum=NAD83 +no_defs "))

#this is better
#Iterpolation at HUC 12
library(rgeos)
AR_huc12 <- gIntersection(HUC12, AR.Delta_d, byid=TRUE)
plot(AR_huc12)
IDW<-function(year, var_type){
  
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("Well_depth_",var_type,"_", year,".csv") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  Data <- read.csv(paste0( folder_name, file_name))
  head(Data) 
  # Data<-read.csv("Data")
  # head(Data)
  
  
  
  dsp <- SpatialPoints(Data[,2:1], proj4string=CRS("+proj=longlat +datum=NAD83"))
  dsp <- SpatialPointsDataFrame(dsp,Data)
  summary(dsp$well_depth)
  
  cuts <- c(0,60,100,140,200,280)
  blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
  pols <- list("sp.polygons", AR_huc12, fill = "lightgray")
  
  plot(AR_huc12)
  spplot(dsp, 'well_depth', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
  
  dta <- spTransform(dsp,TA)
  cata <- spTransform(AR_huc12, TA)
  
  v <- voronoi(dta)
  #plot(v)
  ca <- aggregate(cata)
  vca <-raster::intersect(v, ca)
  spplot(vca, 'well_depth', col.regions=rev(get_col_regions()))
  r <- raster(cata, res=1000) 
  vr <- rasterize(vca, r, 'well_depth')
  plot(vr)
  gs <- gstat(formula=well_depth~1, locations=dta)
  idw <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
  idwr <- mask(idw, vr)
  plot(idwr)
  show(idwr)
  #5-fold cross validation
  RMSE <- function(observed, predicted) {
    sqrt(mean((predicted - observed)^2, na.rm=TRUE))
  }
  set.seed(5132015)
  kf <- kfold(nrow(dta))
  rmse <- rep(NA, 5)
  for (k in 1:5) {
    test <- dta[kf == k, ]
    train <- dta[kf != k, ]
    gs <- gstat(formula=well_depth~1, locations=train)
    p <- predict(gs, test)
    rmse[k] <- RMSE(test$well_depth, p$var1.pred)
  }
  rmse
  
  
  mean(rmse)
  
  
  write.csv(rmse, paste("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/RMSE_",var_type, year,".csv"), row.names = T)
  writeRaster(idwr, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_",var_type, year, ".tif"),  overwrite = T) 
  
  
}



future_lapply(
  2010:2019,
  function (x) IDW(x, "Fall")
)





future_lapply(
  2010:2019,
  function (x) IDW(x, "Spring")
)

future_lapply(
  2010:2019,
  function (x) IDW(x, "DWT")
)



#Kriging at county.
Krige_county<-function(year, var_type){

folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
print(folder_name)
#--- the file name of the downloaded data ---#
file_name <- paste0("Well_depth_","Fall","_",year,".csv") 
print(file_name)
Data <- read.csv(paste0( folder_name, file_name))
head(Data) 
crs(Data)
library(sp)

coordinates(Data) <- ~Longitude + Latitude
crs(Data)
proj4string(Data) <- CRS("+proj=longlat +datum=NAD83 +no_defs ")
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
library(rgdal)
projection(Data)
Data <- Data [-zerodist(Data)[,1],]
library(rgdal)
crs(AR.Delta)
aq <- spTransform(Data, TA)
crs(aq)
ca <- spTransform(AR.Delta, TA)
crs(ca)
r <- raster(ca)
res(r)<-1
g <- as(r, 'SpatialGrid')
plot(g)

library(gstat)
gs <- gstat(formula=well_depth~1, locations=aq)
v <- variogram(gs,width=20)
head(v)


plot(v)


fve <- fit.variogram(v,  vgm(85, "Exp", 15,20))
fve
plot(variogramLine(fve, 100), type='l', ylim=c(0,2000))
points(v[,2:3], pch=20, col='red')




plot(v, fve)

#Ordinary kriging
k <- gstat(formula=well_depth~1, locations=aq, model=fve)
# predicted values
kp <- predict(k, g)

## [using ordinary kriging]
spplot(kp)



# variance
ok <- brick(kp)
ok <- mask(ok, ca)
names(ok) <- c('prediction', 'variance')
plot(ok)


#cross-validation
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
library(dismo)
nfolds <- 5
k <- kfold(aq, nfolds)
 krigrmse <- rep(NA, 5)
for (i in 1:nfolds) {
  test <- aq[k!=i,]
  train <- aq[k==i,]
  
  m <- gstat(formula=well_depth~1, locations=train, model=fve)
  p2 <- predict(m, test)
  krigrmse[i] <-  RMSE(test$well_depth, p2$var1.pred)
 
}
 krigrmse
 krigrmse <- mean(krigrmse)
 
 write.csv(krigrmse, paste("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig_county/RMSE_",var_type, year,".csv"), row.names = T)
 writeRaster(ok, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig_county/krig_",var_type, year, ".tif"),  overwrite = T) 
 
}
 

#--- run the above code in parallel ---#
future_lapply(
  2010:2019,
  function (x) Krige_county(x, "Fall")
)




future_lapply(
  2010:2019,
  function (x) Krige_county(x, "Spring")
)

future_lapply(
  2010:2019,
  function (x) Krige_county(x, "DWT")
)

#Kriging at Huc_12.
Krige_huc<-function(year, var_type){
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("Well_depth_","Fall","_",year,".csv") 
  print(file_name)
  Data <- read.csv(paste0( folder_name, file_name))
  head(Data) 
  crs(Data)
  library(sp)
  
  coordinates(Data) <- ~Longitude + Latitude
  crs(Data)
  proj4string(Data) <- CRS("+proj=longlat +datum=NAD83 +no_defs ")
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
  library(rgdal)
  
  Data <- Data [-zerodist(Data)[,1],]
  library(rgdal)
  crs(AR.Delta)
  aq <- spTransform(Data, TA)
  crs(aq)
  ca <- spTransform(AR_huc12, TA)
  crs(ca)
  r <- raster(ca)
  res(r)<-1
  g <- as(r, 'SpatialGrid')
  plot(g)
  
  library(gstat)
  gs <- gstat(formula=well_depth~1, locations=aq)
  v <- variogram(gs,width=20)
  head(v)
  
  
  plot(v)
  
  
  fve <- fit.variogram(v,  vgm(85, "Exp", 15,20))
  fve
  plot(variogramLine(fve, 100), type='l', ylim=c(0,2000))
  points(v[,2:3], pch=20, col='red')
  
  
  
  
  plot(v, fve)
  
  #Ordinary kriging
  k <- gstat(formula=well_depth~1, locations=aq, model=fve)
  # predicted values
  kp <- predict(k, g)
  
  ## [using ordinary kriging]
  spplot(kp)
  
  
  
  # variance
  ok <- brick(kp)
  ok <- mask(ok, ca)
  names(ok) <- c('prediction', 'variance')
  plot(ok)
  
  
  #cross-validation
  RMSE <- function(observed, predicted) {
    sqrt(mean((predicted - observed)^2, na.rm=TRUE))
  }
  library(dismo)
  nfolds <- 5
  k <- kfold(aq, nfolds)
  krigrmse <- rep(NA, 5)
  for (i in 1:nfolds) {
    test <- aq[k!=i,]
    train <- aq[k==i,]
    
    m <- gstat(formula=well_depth~1, locations=train, model=fve)
    p2 <- predict(m, test)
    krigrmse[i] <-  RMSE(test$well_depth, p2$var1.pred)
    
  }
  krigrmse
  krigrmse <- mean(krigrmse)
  
  write.csv(krigrmse, paste("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig_huc12/RMSE_",var_type, year,".csv"), row.names = T)
  writeRaster(ok, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig_huc12/krig_",var_type, year, ".tif"),  overwrite = T) 
  
}


#--- run the above code in parallel ---#
future_lapply(
  2010:2010,
  function (x) Krige_huc(x, "Fall")
)




future_lapply(
  2010:2019,
  function (x) Krige_huc(x, "Spring")
)

future_lapply(
  2010:2019,
  function (x) Krige_huc(x, "DWT")
)


#
library(gridExtra)
par(mfrow=c(1,4))
spring2010<--raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_Spring2010.tif " )
s10<-plot(spring2010,legend = F,
          axes = FALSE,
          box = FALSE,
          main = "Spring 2010" )


spring2013<--raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_Spring2013.tif " )
s13<-plot(spring2013,legend = F,
     axes = FALSE,
     box = FALSE,
     main = "Spring 2013" )

spring2016<--raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_Spring2016.tif " )
s16<-plot(spring2016,legend = F,
          axes = FALSE,
          box = FALSE,
          main = "Spring 2016" )
spring2019<--raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_Spring2019.tif " )
s19<-plot(spring2019,legend = T,
          axes = FALSE,
          box = FALSE,
          main = "Spring 2019" )

par(mfrow=c(1,4))
Fall2010<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig_huc12/krig_Fall2010.tif " )
s10<-plot(Fall2010,legend = T,
          axes = FALSE,
          box = FALSE,
          main = "Fall 2010" )


Fall2013<--raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_Fall2013.tif " )
s13<-plot(Fall2013,legend = T,
          axes = FALSE,
          box = FALSE,
          main = "Fall 2013" )

Fall2016<--raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_Fall2016.tif " )
s16<-plot(Fall2016,legend = F,
          axes = FALSE,
          box = FALSE,
          main = "Fall 2016" )
Fall2019<--raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW_huc12/IDW_Fall2019.tif " )
s19<-plot(Fall2019,legend = T,
          axes = FALSE,
          box = FALSE,
          main = "Fall 2019" )



###interpolate and extract
ST<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/Data/Saturated Thickness/MRVA_BotAlt_Thickness_Data/MRVA_BotAlt_Thickness_Data.shp",
             stringsAsFactors = FALSE)
plot(ST)
ST <- spTransform(ST, CRS("+proj=longlat +datum=NAD83 +no_defs+units=m "))
library(rgeos)
ST_AD <- intersect(ST, AR.Delta)
plot(ST_AD)
Data<- data.frame(ST_AD@coords,ST@data[["EstThk6602"]])

colnames(Data)[3]<-"Thickness"
colnames(Data)[1]<-"longitude"
colnames(Data)[2]<-"latitude"
#Kriging at county.

  head(Data) 
  crs(Data)
  library(sp)
  
  coordinates(Data) <- ~longitude + latitude
  crs(Data)
  proj4string(Data) <- CRS("+proj=longlat +datum=NAD83 +no_defs ")
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
  library(rgdal)
  Data <- spTransform(Data, TA)
  crs(Data)
  projection(Data)
 
  
  #Data <- Data [-zerodist(Data)[,1],]
  library(rgdal)
  crs(AR.Delta)
  
  ca <- spTransform(AR.Delta, TA)
  crs(ca)
  r <- raster(ca)
  
  res(r)<-1
  g <- as(r, 'SpatialGrid')
  plot(g)
 
  library(gstat)
  gs <- gstat(formula=Thickness~1, locations=Data)
  v <- variogram(gs,width=20)
  head(v)
  
  
  plot(v)
  
  
  fve <- fit.variogram(v,  vgm(85, "Exp", 15,20))
  fve
  plot(variogramLine(fve, 100), type='l', ylim=c(0,2000))
  points(v[,2:3], pch=20, col='red')
  
  
  
  
  plot(v, fve)
  
  #Ordinary kriging
  k <- gstat(formula=Thickness~1, locations=Data, model=fve)
  # predicted values
  kp <- predict(k, g)
  
  ## [using ordinary kriging]
  spplot(kp)
  
  
  
  # variance
  ok <- brick(kp)
  ok <- mask(ok, ca)
  names(ok) <- c('prediction', 'variance')
  plot(ok)
  ok=stack(ok)
  ok=ok[[1]]
ok
  AR2010 <-raster("F:/transfer/cdl/CDL_2010_05.tif") 
  myExtent <- spTransform(AR.Delta, CRS(proj4string(AR2010)))
  projectRaster(ok, crs = projection(myExtent))
  show(myExtent)
  Data_extract<-data.frame( raster::extract(ok,myExtent, 
                                 progress = F,
                                 fun=mean,   sp=T,na.rm =T))%>%  
    subset( ., select = -c(STATEFP10,COUNTYNS10, GEOID10,NAME10,NAMELSAD10, LSAD10,
                           CLASSFP10, MTFCC10, CSAFP10, CBSAFP10, METDIVFP10,FUNCSTAT10,ALAND10,
            AWATER10,  INTPTLAT10,   INTPTLON10))
                           #cross-validation
                           RMSE <- function(observed, predicted) {
                             sqrt(mean((predicted - observed)^2, na.rm=TRUE))
                           }
                           library(dismo)
                           nfolds <- 5
                           k <- kfold(Data, nfolds)
                           krigrmse <- rep(NA, 5)
                           for (i in 1:nfolds) {
                             test <- Data[k!=i,]
                             train <- Data[k==i,]
                             
                             m <- gstat(formula=Thickness~1, locations=train, model=fve)
                             p2 <- predict(m, test)
                             krigrmse[i] <-  RMSE(test$Thickness, p2$var1.pred)
                             
                           }
                           krigrmse
                           krigrmse <- mean(krigrmse)
                           
                           krigrmse
                           
                       
  
  saveRDS(
    Data_extract, 
    file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/ST_mean_krig.rds')
  
  
  #IDW
 
  # Data<-read.csv("Data")
  # head(Data)
  Data2<- data.frame(ST_AD@coords,ST@data[["EstThk6602"]])
  colnames(Data2)[3]<-"Thickness"
  colnames(Data2)[1]<-"longitude"
  colnames(Data2)[2]<-"latitude"
  
  coordinates(Data2) <- ~longitude + latitude
  crs(Data2)
  proj4string(Data2) <- CRS("+proj=longlat +datum=NAD83 +no_defs ")
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
  library(rgdal)
  Data2 <- spTransform(Data2, TA)
  crs(Data2)
  projection(Data2)
  
  
  #Data <- Data [-zerodist(Data)[,1],]
  library(rgdal)
  crs(AR.Delta)
  dsp <- SpatialPoints(Data2[,2:1], proj4string=CRS("+proj=longlat +datum=NAD83"))
  plot(dsp)
  dsp <- Data2
  summary(dsp$Thickness)
 
  cuts <- c(12,70,100,140,200,295)
  blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
  pols <- list("sp.polygons", AR.Delta, fill = "lightgray")
  
  plot(AR.Delta)
  spplot(dsp, 'Thickness', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
  crs(dsp)
  dta <- spTransform(dsp, TA)
  cata <- spTransform(AR.Delta, TA)
  
  v <- voronoi(dta)
  #plot(v)
  ca <- aggregate(cata)
  vca <-raster::intersect(v, ca)
  spplot(vca, 'Thickness', col.regions=rev(get_col_regions()))
  r <- raster(cata, res=1000) 
  vr <- rasterize(vca, r, 'Thickness')
  plot(vr)
  gs <- gstat(formula=Thickness~1, locations=dta)
  idw <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
  idwr <- mask(idw, vr)
  plot(idwr)
  
  AR2010 <-raster("F:/transfer/cdl/CDL_2010_05.tif") 
  myExtent <- spTransform(AR.Delta, CRS(proj4string(AR2010)))
  projectRaster(ok, crs = projection(myExtent))
  show(myExtent)
  Data_extract<-data.frame( raster::extract(idw,myExtent, 
                                            progress = F,
                                            fun=mean,   sp=T,na.rm =T))%>%  
    subset( ., select = -c(STATEFP10,COUNTYNS10, GEOID10,NAME10,NAMELSAD10, LSAD10,
                           CLASSFP10, MTFCC10, CSAFP10, CBSAFP10, METDIVFP10,FUNCSTAT10,ALAND10,
                           AWATER10,  INTPTLAT10,   INTPTLON10))
  
  #show(idwr)
  #5-fold cross validation
  RMSE <- function(observed, predicted) {
    sqrt(mean((predicted - observed)^2, na.rm=TRUE))
  }
  set.seed(5132015)
  kf <- kfold(nrow(dta))
  rmse <- rep(NA, 5)
  for (k in 1:5) {
    test <- dta[kf == k, ]
    train <- dta[kf != k, ]
    gs <- gstat(formula=Thickness~1, locations=train)
    p <- predict(gs, test)
    rmse[k] <- RMSE(test$Thickness, p$var1.pred)
  }
  rmse
  
  mean(rmse)
  #write.csv(  Data_extract, file="C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data_county.csv", row.names = FALSE)
  
  saveRDS(
    Data_extract, 
    file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/ST_mean_IDW.rds')
  
  abab<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/ST_mean_IDW.rds')
  write.csv(  Data_extract, file="C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data_county.csv", row.names = FALSE)
  
  