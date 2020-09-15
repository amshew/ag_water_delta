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

IDW<-function(year, var_type){
  
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/") 
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
Well_Data/Result_raster/

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
ab<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Spring2019.tif " )
plot(ab)

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
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/") 
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
  2010:2019,
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




