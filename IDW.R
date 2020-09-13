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
  
  writeRaster(idwr, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_",var_type, year, ".tif"),  overwrite = T) 
  
  
}


#--- run the above code in parallel ---#
future_lapply(
  2010:2011,
  function (x) IDW(x, "Fall")
)

rmse<- do.call(rbind, rmse)

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
#times plot
Fall2010<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2010.tif ")
Fall2011<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2011.tif ")
Fall2012<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2012.tif ")
Fall2013<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2013.tif ")
Fall2014<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2014.tif ")
Fall2015<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2015.tif ")
Fall2016<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2016.tif ")
Fall2017<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2017.tif ")
Fall2018<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2018.tif ")
Fall2019<-raster("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/raster/IDW_Fall2019.tif ")

Fall_well<-Fall2019-Fall2010
plot.new()
values <- click(Fall_well, n=1)
timeseries <- data.frame(year = c(2010, 2019),
                         values = values[1, ])


#Kriging 


