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
  rspatial,
  cowplot,
  grid,
  gridExtra,
  cdlTools,
  rgdal,
  raster,
  sp,
  gstat,
  dismo,
  ggspatial,
  readxl
)  


###Read in the actual data
well_reading<- read_excel( "C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Alluvial_siteWI.xlsx" ) %>%
  subset( ., select = c(Longitude,Latitude__,WL_Below_L,Measuremen,Station_Na))

colnames(well_reading)[4]<-"date"
colnames(well_reading)[2]<-"Latitude"
colnames(well_reading)[3]<-"well_depth"
well_reading$Year <-as.numeric( format(as.Date(well_reading$date), format = "%Y"))
well_reading$month <- as.numeric(format(as.Date(well_reading$date), format = "%m"))
##Create the spring and fall data
well_reading<-well_reading%>%mutate(.,season=ifelse(month>7,"Spring","Fall"))%>%subset( ., select = -c(date))%>% drop_na()
well_reading$Longitude<- as.numeric( well_reading$Longitude)
well_reading$Latitude<- as.numeric( well_reading$Latitude)
well_reading<-well_reading%>%group_by(Longitude,Latitude,Station_Na,season,Year)%>%summarise(well_depth = mean(well_depth))%>% drop_na()
saveRDS(
  well_reading, 
  file="C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/well_reading.rds")
n_occur <- data.frame(table( well_reading$Year))


Spring_data<- well_reading[well_reading$season=="Spring", ]
colnames(Spring_data)[6]<-"W_S"
Spring_data<-Spring_data[,c(1,2,6,3,4,5)]
###* Fall
library("readxl")
well_reading<- read_excel( "C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Alluvial_siteWI.xlsx" ) %>%
  subset( ., select = c(Longitude,Latitude__,WL_Below_L,Measuremen,Station_Na))

colnames(well_reading)[4]<-"date"
colnames(well_reading)[2]<-"Latitude"
colnames(well_reading)[3]<-"well_depth"
well_reading$Year <-as.numeric( format(as.Date(well_reading$date), format = "%Y"))
well_reading$month <- as.numeric(format(as.Date(well_reading$date), format = "%m"))
well_reading<-well_reading%>%mutate(.,season=ifelse(month>7,"Spring","Fall"))%>%  subset( ., select = -c(date))%>% drop_na()
well_reading<-well_reading%>%group_by(Longitude,Latitude,Station_Na,season,Year)%>%summarise(well_depth = mean(well_depth))%>% drop_na()

Fall_data<- well_reading[well_reading$season=="Fall", ]
colnames(Fall_data)[6]<-"W_F"
Fall_data<-Fall_data[,c(1,2,6,3,4,5)]

DTW<-merge(Fall_data,Spring_data,by=c("Longitude" ="Longitude","Latitude"="Latitude","Station_Na"="Station_Na", "Year"="Year"),sort = TRUE)%>%mutate(.,Diff=W_S-W_F)
colnames(DTW)[9]<-"well_depth"
DTW<-DTW[,c(1,2,9,4,5,7)]
#save DTW which is the difference in well depth in the spring and fall
saveRDS(
  DTW, 
  file="C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/DTW.rds")
#Rename W_F as we well_depth
Fall_data<-Fall_data[,c(1,2,3,6)]
colnames(Fall_data)[3]<-"well_depth"
saveRDS(
  Fall_data, 
  file="C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/Fall.rds")
Spring_data<-Spring_data[,c(1,2,3,6)]
colnames(Spring_data)[3]<-"well_depth"
saveRDS(
  Spring_data, 
  file="C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/Spring.rds")

## Bringing the shapefile
## Bringing the shapefile
AR.Delta<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Arkansas Alluvial Shapefile/Delta.shp",
                   stringsAsFactors = FALSE)
plot(AR.Delta)
AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]

library(rgeos)
HUC12<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/WBD_HU12_USGS/WBD_HU12_USGS.shp",
                stringsAsFactors = FALSE)

#change projection
HUC12 <- spTransform(HUC12, CRS("+proj=longlat +datum=NAD83 +no_defs "))
AR.Delta<- spTransform(AR.Delta, CRS("+proj=longlat +datum=NAD83 +no_defs "))
Delta_d<-aggregate(Delta, dissolve = TRUE)
AR_huc12 <- gIntersection(HUC12,AR.Delta, byid=TRUE)
plot(AR_huc12)
var_type="Fall"
year="2019"
################------IDW at county----################
#run the function
IDW<-function(year, var_type){
  
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0(var_type, ".rds") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  
  
  Data <- readRDS(paste0( folder_name, file_name)) 
  Data<-  Data[Data$Year==year, ]
  
  
  Data<-Data%>% 
    drop_na()
  head(Data) 
  # Data<-read.csv("Data")
  # head(Data)
  
  
  dsp <- SpatialPoints(Data[,1:2], proj4string=CRS("+proj=longlat +datum=NAD83"))
  dsp <- SpatialPointsDataFrame(dsp,Data)
  summary(dsp$well_depth)
  
  cuts <- c(0,60,100,140,180,220)
  blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
  pols <- list("sp.polygons", AR.Delta, fill = "lightgray")
  
  plot(AR.Delta)
  spplot(dsp, 'well_depth', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
  
  dta <- spTransform(dsp,TA)
  cata <- spTransform(AR.Delta, TA) ###correct and update this portion for huc 12
  
  v <- voronoi(dta)
  #plot(v)
  ca <- aggregate(cata) %>%
    gBuffer(byid=TRUE, width=0)
  vca <-raster::intersect(v, ca)
  names(vca)
  #spplot(vca, "well_depth", col.regions=rev(get_col_regions()))
  r <- raster(cata, res=1000) 
  vr <- rasterize(vca, r, vca@data[["well_depth"]])
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
 
  
  write.csv(rmse, paste("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW/County/RMSE_",var_type, year,".csv"), row.names = T)
  
  
  writeRaster(idwr, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW/County/IDW_",var_type, year, ".tif"),  overwrite = T) 
  test_spdf <- as(idwr, "SpatialPixelsDataFrame")
  test_sf<-st_as_sf(test_spdf)
  Delta_sf<-st_as_sf(Delta)
  
  test_sf<-st_transform(test_sf,crs = crs(Delta))
  
  #test_sf <- mutate(test_sf, IDW_Fall2019_cat = cut_number(IDW_Fall2019, n = 4))
  
  colnames(test_sf)[1]<-"value"
  file_name<- paste0("IDW_plot_",var_type, year, ".jpg") 
  folder_name<-paste0(var_type)
  Plot_name <-paste("Depth to water (ft.)",var_type,"", year)
  IDW<-ggplot()+geom_sf(data=test_sf, aes(color = value),size=0.02)+
    geom_sf(data=Delta_sf,fill=NA, color="grey20", size=0.5) +
    scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                          name = Plot_name)+
    annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-95, -89), ylim = c(32.5, 37), expand = F)+theme_bw()+ 
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
    theme(legend.title = element_text(size=10, face="italic"))
  ggsave(paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Figure/IDW/County/" , var_type, "/", "IDW_plot_",var_type, year,".jpeg"), IDW)

}


#--- run the above code in parallel ---#
future_lapply(
  2000:2019,
  function (x) IDW(x, "Fall")
)



future_lapply(
  2000:2019,
  function (x) IDW(x, "Spring")
)

future_lapply(
  2000:2019,
  function (x) IDW(x, "DTW")
)


#IDW at HUC 12


################------Figure from IDW at HUC12----################

IDW<-function(year, var_type){
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0(var_type, ".rds") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  
  
  Data <- readRDS(paste0( folder_name, file_name)) 
  Data<-  Data[Data$Year==year, ]
  
  
  Data<-Data%>% 
    drop_na()
  head(Data) 
  # Data<-read.csv("Data")
  # head(Data)
  
  
  dsp <- SpatialPoints(Data[,1:2], proj4string=CRS("+proj=longlat +datum=NAD83"))
  dsp <- SpatialPointsDataFrame(dsp,Data)
  summary(dsp$well_depth)
  
  cuts <- c(0,60,100,140,180,220)
  blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
  pols <- list("sp.polygons", AR_huc12, fill = "lightgray")
  
  plot(AR_huc12)
  spplot(dsp, 'well_depth', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
  
  dta <- spTransform(dsp,TA)
  cata <- spTransform(AR_huc12, TA) ###correct and update this portion for huc 12
  
  v <- voronoi(dta)
  #plot(v)
  ca <- aggregate(cata) %>%
    gBuffer(byid=TRUE, width=0)
  vca <-raster::intersect(v, ca)
  names(vca)
  #spplot(vca, "well_depth", col.regions=rev(get_col_regions()))
  r <- raster(cata, res=1000) 
  vr <- rasterize(vca, r, vca@data[["well_depth"]])
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
  
  
  write.csv(rmse, paste("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW/HUC12/RMSE_",var_type, year,".csv"), row.names = T)
  
  
  writeRaster(idwr, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW/HUC12/IDW_",var_type, year, ".tif"),  overwrite = T) 
  
  #idwr<-raster(paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_IDW/HUC12/IDW_",var_type, year, ".tif"))
  
  test_spdf <- as(idwr, "SpatialPixelsDataFrame")
  test_sf<-st_as_sf(test_spdf)
  Delta_sf<-st_as_sf(AR_huc12)
  
  test_sf<-st_transform(test_sf,crs = crs(AR_huc12))
  
  #test_sf <- mutate(test_sf, IDW_Fall2019_cat = cut_number(IDW_Fall2019, n = 4))
  
  colnames(test_sf)[1]<-"value"
  file_name<- paste0("IDW_plot_",var_type, year, ".jpg") 
  folder_name<-paste0(var_type)
  Plot_name <-paste("Depth to water (ft.)",var_type,"", year)
  IDW<-ggplot()+geom_sf(data=test_sf, aes(color = value),size=0.02)+
    geom_sf(data=Delta_sf,fill=NA, color="grey20", size=0.5) +
    scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                          name = Plot_name)+
    annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-95, -89), ylim = c(32.5, 37), expand = F)+theme_bw()+ 
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
    theme(legend.title = element_text(size=10, face="italic"))
  ggsave(paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Figure/IDW/HUC12/" , var_type, "/", "IDW_plot_",var_type, year,".jpeg"), IDW)
  
}



future_lapply(
  2000:2019,
  function (x) IDW(x, "Fall")
)





future_lapply(
  2000:2019,
  function (x) IDW(x, "Spring")
)

future_lapply(
  2000:2019,
  function (x) IDW(x, "DTW")
)
library(cowplot)
library(grid)
library(gridExtra)

################------Kriging at county----################
#Kriging at county.
Krige_county<-function(year, var_type){
  
  
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0(var_type, ".rds") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  
  
  Data <- readRDS(paste0( folder_name, file_name)) 
  Data<-  Data[ Data$Year==year, ]
  
  
  Data<-Data%>% 
    drop_na()
  head(Data) 
  # Data<-read.csv("Data")
  # head(Data)
  
  #Data = Data[which(!duplicated(Data[1:2])), ]
  coordinates(Data) <- ~Longitude + Latitude
  crs(Data)
  proj4string(Data) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
  
  library(rgdal)
  projection(Data)
  #Data <- Data [-zerodist(Data)[,1],]
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
  
  write.csv(krigrmse, paste("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig/County/RMSE_",var_type, year,".csv"), row.names = T)
  writeRaster(ok, paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig/County/krig_",var_type, year, ".tif"),  overwrite = T) 
  
  test_spdf <- as(ok, "SpatialPixelsDataFrame")
  test_sf<-st_as_sf(test_spdf)
  Delta_sf<-st_as_sf(Delta)
  
  test_sf<-st_transform(test_sf,crs = crs(Delta))
  
  #test_sf <- mutate(test_sf, IDW_Fall2019_cat = cut_number(IDW_Fall2019, n = 4))
  
  
  file_name<- paste0("IDW_plot_",var_type, year, ".jpg") 
  folder_name<-paste0(var_type)
  Plot_name <-paste("Depth to water (ft.)",var_type,"", year)
  Kring<-ggplot()+geom_sf(data=test_sf, aes(color = prediction),size=0.02)+
    geom_sf(data=Delta_sf,fill=NA, color="grey20", size=0.5) +
    scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                          name = Plot_name)+
    annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-95, -89), ylim = c(32.5, 37), expand = F)+theme_bw()+ 
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
    theme(legend.title = element_text(size=10, face="italic"))
  
  Kring_var<-ggplot()+geom_sf(data=test_sf, aes(color = variance),size=0.02)+
    geom_sf(data=Delta_sf,fill=NA, color="grey20", size=0.5) +
    scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                          name = Plot_name)+
    annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-95, -89), ylim = c(32.5, 37), expand = F)+theme_bw()+ 
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
    theme(legend.title = element_text(size=10, face="italic"))
  bd=plot_grid(Kring,  Kring_var, 
               labels = c("Prediction", "Variance"),
               ncol = 2, nrow = 1,label_size=8)
  bd
  ggsave(paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Figure/Kringing/County/" , var_type, "/", "IDW_plot_",var_type, year,".jpeg"), bd)
  
}
library(gridExtra)

#--- run the above code in parallel ---#
future_lapply(
  2000:2019,
  function (x) Krige_county(x, "Fall")
)




future_lapply(
  2000:2019,
  function (x) Krige_county(x, "Spring")
)

future_lapply(
  2000:2019,
  function (x) Krige_county(x, "DTW")
)



###################HUC12#############
Krige_huc<-function(year, var_type){
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0(var_type, ".rds") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  
  
  Data <- readRDS(paste0( folder_name, file_name)) 
  Data<-  Data[ Data$Year==year, ]
  
  
  Data<-Data%>% 
    drop_na()
  head(Data) 
  # Data<-read.csv("Data")
  # head(Data)
  
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
  ok<-raster(paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/raster_krig/HUC12/krig_",var_type, year, ".tif"))
   
   test_spdf <- as(ok, "SpatialPixelsDataFrame")
  test_sf<-st_as_sf(test_spdf)
  Delta_sf<-st_as_sf(AR_huc12)
  
  test_sf<-st_transform(test_sf,crs = crs(AR_huc12))
  
  #test_sf <- mutate(test_sf, IDW_Fall2019_cat = cut_number(IDW_Fall2019, n = 4))
  
  
  file_name<- paste0("IDW_plot_",var_type, year, ".jpg") 
  folder_name<-paste0(var_type)
  Plot_name <-paste("Depth to water (ft.)",var_type,"", year)
  Kring<-ggplot()+geom_sf(data=test_sf, aes(color = prediction),size=0.02)+
    geom_sf(data=Delta_sf,fill=NA, color="grey20", size=0.5) +
    scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                          name = Plot_name)+
    annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-95, -89), ylim = c(32.5, 37), expand = F)+theme_bw()+ 
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
    theme(legend.title = element_text(size=10, face="italic"))
  
  Kring_var<-ggplot()+geom_sf(data=test_sf, aes(color = variance),size=0.02)+
    geom_sf(data=Delta_sf,fill=NA, color="grey20", size=0.5) +
    scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                          name = Plot_name)+
    annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-95, -89), ylim = c(32.5, 37), expand = F)+theme_bw()+ 
    theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
    theme(legend.title = element_text(size=10, face="italic"))
  bd=plot_grid(Kring,  Kring_var, 
               labels = c("Prediction", "Variance"),
               ncol = 2, nrow = 1,label_size=8)
  bd
  ggsave(paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Figure/Kringing/HUC12/" , var_type, "/", "IDW_plot_",var_type, year,".jpeg"), bd)
  
}


#--- run the above code in parallel ---#
future_lapply(
  2000:2000,
  function (x) Krige_huc(x, "Fall")
)




future_lapply(
  2000:2019,
  function (x) Krige_huc(x, "Spring")
)

future_lapply(
  2000:2019,
  function (x) Krige_huc(x, "DTW")
)
