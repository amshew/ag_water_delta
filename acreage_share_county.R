rm(list=ls()) # Caution: this clears the Environment


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
  prism,
  rgal# download PRISM data
)   
library(sf)
library(pacman)
library(cdlTools)
library(prism)
library(rgdal)
library(raster)
setwd("C:/Users/obemb/OneDrive/Documents")


list.files()

start.time <- Sys.time()
AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]
#dissolve shapefile
#AR.Delta_d<-aggregate(AR.Delta, dissolve = TRUE)
plot(AR.Delta)

#re-extent from polygon to raster
AR2010 <-raster("E:/transfer/cdl/CDL_2010_05.tif") 
myExtent <- spTransform(AR.Delta, CRS(proj4string(AR2010)))
show(myExtent)
AR_Mask <-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask.rds")
show(AR_Mask)
crs(AR_Mask)<-"+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m ++no_defs " 
file_name <- raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/PRISM_ppt_stable_4kmD2_19850101_bil/PRISM_ppt_stable_4kmD2_19850101_bil.bil")

ab<-AR.Delta@data <- data.frame(AR.Delta@data,AR.Delta@plotOrder,raster::extract(file_name, SpatialPoints(AR.Delta), sp = T))%>%as.data.frame()


colnames(ab)
colnames(ab)[20] <- "longitude"
colnames(ab)[21] <-"latitude"
colnames(ab)[18] <-"ID"
ab=subset(ab, select = c(ID,COUNTYFP10,NAME10,latitude,longitude))
saveRDS(ab, file = 'C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord_AD.rds')
coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord_AD.rds')

plot(myExtent)
library(parallel)
num_cores <- detectCores() 
year=2010
plan(multiprocess, workers = num_cores)
year<-2008
share_crop<-function(year){
  folder_name <- paste0("E:/transfer/cdl") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("CDL","_",year,"_","05",".tif") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  Data <-raster(paste0( folder_name,"/", file_name))
  print(Data)
  Data_AR<-  crop(Data, extent(myExtent)) 
  extent(Data_AR)
  extent(AR_Mask)
  Data_AR_crop<-crop ( AR_Mask,Data_AR ) #crop to get the same extent
  extent( Data_AR_crop)
  Data_AR_mask<- mask (Data_AR,Data_AR_crop)
  #plot(Data_AR_mask)
  
  memory.limit(size=10000000) 
  Data_extract<- raster::extract(Data_AR_mask,myExtent, 
                                 progress = F,
                                 cellnumbers=TRUE,   df=T,na.rm =T)%>% 
    drop_na() # drop the NA that represent the  masked area to zero then drop them before calculation share
  
  
  
  #colnames(Data_extract)
  colnames(Data_extract)[3] <- "CDL_Code"
  colnames(Data_extract)[2] <- "Count"
  colnames(Data_extract)
 
  Data_extract_sub<-Data_extract[!(Data_extract$CDL_Code ==195 | Data_extract$CDL_Code ==190|Data_extract$CDL_Code ==176),]
  
  #NLCD fails to mask wetwoods-195,190 and grassland and pasture-176
  Data_extract_count<-Data_extract_sub%>%group_by(ID,CDL_Code)%>%summarise(count= n())
  tmp <- tapply(Data_extract_count$count, Data_extract_count$ID, sum) #Obtains sums by ID
  Data_extract_count$sum <- tmp[ Data_extract_count$ID] 
  Data_extract_count=Data_extract_count%>%
  mutate( .,Acreage=0.222394*sum)%>% mutate( .,share=count/sum)%>%mutate( .,Crop_Acre=0.222394*count)%>%subset( ., select = -c(count,sum))%>%
    merge(.,coord,  by=c("ID"),sort = TRUE)%>% 
    mutate (.,Year=year)%>%subset( ., select = -c(latitude,longitude))
  
  
  
  
  saveRDS(
    Data_extract_count, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County/Crop_share_', "y", year, '.rds')
  )
}

future_lapply(
  2008:2019,
  function (x) share_crop(x)
)
library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)
  year<-2006
share_crop<-function(year){

folder_name <- paste0("F:/transfer/cdl") 
print(folder_name)
#--- the file name of the downloaded data ---#
file_name <- paste0("CDL","_",year,"_","05",".tif") 
print(file_name)
#--- complete path to the downloaded files ---#

Data <-raster(paste0( folder_name,"/", file_name))
print(Data)


Data <-raster(paste0( folder_name,"/", file_name))
print(Data)
if(year==2006|year==2007) { r.new = resample(Data,AR2010,method= 'ngb')
}

#show(r.new)
Data_AR<-  crop(r.new, extent(myExtent)) 
#extent(Data_AR)
#extent(AR_Mask)
Data_AR_crop<-crop ( AR_Mask,Data_AR ) #crop to get the same extent
extent(Data_AR_crop)
Data_AR_mask<- mask (Data_AR,Data_AR_crop)
#extent( Data_AR_mask)  


#plot(Data_AR_mask)
Data_extract<- raster::extract(Data_AR_mask,myExtent, 
                               progress = F,
                               cellnumbers=TRUE,   df=T,na.rm =T)%>% 
  drop_na() # drop the NA that represent the  masked area to zero then drop them before calculation share



#colnames(Data_extract)
colnames(Data_extract)[3] <- "CDL_Code"
colnames(Data_extract)[2] <- "Count"
colnames(Data_extract)

Data_extract_sub<-Data_extract[!(Data_extract$CDL_Code ==0 |Data_extract$CDL_Code ==195 | Data_extract$CDL_Code ==190|Data_extract$CDL_Code ==176),]

#NLCD fails to mask wetwoods-195,190 and grassland and pasture-176
Data_extract_count<-Data_extract_sub%>%group_by(ID,CDL_Code)%>%summarise(count= n())
tmp <- tapply(Data_extract_count$count, Data_extract_count$ID, sum) #Obtains sums by ID
Data_extract_count$sum <- tmp[ Data_extract_count$ID] 
Data_extract_count=Data_extract_count%>%
  mutate( .,Acreage=0.222394*sum)%>% mutate( .,share=count/sum)%>%mutate( .,Crop_Acre=0.222394*count)%>%subset( ., select = -c(count,sum))%>%
  merge(.,coord,  by=c("ID"),sort = TRUE)%>% 
  mutate (.,Year=year)%>%subset( ., select = -c(latitude,longitude))




saveRDS(
  Data_extract_count, 
  paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County/Crop_share_', "y", year, '.rds')
)
}
future_lapply(
  2006:2007,
  function (x) share_crop(x)
)

#append all the data
share_crop<-"C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County"
share_crop_panel<-list.files(share_crop,
                             full.names = T,
                             pattern=".rds$")
all.the.data <- lapply( share_crop_panel,  readRDS)
share_crop_panel_county<- do.call("rbind", all.the.data)

saveRDS(
  share_crop_panel_county, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County/share_crop_panel_county.rds')



subset_crop<-function(d){
  folder_name <- paste0("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0('share_crop_panel_county', '.rds') 
  Data <-readRDS(paste0( folder_name,"/", file_name))
  print(file_name)
  Data_extract<-subset(Data , CDL_Code ==d)
  saveRDS(
    Data_extract, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County/CDL/CDL_', d,"_", '.rds')
  )
}

future_lapply(
  1:5,
  function (x) subset_crop(x)
  
)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#'C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/share_crop_panel.rds'
afa<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County/CDL/CDL_3_.rds')

# 2006-2007
#resampling using the nearest neighbor from 30-(soil_Delta) to 56-(nlcd) using nearest neighbor

