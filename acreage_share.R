
# clear and set wd ----
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

AR_Mask <-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask.rds")
show(AR_Mask)
crs(AR_Mask)<-"+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m ++no_defs " 

AR_huc12<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12.rds")
plot(AR_huc12)
coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord.rds')
#re-extent from polygon to raster
AR2010 <-raster("F:/transfer/cdl/CDL_2010_05.tif") 
myExtent <- spTransform(AR_huc12, CRS(proj4string(AR2010)))
show(myExtent)


plot(myExtent)
library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)
year<-2012
share_crop<-function(year){
  folder_name <- paste0("F:/transfer/cdl") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("CDL","_",year,"_","05",".tif") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  Data <-raster(paste0( folder_name,"/", file_name))
  print(Data)
Data_AR<-  crop(Data, extent(myExtent)) 
#extent(Data_AR)
#extent(AR_Mask)
Data_AR_crop<-crop ( AR_Mask,Data_AR ) #crop to get the same extent
Data_AR_mask<- mask (Data_AR,Data_AR_crop)
#plot(Data_AR_mask)
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
Data_extract_count<-Data_extract_sub%>%group_by(ID,CDL_Code)%>%summarise(count= n())%>% 
  within(., {sum = ave(count,ID,FUN=sum)} )%>%mutate( .,Acreage=0.222394*sum)%>%mutate( .,Crop_Acre=0.222394*count)%>%
  mutate( .,Share=count/sum) %>%subset( ., select = -c(count,sum))%>%
  merge(.,coord,  by=c("ID"),sort = TRUE)%>% 
  mutate (.,year=year)




saveRDS(
  Data_extract_count, 
  paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/Crop_share_', "y", year, '.rds')
  )
  }


future_lapply(
  2008:2009,
  function (x) share_crop(x)
)
#append all the data
share_crop<-"C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share"
share_crop_panel<-list.files(share_crop,
                             full.names = T,
                             pattern=".rds$")
all.the.data <- lapply( share_crop_panel,  readRDS)

share_crop_panel<- do.call("rbind", all.the.data)

saveRDS(
  share_crop_panel, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/share_crop_panel.rds')



subset_crop<-function(d){
folder_name <- paste0("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share") 
print(folder_name)
#--- the file name of the downloaded data ---#
file_name <- paste0('share_crop_panel', '.rds') 
Data <-readRDS(paste0( folder_name,"/", file_name))
print(file_name)
Data_extract<-subset(Data , CDL_Code ==d)
saveRDS(
  Data_extract, 
  paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/CDL/CDL_', d,"_", '.rds')
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
afa<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/CDL/CDL_3_.rds')%>%group_by(year) %>%summarise_at(vars(Acreage,Crop_Acre,Share), sum)
