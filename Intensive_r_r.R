
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

AR2010 <-raster("E:/transfer/cdl/CDL_2010_05.tif") 
myExtent <- spTransform(AR.Delta, CRS(proj4string(AR2010)))
show(myExtent)
AR_Mask <-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask.rds")
ab<-myExtent@data <- data.frame(myExtent@data,myExtent@plotOrder)%>%subset(., select = c(myExtent.plotOrder,COUNTYFP10,NAME10,INTPTLAT10,INTPTLON10))
colnames(ab)[1] <-"ID"
colnames(ab)[4] <- "longitude"
colnames(ab)[5] <-"latitude"

plot(myExtent)
library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)
year=2010
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
  #extent(Data_AR)
  #extent(AR_Mask)
  Data_AR_crop<-crop ( AR_Mask,Data_AR ) #crop to get the same extent
  Data_AR_mask<- mask (Data_AR,Data_AR_crop)
  reclass_df<-c(195,0,
                195,0,
                176,0
  )
  
  reclass_m <- matrix(reclass_df,
                      ncol = 2,
                      byrow = TRUE)
  reclass_m 
  AR_classified <- reclassify( Data_AR_mask,
                                    reclass_m)
  AR_classified[AR_classified== 0] <- NA
  
  reclass_da<-c(1,2,0,
                4,243,0,
                3,3,1
                  )
  reclass_m <- matrix(reclass_da,
                      ncol = 3,
                      byrow = TRUE)
  reclass_m 
  Data_crop<-reclassify(AR_classified,
                        reclass_m  )
  reclass_db<-c(1,0,
                4,0
              
  )
  reclass_ma <- matrix(reclass_db,
                      ncol = 2,
                      byrow = TRUE)
  reclass_ma 
  Data_rice<-reclassify(Data_crop,
                        reclass_ma  )
  
  writeRaster(
    Data_rice, 
    paste0('F:/transfer/Rotate/output/Crop_share_', "y", year, '.bil'),  overwrite = T)
  
}

future_lapply(
  2008:2019,
  function (x) share_crop(x)
)


#Stack/brick the files and extract for rotation
#--- starting date of the working month-year ---#

library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)


Share_rice<-function(start_date ){
  
  
  #--- end date ---#
  #end_date <- dmy(paste0("1/1/",  temp_start_year + 1)) - 1
  end_date <-    start_date+1
  end_date
#--- list of dates of the working month-year ---#
year <- seq(start_date, end_date) 
year

#folder_name <- paste0("Crop_share_", "y",year, "_bil") 
#print(folder_name)
#--- the file name of the downloaded data ---#
file_name <- paste0("Crop_share_", "y",year, ".bil") 
#print(file_name)
#--- complete path to the downloaded files ---#

file_path <- paste0('F:/transfer/Rotate/output', "/", file_name)
#file_path


temp_stars <- stack(file_path)
RT<-function(x,y){ifelse(x==3&y==3,1, 0)}
a=1
b=1+a
#show(temp_stars[[a]])
Diff<-overlay(temp_stars[[a]],temp_stars[[b]],fun=RT)
  #plot(Diff)
#show(Diff)
Data_extract<- raster::extract(Diff,myExtent, 
                               progress = F,
                               cellnumbers=TRUE,   df=T,na.rm =T)%>% 
  drop_na() # drop the NA that represent the  masked area to zero then drop them before calculation share

Ft=Data_extract%>%group_by(layer,ID)%>%summarise(count= n())
tmp <- tapply( Ft$count,  Ft$ID, sum) #Obtains sums by ID
Ft$sum <- tmp[  Ft$ID] 
Ft=Ft%>%mutate( .,share=count/sum)%>%mutate( .,Crop_Acre_con=0.222394*count)%>%mutate( .,Acreage_con=0.222394*sum)%>%
  subset( ., select = -c(count,sum))%>%mutate( .,Year=end_date)


Ft <- Ft[Ft$layer==1, ]
saveRDS(
  Ft, 
  paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/transition/county/lu_', "_y", end_date, ".rds")
)

}

future_lapply(
  2008:2018,
  function (x) Share_rice(x)
)

#For 2006-2007
year=2007
share_crop<-function(year){
  folder_name <- paste0("F:/transfer/cdl") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("CDL","_",year,"_","05",".tif") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  Data <-raster(paste0( folder_name,"/", file_name))
  #print(Data)
  Data <-raster(paste0( folder_name,"/", file_name))
  #print(Data)
  if(year==2006|year==2007) { r.new = resample(Data,AR2010,method= 'ngb')
  }
  
  #show(r.new)
  Data_AR<-  crop(r.new, extent(myExtent)) 
  #extent(Data_AR)
  #extent(AR_Mask)
  Data_AR_crop<-crop ( AR_Mask,Data_AR ) #crop to get the same extent
  #extent(Data_AR_crop)
  Data_AR_mask<- mask (Data_AR,Data_AR_crop)
  #extent( Data_AR_mask)  
  
  #plot(Data_AR_mask)
  reclass_df<-c(195,0,
                195,0,
                176,0
  )
  
  reclass_m <- matrix(reclass_df,
                      ncol = 2,
                      byrow = TRUE)
  reclass_m 
  AR_classified <- reclassify( Data_AR_mask,
                               reclass_m)
  AR_classified[AR_classified== 0] <- NA
  
  reclass_da<-c(1,2,0,
                4,243,0,
                3,3,1
  )
  reclass_m <- matrix(reclass_da,
                      ncol = 3,
                      byrow = TRUE)
  reclass_m 
  Data_crop<-reclassify(AR_classified,
                        reclass_m  )
  reclass_db<-c(1,0,
                4,0
                
  )
  reclass_ma <- matrix(reclass_db,
                       ncol = 2,
                       byrow = TRUE)
  reclass_ma 
  Data_rice<-reclassify(Data_crop,
                        reclass_ma  )
  
  writeRaster(
    Data_rice, 
    paste0('F:/transfer/Rotate/output/Crop_share_', "y", year, '.bil'),  overwrite = T)
  
}

future_lapply(
  2006:2007,
  function (x) share_crop(x)
)


#Stack/brick the files and extract for rotation-dont need separate code
#--- starting date of the working month-year ---#

library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)
start_date=2006

Share_rice<-function(start_date ){
  
  
  #--- end date ---#
  #end_date <- dmy(paste0("1/1/",  temp_start_year + 1)) - 1
  end_date <-    start_date+1
  end_date
  #--- list of dates of the working month-year ---#
  year <- seq(start_date, end_date) 
  year
  
  #folder_name <- paste0("Crop_share_", "y",year, "_bil") 
  #print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("Crop_share_", "y",year, ".bil") 
  #print(file_name)
  #--- complete path to the downloaded files ---#
  
  file_path <- paste0('F:/transfer/Rotate/output', "/", file_name)
  file_path
  
  
  temp_stars <- stack(file_path)
  RT<-function(x,y){ifelse(x==3&y==3,1,0)}
  a=1
  b=1+a
  show(temp_stars[[a]])
  Diff<-overlay(temp_stars[[a]],temp_stars[[b]],fun=RT)
 
  #plot(Diff)
  #show(Diff)
  Data_extract<- raster::extract(Diff,myExtent, 
                                 progress = F,
                                 cellnumbers=TRUE,   df=T,na.rm =T)%>% 
    drop_na() # drop the NA that represent the  masked area to zero then drop them before calculation share
  
  colnames(Data_extract)
  
  Ft=Data_extract%>%group_by(layer,ID)%>%summarise(count= n())
  tmp <- tapply( Ft$count,  Ft$ID, sum) #Obtains sums by ID
  Ft$sum <- tmp[  Ft$ID] 
  Ft=Ft%>%mutate( .,share=count/sum)%>%mutate( .,Crop_Acre_con=0.222394*count)%>%mutate( .,Acreage_con=0.222394*sum)%>%
    subset( ., select = -c(count,sum))%>%mutate( .,Year=end_date)
  
  
 
  Ft <- Ft[Ft$layer==1, ]
  saveRDS(
    Ft, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/transition/county/lu_', "_y", end_date, ".rds")
  )
  
}

future_lapply(
  2006:2007,
  function (x) Share_rice(x)
)

#append together

ab<- data.frame(AR.Delta@plotOrder, AR.Delta@data[["NAME10"]],AR.Delta@data[["COUNTYFP10"]])%>%as.data.frame()
colnames(ab)[1]<- "ID"
colnames(ab)[2]<- "County"
colnames(ab)[3]<- "COUNTYFP10"
ab$COUNTYFP10<- as.numeric(as.character(ab$COUNTYFP10))
lu<-'C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/transition/county'
lu_panel<-list.files(lu,
                        full.names = T,
                        pattern=".rds$")
all.the.data <- lapply( lu_panel,  readRDS)

lu_panel<- do.call("rbind", all.the.data)
lu_panel<-merge(lu_panel,ab)
#colnames(lu_panel)[4]<- "Year"
colnames(lu_panel)[3]<- "share_freq"
#save code 
saveRDS(
  lu_panel, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/lu_panel_county.rds')


library(dplyr)
####
colnames(lu_panel)[4]<- "Crop_Acres_cond"
Expansion<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/County/CDL/CDL_3_.rds')
Expansion$COUNTYFP10<- as.numeric(as.character(Expansion$COUNTYFP10)) %>% as.numeric(as.character(Expansion$Year))
Expansion$yearl=Expansion$Year+1
Expansion<-Expansion%>%subset( ., select = -c(Year))
colnames(Expansion)[10]<- "Year"
lu=merge(lu_panel,Expansion, by=c("COUNTYFP10" =  "COUNTYFP10", "Year"= "Year", "ID"="ID"),sort = TRUE)%>%
  subset( ., select = -c(NAME10,longitude,latitude))%>%mutate( .,diff=Crop_Acres_cond/Crop_Acre)%>%mutate( .,share_cropland=Crop_Acres_cond/Acreage)

saveRDS(
  lu, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/lu_panel_county_updated.rds')
