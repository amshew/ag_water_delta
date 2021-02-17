
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

AR2010 <-raster("F:/transfer/cdl/CDL_2010_05.tif") 
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
  RT<-function(x,y){ifelse(x==3&y==3,1, 0)}
  a=1
  b=1+a
  
  Diff<-overlay(temp_stars[[a]],temp_stars[[b]],fun=RT)
  
  
  Data_extract<- raster::extract(Diff,myExtent, 
                                 progress = F,
                                 cellnumbers=TRUE,   df=T,na.rm =T)%>% 
    drop_na() # drop the NA that represent the  masked area to zero then drop them before calculation share
  
  colnames(Data_extract)
  Ft=Data_extract%>%group_by(layer,ID)%>%summarise(count= n(), )%>%mutate( .,Year=end_date)
  Ft <- Ft[Ft$layer==1, ]
  Data_extract2<- raster::extract(temp_stars[[a]],myExtent, 
                                  progress = F,
                                  cellnumbers=TRUE,   df=T,na.rm =T)%>% 
    drop_na()
  colnames(Data_extract2)[3] <- "CDL_Code"
  lt=Data_extract2%>%group_by(ID,CDL_Code)%>%summarise(count_base= n(), )%>%mutate( .,Year=end_date)
  lt <- lt[lt$CDL_Code==3, ]
  
  Jt=merge(Ft,lt,by=c( "Year"= "Year", "ID"="ID"),sort = TRUE)%>%mutate( .,share=count/count_base)%>%mutate( .,Crop_Acre=0.222394*count)
  saveRDS(
    Jt, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/transition/county/monocrop/lu_', "_y", end_date, ".rds")
  )
  
}

future_lapply(
  2006:2007,
  function (x) Share_rice(x)
)

plan(multiprocess, workers = num_cores)
start_date=2010


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

  Diff<-overlay(temp_stars[[a]],temp_stars[[b]],fun=RT)

 
  Data_extract<- raster::extract(Diff,myExtent, 
                                 progress = F,
                                 cellnumbers=TRUE,   df=T,na.rm =T)%>% 
    drop_na() # drop the NA that represent the  masked area to zero then drop them before calculation share
  
  colnames(Data_extract)
  Ft=Data_extract%>%group_by(layer,ID)%>%summarise(count= n(), )%>%mutate( .,Year=end_date)
  Ft <- Ft[Ft$layer==1, ]
  Data_extract2<- raster::extract(temp_stars[[a]],myExtent, 
                                  
                                 progress = F,
                                 cellnumbers=TRUE,   df=T,na.rm =T)%>% 
    drop_na()
  colnames(Data_extract2)[3] <- "layer"
  lt=Data_extract2%>%group_by(ID,layer)%>%summarise(count_base= n(), )%>%mutate( .,Year=end_date)
  lt <- lt[lt$layer==3|lt$layer==5, ]
  
  Jt=merge(Ft,lt,by=c( "Year"= "Year", "ID"="ID"),sort = TRUE)%>%mutate( .,share=count/count_base)%>%mutate( .,Crop_Acre=0.222394*count)
  saveRDS(
    Jt, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/transition/county/monocrop/lu_', "_y", end_date, ".rds")
  )
  
}

future_lapply(
  2007:2018,
  function (x) Share_rice(x)
)


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Stack/brick the files and extract for rotation-dont need separate code
#--- starting date of the working month-year ---#

#append together

ab<- data.frame(AR.Delta@plotOrder, AR.Delta@data[["NAME10"]],AR.Delta@data[["COUNTYFP10"]])%>%as.data.frame()
colnames(ab)[1]<- "ID"
colnames(ab)[2]<- "County"
colnames(ab)[3]<- "COUNTYFP10"
ab$COUNTYFP10<- as.numeric(as.character(ab$COUNTYFP10))
lu<-'C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/transition/county/monocrop'
lu_panel<-list.files(lu,
                     full.names = T,
                     pattern=".rds$")
all.the.data <- lapply( lu_panel,  readRDS)

lu_panel<- do.call("rbind", all.the.data)
lu_panel<-merge(lu_panel,ab)
#colnames(lu_panel)[4]<- "Year"
colnames(lu_panel)[7]<- "share_freq"
lu_panel=lu_panel%>%
  subset( ., select = -c(layer,count,count_base,CDL_Code))
#save code 
saveRDS(
  lu_panel, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/lu_panel_county_mon.rds')

