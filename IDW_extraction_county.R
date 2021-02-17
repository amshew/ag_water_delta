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
library(deldir)
AR_Mask <-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask.rds")%>%
  plot()
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


library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)

start_date=2010
var_type<-"Spring"
#IDW at HUC 12

IDW<-function(start_date, var_type){

  
  #--- end date ---#
  #end_date <- dmy(paste0("1/1/",  temp_start_year + 1)) - 1
  end_date <-    start_date+1
  end_date
  #--- list of dates of the working month-year ---#
  year <- seq(start_date, end_date) 
  year
  folder_name <- paste0("raster_krig")
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("krig_",var_type, year,".tif") 
  print(file_name) 
  file_path <- paste0('C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Result_raster/', folder_name, "/", file_name)
  file_path
  
  #--- combine all the PRISM files as a RasterStack ---#
  temp_stars <- stack(file_path) 
  show(temp_stars)
  
  #re-extent from polygon to raster

  Data_extract<-data.frame(raster::extract(temp_stars,myExtent, 
                                            progress = F,
                                            fun=mean,   sp=T,na.rm =T)) %>%  
    subset( ., select = -c(COUNTYNS10, GEOID10,NAME10,NAMELSAD10, LSAD10,
                           CLASSFP10, MTFCC10, CSAFP10, CBSAFP10, METDIVFP10,FUNCSTAT10,ALAND10,
                           AWATER10,  INTPTLAT10,   INTPTLON10))%>%
    subset( ., select = -c(STATEFP10))%>%select(.,-ends_with(".2"))
  head(Data_extract)
  
  names(  Data_extract) <-(c('COUNTYFP10',paste0('DTW_', var_type),'year'))
 
  
  Data_extract <-   Data_extract[c('COUNTYFP10',paste0('DTW_', var_type),'year'    )]
  colnames(  Data_extract) 
  
  saveRDS(
    Data_extract, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/County/IDW_',var_type, ".rds")
  )
  
  
  
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
  function (x) Krige_county(x, "DTW")
)



IDW_append<-function(var_type){
  #append all the data
  file_name <- paste0(var_type,"_DWT") 
  panel <- paste0(var_type,"_DWT_panel") 
  file_name<-'C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/County'
  DTW_panel<-list.files(file_name,
                        full.names = T,
                        pattern=".rds$")
  all.the.data <- lapply( DTW_panel,  readRDS)
  
  panel<- do.call("rbind", all.the.data)
  
  saveRDS(
    panel, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/',var_type, "_DTW_panel",'.rds'))
  unlink( DTW_panel, recursive = TRUE)
  
}

Fall<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/Fall_DTW_panel.rds')
Spring<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/Spring_DTW_panel.rds')
Diff<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/DWT_DTW_panel.rds')
#coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord.rds')
colnames(Diff)[3] <-"diff_DTW"    
colnames(Diff)
Fall_spring<-merge(Fall, Spring, by=c("ID"),sort = TRUE)

DTW<- merge( Fall_spring, Diff, by=c("ID"),sort = TRUE)

saveRDS(
  DTW, 
  paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/DTW.rds'))
ab=readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/County/IDW_Fall2010.rds')
