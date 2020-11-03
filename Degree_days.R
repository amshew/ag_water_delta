
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
library(dplyr)
#code starts here
# the end bound
b=38
year=2012
start.time <- Sys.time()
memory.limit(size=1000000)
library( foreach)
  gdd<-function(year){
  
    file_name1 <- paste0("PRISM_", "tmax", "_y",year,".rds")  
    file_name2 <- paste0("PRISM_", "tmin", "_y", year,".rds") 
    file_name3 <- paste0("PRISM_", "ppt", "_y", year,".rds") 
    coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord.rds')
    #print(file_name1)
    # print(file_name2)
    #print(file_name3)
    head(coord)
    #--- complete path to the downloaded files ---#
    
    file_path1 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County',  "/", file_name1)
    #print(file_path1)
    file_path2 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County',  "/", file_name2)
    #print(file_path2)
    file_path3 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County',  "/", file_name3)
    #print(file_path3)
    tmax <- readRDS(file_path1)
    tmin <- readRDS(file_path2)
    ppt <- readRDS(file_path3)
    temp_coord<-merge(coord, tmax, by=c("ID"),sort = TRUE)
    
    temp<- merge( temp_coord,  tmin, by=c("date","ID"),sort = TRUE)
    weather<- merge( temp, ppt, by=c("date","ID"),sort = TRUE)
    
    weather$tavg=(weather$tmax+weather$tmin)/2
    #create label for negative bounds by adding Minus;
   
    result <- foreach(b=-10:b,  .combine=cbind) %do%{
    c<-b
    h<-b+1
    e<-abs(h)
    d<-abs(c)
    f<-ifelse(c < 0,paste0("minus",sep = "",d),c)
      weather[,paste0("dday",f,"C", sep="")] <-0
      weather[,paste0("time",f,"C", sep="")] <-0
      case_dd<- weather$tavg - b
      # case 2: bound <= tMin
      weather[,paste0("dday",f,"C", sep="")] <- ifelse( b <=  weather$tmin, case_dd , 0)
      weather$tempSave = acos( (2*b-weather$tmax-weather$tmin)/(weather$tmax-weather$tmin) )
      weather[,paste0("time",f,"C", sep="")] <-ifelse( b<=  weather$tmin,1, 0)
      weather[,paste0("dday",f,"C", sep="")] <-ifelse (weather$tmin < b & b <  weather$tmax,((  weather$tavg-b)* weather$tempSave + ( weather$tmax- weather$tmin)*sin( weather$tempSave)/2)/pi, weather$tavg - b)
      weather[,paste0("time",f,"C", sep="")] <-ifelse ( weather$tmin< b & b <  weather$tmax,(acos( (2*b-weather$tmax-weather$tmin)/(weather$tmax-weather$tmin) )/pi), 1)
      
      
    weather= subset( weather, select = -c(tempSave) ) }
    
    saveRDS(
      weather, 
      paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/degree days/degreedays_time_', "y", year, ".rds")
    )
   
   
}

future_lapply(
1981:2019,
  function (x) gdd (x)
)
b=-10:37
year=1981:2019
for(year in year){
for(i in b){
  c<-b
  h<-b+1
  e<-abs(h)
  d<-abs(c)
  f<-ifelse(c < 0,paste0("minus",sep = "",d),c)
  weather<-  readRDS(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/degree days/degreedays_time_', "y", year, ".rds"))
  
  weather[,paste0("bin",f,"_",e,sep="")]<-weather[,paste0("time",f,"C", sep="")]-weather[,paste0("time",e,"C", sep="")]
}
  weather= select(weather,-starts_with("time"))
  saveRDS(
    weather, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/degree days/degreedays_', "y", year, ".rds")
  )
  unlink(  paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/degree days/degreedays_time_', "y", year, ".rds"), recursive = TRUE)
}

DDays<-'C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/degree days'
DDays_panel<-list.files(DDays,
                        full.names = T,
                        pattern=".rds$")
all.the.data <- lapply( DDays_panel,  readRDS)

DDays_panel<- do.call("rbind", all.the.data)
#save code 
saveRDS(
  DDays_panel, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/DDays_panel.rds')


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
