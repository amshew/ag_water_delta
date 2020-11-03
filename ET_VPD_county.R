
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
library(pacman)
library(cdlTools)
library(prism)
library(rgdal)
library(raster)
setwd("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output")

##### obtain coordinates 
AR_huc12<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12.rds")
plot(AR_huc12)
AR_huc12_sf<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12_sf.rds")

#merge data and calculate weather variables

G =0
u =2

lamda=2450000
library(lubridate)
#pi=3.14159265358979323844 
year=2012
get_weather_y <- function(year) {
  
  
  
  
  file_name1 <- paste0("PRISM_", "tmax", "_y",year,".rds")  
  file_name2 <- paste0("PRISM_", "tmin", "_y", year,".rds") 
  file_name3 <- paste0("PRISM_", "ppt", "_y", year,".rds") 
  coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord_AD.rds')
  
  my_elev<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/Elevation_county.rds')
  print(file_name1)
  print(file_name2)
  print(file_name3)
  head(coord)
  #--- complete path to the downloaded files ---#
  
  file_path1 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County',  "/", file_name1)
  print(file_path1)
  file_path2 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County',  "/", file_name2)
  print(file_path2)
  file_path3 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County',  "/", file_name3)
  print(file_path3)
  tmax <- readRDS(file_path1)
  tmin <- readRDS(file_path2)
  ppt <- readRDS(file_path3)
  my_elev_coord<-merge(coord, my_elev, by=c("ID"),sort = TRUE)
  temp_coord<-merge(my_elev_coord, tmax, by=c("ID"),sort = TRUE)
  
  temp<- merge( temp_coord,  tmin, by=c("date","ID"),sort = TRUE)
  weather<- merge( temp, ppt, by=c("date","ID"),sort = TRUE)
  #gamma with constant elevation
  weather$tavg=(weather$tmax+weather$tmin)/2
  weather$gamma=0.000665*101.3*((293-0.0065*weather$elev)/293)^5.26  # atmospheric pressure
  
  weather$vpd=0.5*0.6108*(exp(17.27*weather$tmax/(weather$tmax+237.3))-exp(17.27*weather$tmin/(weather$tmin+237.3))) #vapour pressure deficit
  weather$Delta=(4098*0.6108*exp(17.27*weather$tavg/(weather$tavg+237.3)))/((weather$tavg+237.3)^2) #slope of vapour pressure curve;
  
  #Generate day of the year (J);
  weather$J = lubridate::yday(weather$date)
  #extraterrestrial radiation (ra)
  weather$dr= 1 + 0.033*cos(2*pi*weather$J /365) # dr is the inverse relative distance Earth-Sun
  weather$delta= 0.409*sin(2*pi*weather$J /365-1.39) # solar dedication
  weather$theta= (pi/180)*weather$latitude   
  weather$omega=acos(-tan(weather$theta)*tan(weather$delta))  # sunset hour angle 
  # RA in mm/day;
  weather$Ra=24*60/pi*0.0820*weather$dr*(weather$omega*sin(weather$theta)*sin(weather$delta) + cos(weather$theta)*cos(weather$delta)*sin(weather$omega) )
  
  weather$Rns=(1-0.23)*0.16*(weather$tmax-weather$tmin)^(1/2)*weather$Ra
  weather$Rnl=4.903*10^-9*(((weather$tmax+273.16)^4+(weather$tmin+273.16)^4)/2)* 
    (0.34-0.14*(0.6108*exp(17.27*weather$tmin/(weather$tmin+237.3)))^(1/2))* 
    (1.35*(0.16*(weather$tmax-weather$tmin)^(1/2)*weather$Ra)/((0.75+2*10^-5)*weather$Ra)-0.35)
  weather$Rn=weather$Rns-weather$Rnl
  #*****penman*****
  weather$ET0=(0.408*weather$Delta*(weather$Rn-G)+weather$gamma*900/(weather$tavg+273)*u*weather$vpd)/ 
    (weather$Delta+weather$gamma*(1+0.34*u))
  
  
  weather= subset( weather, select = -c(gamma, Delta,  J, dr, delta, omega, Ra, Rns, Rnl, Rn, theta ,elev) )
  saveRDS(
    weather, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/ET/ET_', "y", year, ".rds")
  )
  
}

future_lapply(
  1981:2019,
  function (x) get_weather_y  (x )
)

#append all the data
ET_VPD<-"C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/ET"
ET_VPD_panel<-list.files(ET_VPD,
                         full.names = T,
                         pattern=".rds$")
all.the.data <- lapply( ET_VPD_panel,  readRDS)

ET_VPD_panel<- do.call("rbind", all.the.data)

saveRDS(
  ET_VPD_panel, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/ET_VPD_panel_county.rds')
