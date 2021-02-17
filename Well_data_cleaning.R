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

#Read the data into R
library("readxl")
well_reading<- read_excel( "C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Alluvial_siteWI.xlsx" ) %>%
  subset( ., select = c(WL_Below_L,Measuremen,Latitude__,Longitude,Station_Na))
colnames(well_reading)[2]<-"date"
colnames(well_reading)[3]<-"Latitude"
colnames(well_reading)[1]<-"well_depth"
well_reading$Year <-as.numeric( format(as.Date(well_reading$date), format = "%Y"))
well_reading$month <- as.numeric(format(as.Date(well_reading$date), format = "%m"))
well_reading<-well_reading%>%mutate(.,season=ifelse(month>7,"Spring","Fall"))%>%  subset( ., select = -c(date))

#well_reading<- well_reading[well_reading$Year==year&well_reading$season==var_type, ]



Spring<- well_reading[well_reading$season=="Spring", ]
colnames(Spring)[1]<-"W_S"
library("readxl")
well_reading<- read_excel( "C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Alluvial_siteWI.xlsx" ) %>%
  subset( ., select = c(WL_Below_L,Measuremen,Latitude__,Longitude,Station_Na))
colnames(well_reading)[2]<-"date"
colnames(well_reading)[3]<-"Latitude"
colnames(well_reading)[1]<-"well_depth"
well_reading$Year <-as.numeric( format(as.Date(well_reading$date), format = "%Y"))
well_reading$month <- as.numeric(format(as.Date(well_reading$date), format = "%m"))
well_reading<-well_reading%>%mutate(.,season=ifelse(month>7,"Spring","Fall"))%>%  subset( ., select = -c(date))
Fall<- well_reading[well_reading$season=="Fall", ]
colnames(Fall)[1]<-"W_F"
DTW<-merge(Fall,Spring,by=c("Station_Na"="Station_Na", "Year"="Year","Latitude"="Latitude","Longitude" ="Longitude" ),sort = TRUE)%>%
  subset( ., select = -c(month.x,month.y,season.y, season.x))%>%mutate(.,Diff=W_S-W_F)
