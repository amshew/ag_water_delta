
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
options(prism.path = '~/prismtmp')

list.files()

AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonkoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]
#dissolve shapefile
AR.Delta_d<-aggregate(AR.Delta, dissolve = TRUE)

plot(AR.Delta)
plot(AR.Delta_d)
extent(AR.Delta)
crs(AR.Delta)
#HUC12
HUC12<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/WBD_HU12_USGS/WBD_HU12_USGS.shp",
             stringsAsFactors = FALSE)
crs(HUC12)
plot(HUC12)
extent(HUC12)
#change projection
AR.Delta_d<- spTransform(AR.Delta_d, CRS("+proj=longlat +datum=NAD83 +no_defs "))
HUC12 <- spTransform(HUC12, CRS("+proj=longlat +datum=NAD83 +no_defs "))
#clip -both worked
AR_huc12<-crop(HUC12, extent(AR.Delta_d))
plot(AR_huc12)
#this is better
AR_huc12 <- gIntersection(HUC12, AR.Delta_d,id=T, byid=T)
show(AR_huc12 )
AR_huc12 <- intersect(HUC12, AR.Delta_d)
show(AR_huc12 )
plot(AR_huc12)
#AR_huc12 <- spTransform(AR_huc12, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0, "))
#HUC8
HUC_8<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/hydrologic_units_WBDHU8_ar_2521302_01/hydrologic_units/wbdhu8_a_ar.shp",
                stringsAsFactors = FALSE) 
plot(HUC_8)

#clip -both worked
AR_huc8 <-crop(HUC_8, extent(AR.Delta_d))#not good
AR_huc8 <- gIntersection(HUC_8, AR.Delta_d, byid=TRUE)
plot(AR_huc8)
extent(AR_huc8)

year<-2010

library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)



AR2010 <-raster("F:/transfer/cdl/CDL_2010_05.tif") 
AR2011 <-raster("F:/transfer/cdl/CDL_2011_05.tif") 
AR2012 <-raster("F:/transfer/cdl/CDL_2012_05.tif") 
AR2013 <-raster("F:/transfer/cdl/CDL_2013_05.tif") 
AR2014 <-raster("F:/transfer/cdl/CDL_2014_05.tif") 
AR2015 <-raster("F:/transfer/cdl/CDL_2015_05.tif") 
AR2016 <-raster("F:/transfer/cdl/CDL_2016_05.tif") 
AR2017 <-raster("F:/transfer/cdl/CDL_2017_05.tif") 
AR2018 <-raster("F:/transfer/cdl/CDL_2018_05.tif") 
AR2019 <-raster("F:/transfer/cdl/CDL_2019_05.tif")
AR=brick(AR2010, AR2011,AR2012,AR2013,AR2014,AR2015,AR2016,AR2017,AR2018,AR2019)
AR


AR_Mask <-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask.rds")%>%
  plot()
AR_huc12<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12.rds")
plot(AR_huc12)

#re-extent from polygon to raster
AR_huc12<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12.rds")
plot(AR_huc12)

myExtent <- spTransform(AR_huc12, CRS(proj4string(AR2010)))
show(myExtent)
Data_AR<-  crop(AR2010, extent(myExtent)) %>% 
  mask (., myExtent)
  show(Data_AR)
  plot(Data_AR)
  #extract raster to polygon
  library("exactextractr")

 
summary(Data_extract)

library("exactextractr")
Data_extract_long<- raster::extract(AR2010,myExtent)



 