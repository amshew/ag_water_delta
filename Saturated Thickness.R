

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
library("exactextractr")

setwd("C:/demandrs/obemb/OneDrive/Documents/R/ag_water_delta/Data")
AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonkoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]

ST<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/Data/Saturated Thickness/MRVA_BotAlt_Thickness_Data/MRVA_BotAlt_Thickness_Data.shp",
             stringsAsFactors = FALSE)
plot(ST)
ST <- spTransform(ST, CRS("+proj=longlat +datum=NAD83 +no_defs+units=m "))
library(rgeos)
ST_AD <- intersect(ST, AR.Delta)
writeOGR(ST_AD,"C:/Users/obemb/OneDrive/Desktop/data/Data/Saturated Thickness/MRVA_BotAlt_Thickness_Data/New folder" ,"ST_AR", driver="ESRI Shapefile")

plot(ST_AD)
ST_raster<-rasterize(ST,AR_huc12field='Thickness',fun=mean)
AR_huc12<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12.rds")
plot(AR_huc12)
plot(ST,add=T)

ST <- spTransform(ST, CRS("+proj=longlat +datum=NAD83 +no_defs "))
#clip -both worked

library(rgeos)
#this is better
AR_huc12_ST <- intersect(ST, AR_huc12)
plot(AR_huc12_ST)
plot(ST)

r <- raster(AR_huc12_ST, res=1000)
plot(r)
r.polys <- rasterize(AR_huc12_ST, r, field = AR_huc12_ST@data[,1], 
                     update = TRUE, updateValue = "NA")
plot(r.polys)
#IDW

av<-extract(AR_huc12_ST,AR_huc12)

r <- raster(AR_huc12_ST, res=1000) 
plot(r)
