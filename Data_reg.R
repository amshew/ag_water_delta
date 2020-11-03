
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
memory.limit(size=100000)
setwd('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data')

library("readxl")
cost_share<-read_excel("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/IWM/IWM_costshare_sum.xls")
colnames(cost_share)[1]<- "Year"
colnames(cost_share)[2]<- "HUC12"
colnames(cost_share)[3]<- "Total"
coord_sf <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord_sf.rds')
IWM_costshare<-merge(coord_sf ,cost_share, by=c("HUC12" = "HUC12"),sort = TRUE)%>%
  subset( ., select = -c(longitude,latitude))
Soil_data<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/Soil_data.rds')
DDdays<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/DDays_panel.rds')%>%
  subset( ., select = -c(longitude,latitude))
ET_VPD<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/ET_VPD_panel.rds')%>%
  subset( ., select = -c(tavg,ppt,tmax,tmin,longitude,latitude))
Rice_acreageshare<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/CDL/CDL_3_.rds')%>%
  subset( ., select = -c(longitude,latitude, CDL_Code))
rice_price<-read.csv("C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Rice_price.csv ")
colnames(rice_price)[1]<- "Year"
colnames(Rice_acreageshare)[5]<-"Year"
colnames(Rice_acreageshare)
#merge
Weather<-merge(ET_VPD,DDdays,by=c("ID","date"),sort = TRUE)
Weather$Year <-as.numeric( format(as.Date(Weather$date), format = "%Y"))
Weather$month <- as.numeric(format(as.Date(Weather$date), format = "%m"))

Weather[,paste0("dd","8","_","32","C", sep="")]<-Weather$dday8C-Weather$dday32C

Weather[,paste0("dd","32","C", sep="")]<-Weather$dday32C
Weather<-subset(Weather, month>=3 & month<=5)
Weather<-select(Weather,-starts_with("dday"))
Weather<- select(Weather,-starts_with("bin"))

Weather<-Weather%>%group_by(ID, date) %>%summarise_all(mean)
colnames(Weather)

#Weather1<-Weather%>%group_by(ID, Year) %>%summarise_at(vars(vpd,ET0,mET,tmax,tmin,10:107), mean)
Weather1<-Weather%>%group_by(ID, Year) %>%summarise_at(vars(vpd,ET0,mET,tmax,tmin,tavg, dd8_32C,dd32C), mean)
Weather2<-Weather%>%group_by(ID, Year) %>%summarise_at(vars(ppt), sum)
Weather_year<-merge(Weather1,Weather2,by=c("ID","Year"),sort = TRUE)
Weather_Soil_data<-merge(Weather_year,Soil_data,by=c("ID"),sort = TRUE)
Weather_Soil_cost<-merge(IWM_costshare,Weather_year,by=c("ID","Year"),sort = TRUE)
Data<-merge(Rice_acreageshare,Weather_Soil_cost,by=c("ID","Year"),sort = TRUE) %>%
  inner_join(.,rice_price)%>%mutate(.,t=Year-2009)
write.csv(Data, file="C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data.csv", row.names = FALSE)
colnames(Data)
Data$Total<-Data$Total/1000
Data<-Data%>%mutate(.,logTotal=log(Total))
summary(Data$rice_price)
library(plm)
linearMod <- plm(Share ~  poly(ppt, 2, raw = TRUE)+vpd+Total+rice_price+ poly(t, 2, raw = TRUE)+factor(ID),  data=Data)
summary(linearMod)
coeftest(linearMod , vcov.=vcovHC)
fixef(linearMod)
summary(fixef(linearMod))
summary(fixef(linearMod))[ , c("Estimate", "Pr(>|t|)")] # only estimates and p-values
 library(stats)
linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+ET0+Total+factor(ID) +rice_price+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)
summary(linearMod)

linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+tmin+tmax+Total+factor(ID) +rice_price+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)

summary(linearMod)
linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+tavg+Total+factor(ID) +rice_price+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)

summary(linearMod)
linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+dd8_32C+dd32C+Total+factor(ID) +rice_price+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)
summary(linearMod)
