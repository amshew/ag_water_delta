
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
#ST
krig<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/ST_mean_krig.rds')
colnames(krig)[2]<-"krig"
idw<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/ST_mean_IDW.rds')
colnames(idw)[2]<-"idw"
ST_cv<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/ST_CV.rds')

ST<-merge(krig,idw)
ST<-merge(ST,ST_cv)
ST$COUNTYFP10<- as.numeric(as.character(ST$COUNTYFP10))
#DTW
DTW<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/Spring_DTW_panel.rds')

colnames(DTW)[2]<- "dtw"
colnames(DTW)[3]<- "Year"
colnames(DTW)
DTW$COUNTYFP10<- as.numeric(as.character(DTW$COUNTYFP10))
#return
Return<-read.csv("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Return/Crop_Return.csv")
#colnames(Return)[3]<- "Soy_Return"
#colnames(Return)[4]<- "Rice_Return"
head(Return)
Return=Return[Return$Year >= "2006" &Return$Year<= "2019",]
colnames(Return)
#cost and acreage
cost_share<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/IWM/IWM_cost_county_subsidy_bundle.rds')


Rice_acreagetran<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/lu_panel_county_mon.rds')%>%
  subset( ., select = -c(County))
Rice_acreagetran$COUNTYFP10<- as.numeric(as.character(Rice_acreagetran$COUNTYFP10))
Rice_cost<-merge(Rice_acreagetran ,cost_share, by=c("COUNTYFP10" =  "COUNTYFP10", "Year"= "Year"),sort = TRUE)

Rice<-merge(Rice_cost ,Return, by=c("COUNTYFP10" =  "COUNTYFP10", "Year"= "Year","fips"="fips"),sort = TRUE)
#merge with DTW
Rice=merge(Rice,DTW, by=c("COUNTYFP10" =  "COUNTYFP10", "Year"= "Year"),sort = TRUE)

Rice=merge( Rice,ST,by=c("COUNTYFP10" =  "COUNTYFP10"))

#soil and weather

Soil_data<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/Soil_data_county.rds')
DDdays<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/DDays_panel_county.rds')%>%
  subset( ., select = -c(longitude,latitude))
ET_VPD<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/ET_VPD_panel_county.rds')%>%
  subset( ., select = -c(tavg,ppt,tmax,tmin,longitude,latitude))


#merge
Weather<-merge(ET_VPD,DDdays,by=c("ID","date","COUNTYFP10","NAME10"),sort = TRUE)
Weather$Year <-as.numeric( format(as.Date(Weather$date), format = "%Y"))
Weather$month <- as.numeric(format(as.Date(Weather$date), format = "%m"))
Weather=Weather[Weather$Year >= "2006" &Weather$Year<= "2019",]
Weather[,paste0("dd","8","_","32","C", sep="")]<-Weather$dday8C-Weather$dday32C

Weather[,paste0("dd","32","C", sep="")]<-Weather$dday32C
Weather<-subset(Weather, month>=3 & month<=5)%>%subset( ., select = -c(NAME10))
Weather<-select(Weather,-starts_with("dday"))
#Weather<- select(Weather,-starts_with("bin"))

Weather<-Weather%>%group_by(ID, date, COUNTYFP10) %>%summarise_all(mean)
colnames(Weather)
Weather$COUNTYFP10<- as.numeric(as.character(Weather$COUNTYFP10))
#Weather1<-Weather%>%group_by(ID, Year) %>%summarise_at(vars(vpd,ET0,mET,tmax,tmin,10:107), mean)
Weather1<-Weather%>%group_by(ID, COUNTYFP10,Year) %>%summarise_at(vars(vpd,ET0,tmax,tmin,tavg, dd8_32C,dd32C), mean)
Weather2<-Weather%>%group_by(ID, Year) %>%summarise_at(vars(ppt), sum)
Weather_year<-merge(Weather1,Weather2,by=c("ID","Year"),sort = TRUE)

Weather_Soil_data<-merge(Weather_year,Soil_data,by=c("ID"),sort = TRUE)
Data<-merge(Rice,Weather_Soil_data,by=c("Year","COUNTYFP10","ID"),sort = TRUE)%>%mutate(.,t=Year-2005)

write.csv(Data, file="C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data_county_freq.csv", row.names = FALSE)
colnames(Data)
Data<-Data%>%mutate(.,logTotal=log(Total))
Data$Total<-Data$Total
Data<-Data%>%mutate(.,lagTotal=lag(logTotal,1))
Data<-Data%>%mutate(.,Totaldtw=Total*dtw)

summary(Data$Rice_Return)
summary(Data$Total)
summary(Data$logTotal)
library(plm)
library(lmtest)
library(sandwich)

linearMod <- plm(share_freq~  ppt+tavg+ Total+soybean_return_fut +rice_return_fut,data=Data, model = "within",index = "COUNTYFP10")
summary(linearMod)
coeftest(linearMod, vcov=vcovHC(linearMod,type="HC0",cluster="group",vcov = vcovHC))
linearMod <- plm(share_freq ~  ppt+tmin +tmax+ Total+soybean_return_fut +rice_return_fut,data=Data, model = "within",index = "COUNTYFP10")
summary(linearMod)
coeftest(linearMod, vcov=vcovHC(linearMod,type="HC0",cluster="group",vcov = vcovHC))




linearMod <- plm(share_freq ~  ppt+tavg+ Total+Crop_Return_fut +rice_return_fut,data=Data, model = "within")
summary(linearMod)
coeftest(linearMod, vcov=vcovHC(linearMod,type="HC0",cluster="group",vcov = vcovHC))
linearMod <- plm(share_freq ~ ppt+tavg+   Total+dtw+Crop_Return_fut +rice_return_fut,data=Data, model = "within")
summary(linearMod)
coeftest(linearMod, vcov=vcovHC(linearMod,type="HC0",cluster="group",vcov = vcovHC))

















library(lmtest)
linearMod <- plm(Share ~  ppt+ET0++   logTotal+Totaldtw+Soy_Return +Rice_Return,data=Data, model = "within")
summary(linearMod)

linearMod <- plm(Share ~  ppt+dd8_32C+dd32C++   logTotal+Totaldtw+Soy_Return +Rice_Return,  data=Data)
summary(linearMod)


linearMod <- plm(Share ~  ppt+tavg+ +Soy_Return +Rice_Return+logTotal,  data=Data)
summary(linearMod)
linearMod <- plm(Share ~  ppt+vpd+ +Soy_Return +Rice_Return+logTotal,  data=Data)
summary(linearMod)

linearMod <- plm(Share ~  ppt+vpd+ lagTotal+Soy_Return +Rice_Return,  data=Data)
summary(linearMod)





linearMod <- plm(Share ~  poly(ppt, 2, raw = TRUE)+vpd+ lag(Total,1) +Soy_Return +Rice_Return+ poly(t, 2, raw = TRUE),  data=Data)
summary(linearMod)
#linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+ET0+logTotal+factor(ID) +Soy_Return +Rice_Return + poly(t, 2, raw = TRUE)+factor(ID), data=Data)
#summary(linearMod)

linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+tmin+tmax+logTotal+factor(ID) +Soy_Return +Rice_Return + poly(t, 2, raw = TRUE)+factor(ID), data=Data)

summary(linearMod)
linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+tavg+logTotal+factor(ID) +Soy_Return +Rice_Return+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)

summary(linearMod)
linearMod <- plm(Share ~ poly(ppt, 2, raw = TRUE)+dd8_32C+dd32C+logTotal+factor(ID) +Soy_Return +Rice_Return+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)
summary(linearMod)



library(plm)
linearMod <- plm(Share ~  ppt+vpd+logTotal+Soy_Return +Rice_Return +t,  data=Data)
summary(linearMod)

linearMod <- plm(Share ~ppt+ET0+Total+factor(ID) +Soy_Return +Rice_Return + t+factor(ID), data=Data)
summary(linearMod)

linearMod <- plm(Share ~ ppt+tmin+tmax+Total+factor(ID) +Soy_Return +Rice_Return + poly(t, 2, raw = TRUE)+factor(ID), data=Data)

summary(linearMod)
linearMod <- plm(Share ~ ppt+tavg+Total+factor(ID) +Soy_Return +Rice_Return+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)

summary(linearMod)
linearMod <- plm(Share ~ ppt+dd8_32C+dd32C+Total+factor(ID) +Soy_Return +Rice_Return+ poly(t, 2, raw = TRUE)+factor(ID), data=Data)
summary(linearMod)
