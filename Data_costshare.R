
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
library(plm)
library(lmtest)
library(sandwich)

Rice_freq<-  readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Crop_share/lu_panel_county_mon.rds')%>%
  subset( ., select = -c(County,Crop_Acre))
Rice_freq$COUNTYFP10<- as.numeric(as.character(Rice_freq$COUNTYFP10))
Data_freq<- read.csv("C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data_county_freq.csv")
Data_exp<- read.csv("C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data_county.csv")

Data<- merge(Data_exp,Rice_freq,by=c("Year","COUNTYFP10","ID"),all.x=TRUE)%>%
  subset( ., select = c(share,share_freq, Total,soybean_return_fut,rice_return_fut,Clay,ppt,mean_sT,Year,fips,county))
summary(Data)
Data<-Data%>%mutate(.,logTotal=log(Total))
 
Data$region1<- ifelse(Data$county=="Lonoke"|Data$county=="Prairie"|Data$county=="Jefferson"|Data$county=="Arkansas"|Data$county=="Phillips", 1, 0) 
Data$region2<- ifelse(Data$county=="Clay"|Data$county=="Greene"|Data$county=="Craighead"|Data$county=="Poinsett"|Data$county=="Cross"
                      |Data$county=="St. Francis"|Data$county=="Lee" |Data$county=="White"|Data$county=="Pulaski", 1, 0) 
Data$region3<- ifelse(Data$county=="Chicot"|Data$county=="Ashley"|Data$county=="Drew"|Data$county=="Desha"|   Data$county=="Lincoln"|Data$county=="Crittenden"|
                        Data$county=="Monroe"|Data$county=="Woodruff"|Data$county=="Jackson"|Data$county=="Lawrence"|Data$county=="Randolph"|Data$county=="Mississippi", 1, 0) 

Data<-Data%>%mutate(.,intertotal_region1=logTotal*region1)
                    Data<-Data%>%mutate(.,intertotal_region2=logTotal*region2)
                                        Data<-Data%>%mutate(.,intertotal_region3=logTotal*region3)
                                         Data<- within(Data, {logtotal_bar = ave(logTotal,fips,FUN=mean)} )
                                         Data<- within(Data, {rice_bar = ave(rice_return_fut,fips,FUN=mean)} )
                                         Data<- within(Data, {soybean_bar = ave(soybean_return_fut,fips,FUN=mean)} )
                                         
                                        
    summary(Data)                                    
 
    #Data<-Data[Data$Year==2017,]
write.csv(Data, file="C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/Costshare_analysis/Data_complete.csv", row.names = FALSE)

#***FE_LPM regression expansion***#
FE_LPM_expansion <- plm(share ~ logTotal+intertotal_region1+intertotal_region2+soybean_return_fut +rice_return_fut+ppt,
                 Data=Data[which(!is.na(Data$Share)), ],data=Data, model = "within",index=c("fips"))
print(summary(FE_LPM_expansion ), digits=2)
FE_LPM_expansion<-coeftest(FE_LPM_expansion , vcov=vcovHC(FE_LPM_expansion ,type="HC0",cluster="time",vcov = vcovHC))
print(summary(FE_LPM_expansion ), digits=2)
options(scipen=999)

#***Fractional regression***#

Frac_expansion <- glm(share ~ logTotal+intertotal_region1+intertotal_region2+soybean_return_fut +rice_return_fut+ppt+
                          Clay+mean_sT+logtotal_bar+rice_bar+soybean_bar,
                       data=Data, family = binomial)
summary(Frac_expansion)
Frac_expansion<-coeftest(Frac_expansion , vcov=vcovHC(Frac_expansion ,type="HC1",cluster="time",vcov = vcovHC))
Frac_expansion


#***FE_LPM regression expansion***#
#*

FE_LPM_intensity <- plm(share_freq ~ logTotal+intertotal_region1+intertotal_region2+soybean_return_fut +rice_return_fut+ppt,
                        Data=Data[which(!is.na(Data$Share)), ],data=Data, model = "within",index=c("fips"))
print(summary(FE_LPM_intensity ), digits=2)
FE_LPM_intensity<-coeftest(FE_LPM_intensity , vcov=vcovHC(FE_LPM_intensity ,type="HC0",cluster="time",vcov = vcovHC))
print(summary(FE_LPM_intensity ), digits=2)
options(scipen=999)
