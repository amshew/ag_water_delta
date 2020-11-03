

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

AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonkoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]

extent(AR.Delta)
plot(AR.Delta)
crs(AR.Delta)


#nlcd
library(raster)
nlcd <- raster("F:/transfer/Soil_code/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
plot(nlcd)
show(nlcd)
crs(nlcd )<-"+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" 


#soil 

require(rgdal)


soil_AR<-raster("F:/transfer/Soil_code/soils_GSSURGO_ar_3847137_01/raster/MapunitRaster_10m.tif")
plot(soil_AR)
show(soil_AR)


#re-extent from polygon to raster
myExtent <- spTransform(AR.Delta, CRS(proj4string(soil_AR)))
show(myExtent)
show(AR.Delta)
soil_Delta<-crop(soil_AR, extent(myExtent))
#soil_masked<-soil_Delta%>%  mask (., myExtent)
plot(soil_Delta)
show(soil_Delta)
library(rgeos)



#re-extent from  raster to polygon 
myExtent <- spTransform(AR.Delta, CRS(proj4string(soil_AR)))
show(myExtent)
#masknlcd

attr_table <- nlcd@data@attributes[[1]]

nlcd_Delta<-crop(nlcd, extent(myExtent))
plot(nlcd_Delta)
AR_Mask<- mask(nlcd_Delta, nlcd_Delta== 82, maskvalue = FALSE)
#writeRaster(AR_Mask, paste0("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask_county", ".tif"),  overwrite = T) 
#saveRDS(AR_Mask, file = "C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask_county.rds")
#plot(AR_Mask)
AR_Mask<-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask_county.tif") #saving the raster saves time.


extent(soil_Delta)
extent(AR_Mask)
show(AR_Mask)
show(soil_Delta)
#resampling using the nearest neighbor from 10-(soil_Delta) to 30-(nlcd) using nearest neighbor

#r.new = resample(soil_Delta ,AR_Mask ,method= 'ngb')
#writeRaster(r.new, paste0("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/r.new_county", ".tif"),  overwrite = T)
#show(r.new)

r.new<-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/r.new.tif") #saving the raster saves time.
soil<- mask(r.new, AR_Mask, maskvalue = FALSE)
show(soil)



#re-extent from  raster to polygon 
myExtent <- spTransform(AR.Delta, CRS(proj4string(soil)))
show(myExtent)
 Soil_df<-raster::extract(soil, myExtent, progress = F,
                          cellnumbers=TRUE,   df=T,na.rm =T)
 saveRDS( Soil_df, file = "C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/ Soil_df_county.rds")
 Soil_df<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/ Soil_df_county.rds")
colnames(Soil_df)
colnames(Soil_df)[3] <- "mukey"
colnames(Soil_df)[2] <- "count"
Soil_df_weigt<-Soil_df%>%group_by(ID, mukey)%>%summarise(count= n())%>%group_by(ID)%>%
  within(., {sum = ave(count,ID,FUN=sum)} )%>%mutate( .,weight=count/sum)%>%
  subset( ., select = -c(count,sum))
gssurgo_value<-read.csv("F:/transfer/Soil_code/statsgo/gssurgo_value.csv")
#statsgo
statsgo_value<-read.csv("F:/transfer/Soil_code/statsgo/mapunit_soils_data_needed.csv")

soil_value<-full_join( gssurgo_value, statsgo_value, by=c("mukey"),sort = TRUE)

soil_all<-full_join(Soil_df_weigt, soil_value, by=c("mukey"),sort = TRUE)
soil_all[is.na(soil_all)] <- 0
#reweighting the soil attributes based on weight and summarizong to HUC 12

Soil_data<-soil_all%>%mutate(across(4:93,~ . *weight))%>% group_by(ID) %>%
  summarise_each(funs(sum)) %>%
  subset( ., select = -c(mukey,objectid))
#drop the row with mukey 0
Soil_data<- Soil_data[-c(1), ]  
  
  saveRDS(
    Soil_data, 
    file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/Soil_data_county.rds')
  
Elevation<- subset( Soil_data, select =c(ID, elev))
saveRDS(
  Elevation, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/Elevation_county.rds')
