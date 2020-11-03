
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

setwd("C:/Users/obemb/OneDrive/Desktop/data/hydrologic_units_WBDHU8_ar_2521302_01/hydrologic_units")
list.files()
loadfonts(quiet = T)

coord_sf <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord_sf.rds')

library("readxl")
IWM_costshare<- read_excel( "C:/Users/obemb/OneDrive/Desktop/data/Data/practices/raw_REAP_Req199_20_AR Program Summary FY04 to FY19.xlsx" )
#subset IWM practices
IWM_costshare_subset=subset(IWM_costshare, practice_code=="449"|practice_code=="464"|practice_code=="447"|practice_code=="442"|practice_code=="430DD"|practice_code=="118" 
                            |practice_code=="436"  |practice_code=="441"  |practice_code=="443"|practice_code=="430"|practice_code=="552"|practice_code=="642"|practice_code=="443" 
                            |practice_code=="E449114Z7"| practice_code=="342"| practice_code=="484"| practice_code=="533"| practice_code=="587"|
                              practice_code=="554"|practice_code=="557"|practice_code=="587"|practice_code=="607"|practice_code=="608")
colnames(IWM_costshare_subset)[5] <- "HUC12"
colnames(IWM_costshare_subset)[15] <- "practice_amount"
colnames(IWM_costshare_subset)[3] <- "Year"
colnames(IWM_costshare_subset)
#IWM_costshare<-left_join(coord_sf ,IWM_costshare_subset, by=c("HUC12" = "HUC12"), keep = F)
IWM_costshare_subset$cost_share[is.na(IWM_costshare_subset$cost_share)] <- 0
IWM_costshare_merged<-merge(coord_sf ,IWM_costshare_subset, by=c("HUC12" = "HUC12"),sort = TRUE)%>%
  subset(., select = c(HUC12,Year,ID,component_name,County,practice_name,practice_code, contract_id,practice_amount,qty,unit_cost,cost_share,unit_type))

colnames(IWM_costshare_merged)

#sum of cost share over practice in huc
IWM_costshare_sum_practice=IWM_costshare_merged%>%group_by(County,Year,practice_code)%>%
  within(., {sum_huc12 = ave(cost_share,County,Year,practice_code,FUN=sum)} )
colnames(IWM_costshare_sum_practice)
#Total cost share per Huc
IWM_costshare_sum=IWM_costshare_sum_practice %>%group_by(County,Year) %>%
  summarise_each(funs(sum),Total=sum_huc12) 
County<-IWM_costshare_sum$County
IWM_costshare_sum$County=str_to_title(County)
coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord_AD.rds')
colnames(coord)[3] <- "County"
IWM_cost<-merge(coord,IWM_costshare_sum, by=c("County"),sort = TRUE)
saveRDS(
  IWM_cost, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/IWM/IWM_cost_county.rds')
colnames(IWM_cost)









AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonkoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]
#dissolve shapefile
#AR.Delta_d<-aggregate(AR.Delta, dissolve = TRUE)
plot(AR.Delta)
head(AR.Delta)
AR.Delta.tbl <- fortify(AR.Delta , region = 'COUNTYFP10')
AR.Delta$COUNTYFP10 <- as.numeric(AR.Delta$COUNTYFP10 )


library(gganimate)
require(magick)
(aa=ggplot(data=AR)+geom_sf(aes(x=long,y=lat)))
library(gifski)
animate(aa, nframes = 12, duration=10,renderer = gifski_renderer("C:/Users/obemb/OneDrive/Desktop/data/Data/practices/gganim/gganim.gif"))
animate(aa,nframes = 12, device = "png",
        renderer = file_renderer("C:/Users/obemb/OneDrive/Desktop/data/Data/practices/gganim", prefix = "gganim_plot", overwrite = TRUE))







