
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
memory.limit(size=1000000)

setwd("C:/Users/obemb/OneDrive/Desktop/data/hydrologic_units_WBDHU8_ar_2521302_01/hydrologic_units")
list.files()
loadfonts(quiet = T)


County<-read.csv("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Return/county_fips.csv")
library("readxl")
IWM_costshare<- read_excel( "C:/Users/obemb/OneDrive/Desktop/data/Data/practices/raw_REAP_Req199_20_AR Program Summary FY04 to FY19.xlsx" )
#subset IWM practices
#IWM_costshare_subset=subset(IWM_costshare,practice_code=="436"&contract_status=="Completed" )
IWM_costshare_subset=subset(IWM_costshare, practice_code=="449"|practice_code=="464"|practice_code=="447"|practice_code=="442"|practice_code=="430DD"|practice_code=="118" 
                              |practice_code=="436"  |practice_code=="441"  |practice_code=="443"|practice_code=="430"|practice_code=="552"|practice_code=="642"|practice_code=="443"  
                              |practice_code=="E449114Z7"| practice_code=="342"| practice_code=="484"| practice_code=="533"| practice_code=="587"|practice_code=="607"|practice_code=="608"
                              |practice_code=="574"|practice_code=="554"|practice_code=="557"|practice_code=="410")
colnames(IWM_costshare_subset)[5] <- "HUC12"
colnames(IWM_costshare_subset)[15] <- "practice_amount"
colnames(IWM_costshare_subset)[17] <- "PracticeStatus"
colnames(IWM_costshare_subset)[18] <- "AppliedDate"
colnames(IWM_costshare_subset)[3] <- "Year"
#IWM_costshare_subset=subset(IWM_costshare_subset,PracticeStatus=="Certified"|PracticeStatus==" Partial Certified" )
#IWM_costshare_subset$Year1 <-as.numeric( format(as.Date(IWM_costshare_subset$AppliedDate), format = "%Y"))
colnames(IWM_costshare_subset)
IWM_costshare_subset=subset(IWM_costshare_subset,contract_status=="Completed" )

#IWM_costshare<-left_join(coord_sf ,IWM_costshare_subset, by=c("HUC12" = "HUC12"), keep = F)
IWM_costshare_subset$cost_share[is.na(IWM_costshare_subset$cost_share)] <- 0
IWM_costshare_subset<-IWM_costshare_subset[!(IWM_costshare_subset$cost_share ==0),]
IWM_costshare_merged<-IWM_costshare_subset%>%
  subset(., select = c(Year,component_name,County,practice_name,practice_code, 
                       contract_id,practice_amount,cost_share,latitude,longitude))%>%
  merge(.,County, by=c("County", "Year"="Year"),sort = TRUE)

#sum of cost share over practice in huc
IWM_costshare_sum_practice=IWM_costshare_merged%>%group_by(County,Year,practice_code,fips,region,COUNTYFP10 )%>%summarise(Practice_cost = sum(cost_share))
colnames(IWM_costshare_sum_practice)
#sum of cost share over farm
IWM_costshare_sum_farm=IWM_costshare_merged%>%group_by(County,contract_id,fips,region,COUNTYFP10 )%>%summarise(Practice_cost = sum(cost_share))
saveRDS(
  IWM_costshare_sum_farm, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/IWM/IWM_cost_farm.rds')
#Total cost share per Huc
IWM_costshare_sum=IWM_costshare_sum_practice %>%group_by(County,Year) %>%summarise(Total = sum(Practice_cost))


IWM_cost<-merge(IWM_costshare_sum,County, by=c("County", "Year"="Year"),sort = TRUE)
IWM_cost=IWM_cost[IWM_cost$Year >= "2006" &IWM_cost$Year<= "2019",]
IWM_cost$COUNTYFP10<- as.numeric(as.character(IWM_cost$COUNTYFP10))
saveRDS(
  IWM_cost, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/IWM/IWM_cost_county.rds')

write.csv(IWM_cost, file="C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data_county_subsidy.csv", row.names = FALSE)
colnames(IWM_cost)


###***Dont run
###########Bundles#############

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
memory.limit(size=1000000)

setwd("C:/Users/obemb/OneDrive/Desktop/data/hydrologic_units_WBDHU8_ar_2521302_01/hydrologic_units")
list.files()
loadfonts(quiet = T)


County<-read.csv("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Return/county_fips.csv")
library("readxl")
IWM_costshare<- read_excel( "C:/Users/obemb/OneDrive/Desktop/data/Data/practices/raw_REAP_Req199_20_AR Program Summary FY04 to FY19.xlsx" )
#subset IWM practices
#IWM_costshare_subset=subset(IWM_costshare,practice_code=="436"&contract_status=="Completed" )
IWM_costshare_subset=subset(IWM_costshare, practice_code=="436"|practice_code=="342"|practice_code=="484")
colnames(IWM_costshare_subset)[5] <- "HUC12"
colnames(IWM_costshare_subset)[15] <- "practice_amount"
colnames(IWM_costshare_subset)[17] <- "PracticeStatus"
colnames(IWM_costshare_subset)[18] <- "AppliedDate"
colnames(IWM_costshare_subset)[3] <- "Year"
#IWM_costshare_subset=subset(IWM_costshare_subset,PracticeStatus=="Certified"|PracticeStatus==" Partial Certified" )
#IWM_costshare_subset$Year1 <-as.numeric( format(as.Date(IWM_costshare_subset$AppliedDate), format = "%Y"))
colnames(IWM_costshare_subset)
IWM_costshare_subset=subset(IWM_costshare_subset,contract_status=="Completed" )

#IWM_costshare<-left_join(coord_sf ,IWM_costshare_subset, by=c("HUC12" = "HUC12"), keep = F)
IWM_costshare_subset$cost_share[is.na(IWM_costshare_subset$cost_share)] <- 0
IWM_costshare_subset<-IWM_costshare_subset[!(IWM_costshare_subset$cost_share ==0),]
IWM_costshare_merged<-IWM_costshare_subset%>%
  subset(., select = c(Year,component_name,County,practice_name,practice_code, 
                       contract_id,practice_amount,cost_share,latitude,longitude))



#sum of cost share over practice in huc
IWM_costshare_sum_practice=IWM_costshare_merged%>%group_by(County,Year,practice_code)%>%summarise(Practice_cost = sum(cost_share))

colnames(IWM_costshare_sum_practice)
#Total cost share per Huc
IWM_costshare_sum=IWM_costshare_sum_practice %>%group_by(County,Year) %>%summarise(Total = sum(Practice_cost))


IWM_cost<-merge(IWM_costshare_sum,County, by=c("County", "Year"="Year"),sort = TRUE)
IWM_cost=IWM_cost[IWM_cost$Year >= "2006" &IWM_cost$Year<= "2019",]
IWM_cost$COUNTYFP10<- as.numeric(as.character(IWM_cost$COUNTYFP10))
saveRDS(
  IWM_cost, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/IWM/IWM_cost_county_subsidy_bundle.rds')

write.csv(IWM_cost, file="C:/Users/obemb/OneDrive/Desktop/data/Code/STATA/prices/Data_county_subsidy_bundle.csv", row.names = FALSE)
colnames(IWM_cost)


