
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
AR_huc12<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12.rds")
head(AR_huc12)
crs(AR_huc12)


#--- month to work on ---#  
temp_start_month <- 01
temp_end_month <- 12
#--- year to work on ---#  
temp_start_year <- 1981

temp_end_year <- 2009


(
  start_date <- dmy(paste0("01/", temp_start_month, "/",temp_start_year))
)


(
  end_date <- dmy(paste0("31/", temp_end_month, "/", temp_start_year))
)

library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)

#--- define a function to download and save PRISM data stacked by year ---#
library(maptools)
lps <- coordinates(AR_huc12)

get_save_prism_y <- function(temp_start_year, var_type) {
  
  
  #--- starting date of the working month-year ---#
  start_date <- dmy(paste0("01/", temp_start_month, "/", temp_start_year))
  start_date 
  #--- end date ---#
  #end_date <- dmy(paste0("1/1/",  temp_start_year + 1)) - 1
  end_date <- dmy(paste0("31/", temp_end_month, "/", temp_start_year))
  
  
  
  
  #--- list of dates of the working month-year ---#
  dates_ls <- seq(start_date, end_date, "days") 
  dates_ls
  #--- remove dashes ---#
  dates_prism_txt <- str_remove_all(dates_ls, "-")
  dates_prism_txt
  get_prism_dailys(
    type = var_type,
    minDate = as.character(start_date),
    maxDate = as.character(end_date),
    keepZip = FALSE)
  
  #--- folder names ---#
  
  folder_name <- paste0("PRISM_",var_type, "_stable_4kmD2_", dates_prism_txt, "_bil") 
  #print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("PRISM_",var_type, "_stable_4kmD2_", dates_prism_txt, "_bil.bil") 
  #print(file_name)
  #--- complete path to the downloaded files ---#
  
  file_path <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/', folder_name, "/", file_name)
  file_path
  
  #--- combine all the PRISM files as a RasterStack ---#
  temp_stars <- stack(file_path) %>% 
    #--- crop to AR ---#
    crop(., extent(AR.Delta)) %>% 
    mask (., AR.Delta)%>% 
    raster::extract(.,AR.Delta, progress = F,
                    fun=mean, df=T,weights=T,na.rm =T)
  
  
  (
    tmax_long <- pivot_longer(
      temp_stars , 
      -c( ID),
      names_to = "date",
      values_to = var_type
    )  
  )
  #temp_stars <- stack(file_path) 
  
  
  tmax_long$date <- str_remove_all( tmax_long$date, "PRISM_")
  tmax_long$date <- str_remove_all( tmax_long$date, "_stable_4kmD2_")
  tmax_long$date <- str_remove_all( tmax_long$date, var_type)
  tmax_long$date <- str_remove_all( tmax_long$date, "_bil")
  tmax_long$date<-as.numeric(as.character(tmax_long$date))
  tmax_long$date<- ymd(tmax_long$date)

  
  #--- save the stars as an rds file ---#
  saveRDS(
    tmax_long, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/PRISM_', var_type, "_y", temp_start_year, ".rds")
  )
  #writeRaster(temp_stars, paste0("C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/raster/PRISM_",var_type,  temp_start_year, "_y", ".tif"),  overwrite = T) 
  
  
  #--- delete all the downloaded files ---#
  unlink(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/', folder_name), recursive = TRUE)
  
}




#--- run the above code in parallel ---#


future_lapply(
  1981:2019,
  function (x) get_save_prism_y(x, "ppt")
)





#--- run the above code in parallel ---#
future_lapply(
  1981:2019,
  function (x) get_save_prism_y(x, "tmax")
)


lal=c(2007,2009,2018)
#--- run the above code in parallel ---#
future_lapply(
 lal,
  function (x) get_save_prism_y(x, "tmin")
)

##### obtain coordinates 

#--- the file name of the downloaded data ---#
file_name <- raster("C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_ppt_stable_4kmD2_20101231_bil/PRISM_ppt_stable_4kmD2_20101231_bil.bil")
coord<-raster::extract(file_name, SpatialPoints(AR.Delta), sp = T)%>%
  data.frame(.) %>% 
  dplyr::select(coords.x1, coords.x2)
names(coord) <-(c('longitude','latitude'))
coord=cbind(raster::extract(file_name, coord, df = T),coord)%>%
  dplyr::select(ID,longitude,latitude)
saveRDS(coord, file = 'C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord.rds')

plot(coord$longitude,coord$latitude)


abab<-readRDS("C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/County/PRISM_tmin_y2009.rds")
