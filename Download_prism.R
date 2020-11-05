
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
  rgal,
  foreach,
  rgeos # download PRISM data
)   

#code starts here
# the end bound
memory.limit(size=1000000)


#basis for data download from Prism
#--- month to work on ---#  
temp_start_month <- 01
temp_end_month <- 12
#--- year to work on ---#  
temp_start_year <-1980

temp_end_year <- 2019



setwd("E:/Prism_data")
options(prism.path = 'C:/Users/obemb/OneDrive - Kansas State University/Obembe/prism')

library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)
#getting the weather data from prism
## Not run:
get_save_prism_y <- function(temp_start_year, var_type) {
  
  
  #--- starting date of the working month-year ---#
  start_date <- dmy(paste0("01/", temp_start_month, "/", temp_start_year))
  start_date 
  #--- end date ---#
  # end_date <- dmy(paste0("01/", temp_end_month/,  temp_start_year + 1)) - 1
  end_date <- dmy(paste0("31/", temp_end_month, "/", temp_start_year))
  end_date 
  
  
  
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
  
 
} 


#--- run the above code in parallel ---#
future_lapply(
 1981:2019,
  function (x) get_save_prism_y(x, "tmax")
)



#--- run the above code in parallel ---#
future_lapply(
  1981:2019,
  function (x) get_save_prism_y(x, "tmin")
)  


#--- run the above code in parallel ---#
future_lapply(
  1981:2019,
  function (x) get_save_prism_y(x, "ppt")
)