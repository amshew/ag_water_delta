rm(list=ls()) # Caution: this clears the Environment
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  raster, # raster data handling
  sf, # vector data handling
  dplyr, # data wrangling
  stringr, # string manipulation
  lubridate, # dates handling
  data.table, # data wrangling
  tidyr, # reshape
  tidyUSDA, # download USDA NASS data
  FedData, # download Daymet data
  daymetr, # download Daymet data
  ggplot2, # make maps
  future.apply, # parallel processing
  CropScapeR, # download CDL data
  prism, # download PRISM data
  raster,
  cdltools,
  maptools,
  parallel
  )   
  library(pacman)
setwd("C:/Users/obemb/OneDrive/Documents")
options(prism.path = '~/prismtmp')

#A.

#--- month to work on ---#  
temp_start_month <- 07
temp_end_month <- 07
#--- year to work on ---#  
var_type="ppt"
temp_start_year=2019
#get shapefile for Arkansas
AR<-getData('GADM', country='USA', level=2)%>%subset(.,NAME_1=="Arkansas")
plot(AR)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)
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
  Data<- stack(file_path)
 
  
  #--- save the stars as an rds file ---#
  saveRDS(
    Data, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_', var_type, "_y", temp_start_year, ".rds")
  )
   
  #--- delete all the downloaded files ---#
  #unlink(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/', folder_name), recursive = TRUE)
  
}

future_lapply(
 2019:2019,
  function (x) get_save_prism_y(x, "ppt")
)


Data<-readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_ppt_y2019.rds')
#Plot precipitation for USA
Data_USA <- Data%>% 
  calc(.,sum) %>% plot()

#--- crop and plot data for Arkansas ---#
Data_AR <-Data%>%  crop(., extent(AR))%>%
  mask (., AR)%>% calc(.,sum) %>% plot()


