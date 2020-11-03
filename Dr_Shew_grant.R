
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
year=2012

library( foreach)
#code starts here
# the end bound
#code starts here
# the end bound

library( foreach)
#Get shapefile to extract dat for Colby
KS<- readOGR("C:/Users/obemb/OneDrive - Kansas State University/WaterUse/shapefile/KSshapes.shp",
             stringsAsFactors = FALSE)
plot(KS)
nestates <-c("Thomas")
KS_colby <- KS[as.character(KS@data$NAME) %in% nestates, ]

head(KS_colby)
crs(KS_colby)
plot(KS_colby)
#basis for data download from Prism
#--- month to work on ---#  
temp_start_month <- 01
temp_end_month <- 07
#--- year to work on ---#  
temp_start_year <- 2000

temp_end_year <- 2019



setwd("C:/Users/obemb/OneDrive/Documents")
options(prism.path = '~/prismtmp')

library(parallel)
num_cores <- detectCores() 
var_type="tmax"
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
    crop(., extent(KS_colby)) %>% 
        raster::extract(.,KS_colby, progress = F,
                    fun=max, df=T,weights=F,na.rm =T)
  
  
  (
    tmax_long <- pivot_longer(
      temp_stars , 
      -c( ID),
      names_to = "date",
      values_to = var_type
    )  
  )
  temp_stars <- stack(file_path) 
  
  
  tmax_long$date <- str_remove_all( tmax_long$date, "PRISM_")
  tmax_long$date <- str_remove_all( tmax_long$date, "_stable_4kmD2_")
  tmax_long$date <- str_remove_all( tmax_long$date, var_type)
  tmax_long$date <- str_remove_all( tmax_long$date, "_bil")
  tmax_long$date<-as.numeric(as.character(tmax_long$date))
  tmax_long$date<- ymd(tmax_long$date)
  
  
  #--- save the stars as an rds file ---#
  saveRDS(
    tmax_long, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/PRISM_', var_type, "_y", temp_start_year, ".rds")
  )
  #writeRaster(temp_stars, paste0("C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/raster/PRISM_",var_type,  temp_start_year, "_y", ".tif"),  overwrite = T) 
  
  
  #--- delete all the downloaded files ---#
  unlink(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/', folder_name), recursive = TRUE)
} 


#--- run the above code in parallel ---#
future_lapply(
 2000:2002,
  function (x) get_save_prism_y(x, "tmax")
)



#--- run the above code in parallel ---#
future_lapply(
2000:2019,
  function (x) get_save_prism_y(x, "tmin")
)  
## End(Not run)
start.time <- Sys.time()

###Calculate the degree days, exposure and temperature bins
###Get coordinates

###Getting coordinates
file_name <- raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/PRISM_ppt_stable_4kmD2_19850101_bil/PRISM_ppt_stable_4kmD2_19850101_bil.bil")

ab<-KS_colby@data <- data.frame(KS_colby@data,KS_colby@plotOrder,raster::extract(file_name, SpatialPoints(KS_colby), sp = T))%>%as.data.frame()


colnames(ab)
colnames(ab)[60] <- "longitude"
colnames(ab)[61] <-"latitude"
colnames(ab)[58] <-"ID"
colnames(ab)[5] <-"COUNTYFP10"
ab=subset(ab, select = c(ID,COUNTYFP10,NAME,latitude,longitude))
saveRDS(ab, file = 'C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/coord_AD.rds')


###Calculate the degree days, exposure and temperature bins
threshold=34
setwd("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output")

gdd<-function(year){
  
  
  file_name1 <- paste0("PRISM_", "tmax", "_y",year,".rds")  
  file_name2 <- paste0("PRISM_", "tmin", "_y", year,".rds") 
  #file_name3 <- paste0("PRISM_", "ppt", "_y", year,".rds") 
  coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/coord_AD.rds')
  #print(file_name1)
  # print(file_name2)
  #print(file_name3)
  head(coord)
  #--- complete path to the downloaded files ---#
  
  file_path1 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas',  "/", file_name1)
  #print(file_path1)
  file_path2 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas',  "/", file_name2)
  #print(file_path2)
  #file_path3 <- paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas',  "/", file_name3)
  #print(file_path3)
  tmax <- readRDS(file_path1)
  tmin <- readRDS(file_path2)
  #ppt <- readRDS(file_path3)
  temp_coord<-merge(coord, tmax, by=c("ID"),sort = TRUE)
 
  weather<- merge( temp_coord,  tmin, by=c("date","ID"),sort = TRUE)
  #weather<- merge( temp, ppt, by=c("date","ID"),sort = TRUE)
  
  weather$tavg=(weather$tmax+weather$tmin)/2
  #create label for negative bounds by adding Minus;
  
  result <- foreach(b=16:threshold,  .combine=cbind) %do%{
    c<-b
    h<-b+1
    e<-abs(h)
    d<-abs(c)
    f<-ifelse(c < 0,paste0("minus",sep = "",d),c)
    #case 1
    weather[,paste0("dday",f,"C", sep="")] <-0
    weather[,paste0("time",f,"C", sep="")] <-0
    case_dd<- weather$tavg - b
    # case 2: bound <= tMin
    weather[,paste0("dday",f,"C", sep="")] <- ifelse( b <=  weather$tmin, case_dd , 0)
    weather$tempSave = acos( (2*b-weather$tmax-weather$tmin)/(weather$tmax-weather$tmin) )
    weather[,paste0("time",f,"C", sep="")] <-ifelse( b<=  weather$tmin,1, 0)
    weather[,paste0("dday",f,"C", sep="")] <-ifelse (weather$tmin < b & b <  weather$tmax,((  weather$tavg-b)* weather$tempSave + ( weather$tmax- weather$tmin)*sin( weather$tempSave)/2)/pi, weather$tavg - b)
    weather[,paste0("time",f,"C", sep="")] <-ifelse ( weather$tmin< b & b <  weather$tmax,(acos( (2*b-weather$tmax-weather$tmin)/(weather$tmax-weather$tmin) )/pi), 1)
    
    
    weather= subset( weather, select = -c(tempSave) ) }
  
  saveRDS(
    weather, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/Degree Days/degreedays_time_', "y", year, ".rds")
  )
  
  
}

future_lapply(
  2000:2019,
  function (x) gdd (x)
)

#calculate the total number hours crop is exposed to different temperature thresholds

g=c(1,5,9,13,17)

total_exposure<-function(year){
  weather<-  readRDS(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/Degree Days/degreedays_time_', "y", year, ".rds"))
  
    weather$Year <-as.numeric( format(as.Date(weather$date), format = "%Y"))
       weather$month <- as.numeric(format(as.Date( weather$date), format = "%m"))
       weather<-subset(weather, month>=2 & month<=6)
       weather=select(weather,-starts_with("dday")) %>% subset( ., select = -c(date,tmax,tmin,ID,tavg))
    result <- foreach(n=g,  .combine=cbind) %do%{
       
   
 
    weather[ ,paste0("above",n+15)]=as.numeric(apply(weather[,n:19], 1, sum))
 }
  weather= select(weather,-starts_with("time")) 

  weather=
  saveRDS(
    weather, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/Degree Days/exposure/degreedays_', "y", year, ".rds")
  )
  #unlink(  paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/Degree Days/degreedays_time_', "y", year, ".rds"), recursive = TRUE)
}
future_lapply(
  2000:2019,
  function (x) total_exposure (x)
)
#Append the data together
exposure<-'C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/Degree Days/exposure'
exposure_panel<-list.files(exposure,
                        full.names = T,
                        pattern=".rds$")
all.the.data <- lapply( exposure_panel,  readRDS)

times_panel<- do.call("rbind", all.the.data)



Colby<-times_panel%>%group_by(Year) %>%summarise_at(vars(above16,above20,above24,above28,above32), sum)
saveRDS(
  Colby, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/Kansas_exposure.rds')


ggplot( Colby, aes(Year)) + 
  geom_line(aes(y = above16, colour = "Above 16C")) + 
  geom_line(aes(y = above20, colour = "Above 20C"))+ 
  geom_line(aes(y = above24, colour = "Above 24C"))+
  geom_line(aes(y = above28, colour = "Above 28C"))+
  geom_line(aes(y = above32, colour = "Above 32C"))+
  ylab("Total Time spent (Hours)") + xlab(" Year")+
  scale_colour_discrete("Temperature")
  
  
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#bins
b=16:32
year=2000:2019
for(year in year){
  for(i in b){
    c<-b
    h<-b+2
    e<-abs(h)
    d<-abs(c)
    f<-ifelse(c < 0,paste0("minus",sep = "",d),c)
    weather<-  readRDS(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/Degree Days/degreedays_time_', "y", year, ".rds"))
    
    weather[,paste0("bin",f,"_",e,sep="")]<-weather[,paste0("time",f,"C", sep="")]-weather[,paste0("time",e,"C", sep="")]
  }
  weather= select(weather,-starts_with("time"))
  weather$Year <-as.numeric( format(as.Date(weather$date), format = "%Y"))
  weather$month <- as.numeric(format(as.Date( weather$date), format = "%m"))
  weather<-subset(weather, month>=2 & month<=6)
  weather=select(weather,-starts_with("dday")) %>% subset( ., select = -c(date,tmax,tmin,ID,tavg))
  saveRDS(
    weather, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/degree days/bin/bin_', "y", year, ".rds")
  )
  unlink(  paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/degree days/degreedays_time_', "y", year, ".rds"), recursive = TRUE)
}

bin<-'C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/degree days/bin'
bin_panel<-list.files(bin,
                           full.names = T,
                           pattern=".rds$")
all.the.data <- lapply( bin_panel,  readRDS)

bin_panel<- do.call("rbind", all.the.data)



Colby_bin<-bin_panel%>%group_by(Year)  %>%
  summarise(across(starts_with("bin"),sum))
saveRDS(
  Colby_bin, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/SW_Raw_data/Kansas_bin.rds')

aba<-readRDS("C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/Kansas/degree days/bin_y2000.rds")
