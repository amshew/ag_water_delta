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
temp_start_month <- 01
temp_end_month <- 01
#--- year to work on ---#  
temp_start_year <- 2019

temp_end_year <- 2019
#--- month to work on ---#  
temp_start_month <- 07
temp_end_month <-07
#--- year to work on ---#  
temp_start_year=2019
#get shapefile for Arkansas
AR<-getData('GADM', country='USA', level=2)%>%subset(.,NAME_1=="Arkansas")
plot(AR)
start.time <- Sys.time()
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)
get_save_prism_y <- function(temp_start_year, var_type) {
  
  
  #--- starting date of the working month-year ---#
  start_date <- dmy(paste0("01/", temp_start_month, "/", temp_start_year))
  start_date 
  #--- end date ---#
  #end_date <- dmy(paste0("1/1/",  temp_start_year + 1)) - 1
  end_date <- dmy(paste0("10/", temp_end_month, "/", temp_start_year))
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
  Data<- stack(file_path)
 
  
  #--- save the stars as an rds file ---#
  saveRDS(
    Data, 
    paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_', var_type, "_y", temp_start_year, ".rds")
  )
   
  #--- delete all the downloaded files ---#
  #unlink(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/', folder_name), recursive = TRUE)
  
}

#Download ppt in mm
future_lapply(
 2019:2019,
  function (x) get_save_prism_y(x, "ppt")
)
#Download ppt in tmin
future_lapply(
  2019:2019,
  function (x) get_save_prism_y(x, "tmin")
)

#Download ppt in tmax
future_lapply(
  2019:2019,
  function (x) get_save_prism_y(x, "tmax")
)


#some plots with precipitation 
Data_ppt<-readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_ppt_y2019.rds')
#Plot precipitation for USA
Data_USA <- Data_ppt%>% 
  calc(.,sum) %>% plot()

#--- crop and plot data for Arkansas ---#
Data_AR_ppt <-Data_ppt%>%  crop(., extent(AR))%>%
  mask (., AR)
Data_AR_ppt%>% calc(.,sum) %>% plot()
Data_AR_frame<-Data_AR_ppt %>% 
    raster::extract(.,AR, progress = F,
                  fun=mean, df=T,weights=T,na.rm =T)


(
  Data_long <- pivot_longer(
    Data_AR_frame , 
    -c( ID),
    names_to = "date",
    values_to ="ppt"
  )  
)



Data_long$date <- str_remove_all( Data_long$date, "PRISM_")
Data_long$date <- str_remove_all( Data_long$date, "_stable_4kmD2_")
Data_long$date <- str_remove_all( Data_long$date, "ppt")
Data_long$date <- str_remove_all( Data_long$date, "_bil")
Data_long$date<-as.numeric(as.character(Data_long$date))
Data_long$date<- ymd(Data_long$date)
saveRDS(
  Data_long, 
  file='C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_ppt.rds')


###Degree days and temperature bins
Data_tmin<-readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_tmin_y2019.rds')
Data_tmax<-readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_tmax_y2019.rds')
var_type="tmax"
Degree_days<-function(var_type,year){
  Data<-readRDS(paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_', var_type, "_y", temp_start_year, ".rds"))
temp_stars<-Data%>% 
  #--- crop to AR ---#
  crop(., extent(AR)) %>% 
  mask (., AR)%>% 
  raster::extract(.,AR, progress = F,
                  fun=mean, df=T,weights=T,na.rm =T)


(
  Data_long <- pivot_longer(
    temp_stars , 
    -c( ID),
    names_to = "date",
    values_to = var_type
  )  
)



Data_long$date <- str_remove_all( Data_long$date, "PRISM_")
Data_long$date <- str_remove_all( Data_long$date, "_stable_4kmD2_")
Data_long$date <- str_remove_all( Data_long$date, var_type)
Data_long$date <- str_remove_all( Data_long$date, "_bil")
Data_long$date<-as.numeric(as.character(Data_long$date))
Data_long$date<- ymd(Data_long$date)
saveRDS(
  Data_long, 
paste0('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_', var_type, "_y", ".rds"))
}

Degree_days("tmax")
Degree_days("tmin")
tmin<-readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_tmin_y.rds')
tmax<-readRDS("C:/Users/obemb/OneDrive/Documents/prismtmp/PRISM_tmax_y.rds")
weather<-merge(tmax,tmin)

weather$tavg=(weather$tmax+weather$tmin)/2
ggplot(data = weather, 
       aes(x = date, y = tavg,group=date)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Average Temperature") 
#create label for negative bounds by adding Minus;

result <- foreach(b=0:35,  .combine=cbind) %do%{
  c<-b
  h<-b+1
  e<-abs(h)
  d<-abs(c)
  f<-ifelse(c < 0,paste0("minus",sep = "",d),c)
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

#Calculate the temperature bins
b=0:34
for(i in b){
  c<-b
  h<-b+1
  e<-abs(h)
  d<-abs(c)
  f<-ifelse(c < 0,paste0("minus",sep = "",d),c)
  
  weather[,paste0("bin",f,"_",e,sep="")]<-weather[,paste0("time",f,"C", sep="")]-weather[,paste0("time",e,"C", sep="")]
}
weather= select(weather,-starts_with("time"))


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#regresion

ab<- data.frame(AR@plotOrder, AR@data[["NAME_2"]])%>%as.data.frame()
colnames(ab)[1]<- "ID"
colnames(ab)[2]<- "County"
  weather[,paste0("dd","8","_","32","C", sep="")]<-weather$dday8C-weather$dday32C
  weather[,paste0("dd","32","C", sep="")]<-weather$dday32C
  weather<-weather%>%group_by(ID) %>%summarise_at(vars(tmax,tmin,tavg, dd8_32C,dd32C), mean)
   prec<-Data_long%>%group_by(ID) %>%summarise_at(vars(ppt), sum)
weather<-  merge(weather,prec)
Data_weather<-merge(weather,ab, by=c("ID" = "ID"),sort = TRUE)

Corn<- read.csv("C:/Users/obemb/Downloads/corn.csv")%>% 
  subset(., select = c(Value, County,County.ANSI,Year))
County<-Corn$County
Corn$County=str_to_title(County)
Corn<-Corn[Corn$Year== "2019",]%>%
  drop_na()
colnames(Corn)[1]<- "Yield"
Data<-merge(Data_weather,Corn)

library(plm)
library(lmtest)
library(sandwich)
linearMod <- lm(Yield ~ poly(ppt, 2, raw = TRUE)+dd8_32C+dd32C,data=Data)
summary(linearMod)
