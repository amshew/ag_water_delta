rm(list=ls()) # Caution: this clears the Environment

library(raster)
# clear and set wd ----
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
  prism,# download PRISM data
  rgal,
  rspatial
)   
library(pacman)
library(cdlTools)
library(prism)
library(rgdal)
library(raster)

#Inverse distance weighted
library(rgdal)
library(gstat)
library(sp)
library(dismo)
library(deldir)
AR_Mask <-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_Mask.rds")%>%
  plot()
AR<- readOGR("C:/Users/obemb/OneDrive/Desktop/data/tl_2010_05_county10/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]
#dissolve shapefile
#AR.Delta_d<-aggregate(AR.Delta, dissolve = TRUE)
plot(AR.Delta)
Well_reading<-read_xlsx("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Alluvial_siteWI.xlsx")%>%subset( ., select = -c(Agency_Cod,Site_Numbe,Station_Na,Measurem_1, WL_Time_Da, WL_Time__1, Level_Type,
                          WL_Below_M,WLBMP_Sequ,WL_Above_S,WLASL_Datu,WL_Accurac,WL_Status,Equipment,
                          WL_Method,WL_Statist,WL_Source,WL_Party__,WL_Sourc_1,Level_Web,Remarks__C))


colnames(Well_reading)
colnames(Well_reading)[1]<-"latitude"
colnames(Well_reading)[2]<-"longitude"
colnames(Well_reading)[4]<-"well_depth"
colnames(Well_reading)[3]<-"date"
Well_reading$Year <-as.numeric( format(as.Date(Well_reading$date), format = "%Y"))
Well_reading$month <- as.numeric(format(as.Date(Well_reading$date), format = "%m"))
Well_reading<-Well_reading%>%mutate(.,season=ifelse(month<7,"Spring","Fall"))
saveRDS(
  Well_reading, 
  file='C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/Well_reading.rds')


library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)

year=2010
var_type<-"Spring"
#IDW at HUC 12

IDW<-function(year, var_type){

  Data<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/Well_reading.rds')
  #head(Data) 
  Data<-subset(Data, Year==year &season==var_type)%>%subset( ., select = -c(Year,date,month,season))
  #head(Data) 
  
  
  
  dsp <- SpatialPoints(Data[,2:1], proj4string=CRS("+proj=longlat +datum=NAD83"))
  dsp <- SpatialPointsDataFrame(dsp,Data)
  summary(dsp$well_depth)
  
  cuts <- c(0,60,100,140,200,280)
  blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
  pols <- list("sp.polygons", AR.Delta, fill = "lightgray")
  
  plot(AR.Delta)
  spplot(dsp, 'well_depth', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80 +towgs84=0,0,0")
  
  dta <- spTransform(dsp,TA)
  cata <- spTransform(AR.Delta, TA)
  
  v <- voronoi(dta)
  #plot(v)
  ca <- aggregate(cata)
  vca <-raster::intersect(v, ca)
  spplot(vca, 'well_depth', col.regions=rev(get_col_regions()))
  r <- raster(cata, res=1) 
  vr <- rasterize(vca, r,field=vca@data[["well_depth"]])
  plot(vr)
  gs <- gstat(formula=well_depth~1, locations=dta)
  idw <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
  
  #re-extent from polygon to raster
  myExtent <- spTransform(AR.Delta, CRS(proj4string( idw)))
  #show(myExtent)
  idwr <- mask(idw, vr)
  Data_extract<-data.frame(raster::extract(  idwr,myExtent, 
                                            progress = F,
                                            fun=mean,   sp=T,na.rm =T))%>%  
    subset( ., select = -c(COUNTYNS10, GEOID10,NAME10,NAMELSAD10, LSAD10,
                           CLASSFP10, MTFCC10, CSAFP10, CBSAFP10, METDIVFP10,FUNCSTAT10,ALAND10,
                           AWATER10,  INTPTLAT10,   INTPTLON10))%>% mutate(., year=year)%>%
    subset( ., select = -c(STATEFP10))
  head(Data_extract)
  
  names(  Data_extract) <-(c('COUNTYFP10',paste0('DTW_', var_type),'year'))
 
  
  Data_extract <-   Data_extract[c('COUNTYFP10',paste0('DTW_', var_type),'year'    )]
  colnames(  Data_extract) 
  
  saveRDS(
    Data_extract, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/County/IDW_',var_type, year, ".rds")
  )
  
  
  
}

IDW_append<-function(var_type){
  #append all the data
  file_name <- paste0(var_type,"_DWT") 
  panel <- paste0(var_type,"_DWT_panel") 
  file_name<-'C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/County'
  DTW_panel<-list.files(file_name,
                        full.names = T,
                        pattern=".rds$")
  all.the.data <- lapply( DTW_panel,  readRDS)
  
  panel<- do.call("rbind", all.the.data)
  
  saveRDS(
    panel, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/',var_type, "_DTW_panel",'.rds'))
  unlink( DTW_panel, recursive = TRUE)
  
}

future_lapply(
  1990:2019,
  function (x) IDW(x, "Fall")
)


future_lapply(
  "Fall",
  function (x)IDW_append(x)
)

future_lapply(
  1990:2019,
  function (x) IDW(x, "Spring")
)
future_lapply(
  "Spring",
  function (x)IDW_append(x)
)


Fall<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/Fall_DTW_panel.rds')
Spring<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/Spring_DTW_panel.rds')
Diff<-readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/DWT_DTW_panel.rds')
#coord <- readRDS('C:/Users/obemb/OneDrive/Documents/prismtmp/Weather Data/coord.rds')
colnames(Diff)[3] <-"diff_DTW"    
colnames(Diff)
Fall_spring<-merge(Fall, Spring, by=c("ID"),sort = TRUE)

DTW<- merge( Fall_spring, Diff, by=c("ID"),sort = TRUE)

saveRDS(
  DTW, 
  paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/DTW.rds'))
ab=readRDS('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/County/IDW_Fall2010.rds')
