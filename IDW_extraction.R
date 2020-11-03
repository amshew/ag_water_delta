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
AR_huc12<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/AR_huc12.rds")
plot(AR_huc12)

library(parallel)
num_cores <- detectCores() 

plan(multiprocess, workers = num_cores)

year=2010
var_type<-"DTW"
#IDW at HUC 12

IDW<-function(year, var_type){
  
  #--- folder names ---#
  
  folder_name <- paste0("C:/Users/obemb/OneDrive/Desktop/data/Data/well_data/Water_depth/Well_Data/Point well data/") 
  print(folder_name)
  #--- the file name of the downloaded data ---#
  file_name <- paste0("Well_depth_",var_type,"_", year,".csv") 
  print(file_name)
  #--- complete path to the downloaded files ---#
  
  Data <- read.csv(paste0( folder_name, file_name))
  head(Data) 
  # Data<-read.csv("Data")
  # head(Data)
  
  
  
  dsp <- SpatialPoints(Data[,2:1], proj4string=CRS("+proj=longlat +datum=NAD83"))
  dsp <- SpatialPointsDataFrame(dsp,Data)
  summary(dsp$well_depth)
  
  cuts <- c(0,60,100,140,200,280)
  blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
  pols <- list("sp.polygons", AR_huc12, fill = "lightgray")
  
  plot(AR_huc12)
  spplot(dsp, 'well_depth', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
  TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0")
  
  dta <- spTransform(dsp,TA)
  cata <- spTransform(AR_huc12, TA)
  
  v <- voronoi(dta)
  #plot(v)
  ca <- aggregate(cata)
  vca <-raster::intersect(v, ca)
  spplot(vca, 'well_depth', col.regions=rev(get_col_regions()))
  r <- raster(cata, res=1000) 
  vr <- rasterize(vca, r, 'well_depth')
  plot(vr)
  gs <- gstat(formula=well_depth~1, locations=dta)
  idw <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
 
  #re-extent from polygon to raster
  myExtent <- spTransform(AR_huc12, CRS(proj4string( idw)))
  show(myExtent)
  idwr <- mask(idw, vr)%>% 
    raster::extract(.,myExtent, progress = F,
                    fun=mean, df=T,weights=T,na.rm =T)%>% mutate(., year=year)
  names(idwr) <-(c('ID',paste0('DTW_', var_type),'year'))

  idwr <- idwr[c("ID", "year", paste0('DTW_', var_type)    )]
  colnames( idwr)
 
  saveRDS(
    idwr, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/IDW_',var_type, year, ".rds")
  )
  
  
  
}

IDW_append<-function(var_type){
  #append all the data
  file_name <- paste0(var_type,"_DWT") 
  panel <- paste0(var_type,"_DWT_panel") 
  file_name<-"C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW"
  DTW_panel<-list.files(file_name,
                        full.names = T,
                        pattern=".rds$")
  all.the.data <- lapply( DTW_panel,  readRDS)
  
  panel<- do.call("rbind", all.the.data)
  
  saveRDS(
    panel, 
    paste0('C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/DTW/DTW_panel/',var_type, "_DTW_panel",'.rds'))
  unlink( DTW_panel, recursive = TRUE)
  
}

future_lapply(
  2010:2019,
  function (x) IDW(x, "Fall")
)


future_lapply(
  "Fall",
  function (x)IDW_append(x)
)

future_lapply(
  2010:2019,
  function (x) IDW(x, "Spring")
)
future_lapply(
  "Spring",
  function (x)IDW_append(x)
)


future_lapply(
  2010:2019,
  function (x) IDW(x, "DWT")
)
future_lapply(
  "DWT",
  function (x) IDW_append(x)
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
