# clear and set wd ----
rm(list=ls()) # Caution: this clears the Environment
# load packages ----
library(ggplot2)
library(scales)
library(ggmap)
library(expm)
library(extrafont)
library(httr)
library(jsonlite)
library(lubridate)
library(magick)
library(rgdal)
library(maptools)
library(spdep)
library(spdplyr)
library(tidyverse)
library(data.table)
library(cowplot)
library(foreign)
#install.packages("readstata13")
library(patchwork)
library(raster)
library(readstata13)
#install.packages("devtools")
library(devtools)
library(ggpubr)
dplyr::select
library(sf)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  dplyr, # data wrangling
  dataRetrieval, # download USGS NWIS data
  lubridate, # Date object handling
  stargazer, # regression table generation
  lfe # fast regression with many fixed effects 
)  
library(pacman)
library(cdlTools)
#**************new
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  maps, # to get county boundary data
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  stargazer, # regression table generation
  future.apply, # parallel computation
  cdlTools, # download CDL data
  rgdal, # required for cdlTools
  prism, # dow/nload PRISM data
  stringr # string manipulation
)  
setwd("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data")
list.files()
memory.limit(size=100000)

######Download CDL*******************#

(
  AR_cdl<- getCDL(5, 2010:2019,location="C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl")
)
show( AR_cdl$AR2015)
crs(AR_cdl$AR2015)
projection(crs(AR_cdl$AR2015))

#----Hardway---will rewrite a  simpler code later---#
AR2010 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2010_05.tif") 
AR2011 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2011_05.tif") 
AR2012 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2012_05.tif") 
AR2013 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2013_05.tif") 
AR2014 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2014_05.tif") 
AR2015 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2015_05.tif") 
AR2016 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2016_05.tif") 
AR2017 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2017_05.tif") 
AR2018 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2018_05.tif") 
AR2019 <-raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2019_05.tif")
library(mapview)
#*************plot#
plot(AR2010)
mapView((AR2010))

#****crop frequency/pixel count***


CDL_code<- read.csv("cdl_codes_names.csv")

duration<- data.frame(id=1:10,year=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))
str(duration)

AR=c(AR2010, AR2011,AR2012,AR2013,AR2014,AR2015,AR2016,AR2017,AR2018,AR2019)
AR


cropdata<-c()
####----downloadlanduse***#
Dipo2<-function(AR,i,b){
  
  for(i in 1:b){
    print(i)
    
    cropdata[[i]]<-freq(AR[[i]])%>% 
      #--- matrix to data.frame ---#
      data.frame(.) %>% 
      #--- find acreage ---#
      #---0.222394 is the conversion factor---#
      mutate(Acreage = count*0.222394)%>%
      #--- code ---#
      left_join(CDL_code, by=c("value" = "value")) %>%
      #--- keep only the share of rice and soy ---#
      filter(value %in% c(1, 3, 5), )  %>%
      subset( .,select=c( Crop, Acreage))%>%
      #--- long to wide ---#
      spread(Crop,Acreage)  %>%
      #---year--#
      mutate(year =2009+i) 
  }
  saveRDS(cropdata, file="C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/output.rds")
  cropdata<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/output.rds")
  Acreage<- do.call(rbind, cropdata)
  
  saveRDS(Acreage, file="C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Output/Acreage.rds")
  return(Acreage)
}

Acres<-Dipo2(AR,1,2)
Acres
gather(Acres,key="crop", value="Acreage", Corn,Soybeans,Rice)%>%
  mutate(Acreage=Acreage/1000)%>%
  ggplot(aes(year,Acreage, color = crop))+
  geom_line()+
  geom_point() +
  theme_bw() +
  
  labs(y = "Acreage (,000 Acres.) ",
       x = "Years",
       colour = "")+
  
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=seq(2010,2019,1),limits = c(2010,2019))+
  
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(colour=guide_legend(ncol=1))+
  theme(legend.justification = c(0, 1), 
        legend.position = "right",
        legend.box.margin=margin(c(0,10,0,10)))+
  theme(legend.text=element_text(size=rel(0.9)))+
  theme(legend.key.size = unit(1.5,"line"))+
  theme(axis.title  = element_text(size = 11))+
  theme(axis.text = element_text(size = 11))


#***Share of irrigated soybean under total acreage of soybean****#

Share_irr_soy<- data.frame(Share_irr_soy=c(0.736050157,	0.788888889,0.790625,	0.819571865,	0.817337461,	0.846875,	0.814376997,	0.842492918,0.830275229,0.84
),year=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))
Share_irr_soy
(Acreage_irr<- left_join(Acres,Share_irr_soy, by=c("year" = "year")) %>%
    mutate(irrigated_soybean =Soybeans*Share_irr_soy)  %>%
    dplyr::select(c(year,Corn,Soybeans,irrigated_soybean,Rice) )  %>%
    gather(key="crop", value="Acreage", Corn,Soybeans,irrigated_soybean,Rice) %>%
    mutate(Acreage=Acreage/1000) %>%
    ggplot(aes(year,Acreage, color = crop))+
    geom_line()+
    geom_point() +
    theme_bw() +
    
    labs(y = "Acreage (,000 Acres.) ",
         x = "Years",
         colour = "")+
    
    scale_y_continuous(breaks=seq(1000,4500,500),labels = scales::comma)+
    scale_x_continuous(breaks=seq(2010,2019,1),limits = c(2010,2019))+
    
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    guides(colour=guide_legend(ncol=1))+
    theme(legend.justification = c(0, 1), 
          legend.position = "right",
          legend.box.margin=margin(c(0,10,0,10)))+
    theme(legend.text=element_text(size=rel(0.9)))+
    theme(legend.key.size = unit(1.5,"line"))+
    theme(axis.title  = element_text(size = 11))+
    theme(axis.text = element_text(size = 11))+
    scale_color_manual(name="Acreage",
                       labels=c("Corn","Irrigated Soybeans","Total Soybean acreage", 'Rice'),
                       values=c("red","green","blue", "purple")                        )
)

#*********crop Rotation********#





datat1 <- raster::rasterToPoints(AR2010)
datat2 <- raster::rasterToPoints(AR2011)

datat1 <- data.table::as.data.table(datat1)
datat2 <- data.table::as.data.table(datat2)

(pixelcounts <- merge(datat1, datat2, by = c('x', 'y')) %>%
    as.data.frame() %>%
    'colnames<-'(c('x', 'y', 'value.x', 'value.y')) %>%
    dplyr::filter(value.x > 0, value.y > 0) %>%
    dplyr::group_by(value.x, value.y) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::left_join(., CDL_code, by = c('value.x' = 'value'),copy=T) %>%
    dplyr::left_join(., CDL_code, by = c('value.y' = 'value'),copy=T) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value.x, -value.y) %>%
    dplyr::rename(From = Crop.x, To = Crop.y) %>%
    dplyr::mutate(Acreage = Count*0.222394)
)





#----Trying this---#

Dipo<-function(AR,a,b){
  for (i in a:b){
  datat1<- raster::rasterToPoints(AR[[i]])
  datat2 <- raster::rasterToPoints(AR[[i+1]])
  
  datat1<- data.table::as.data.table(datat1)
  datat2 <-data.table::as.data.table(datat2)
  Dipo <- merge(datat1 , datat2 , by = c('x', 'y')) %>%
    as.data.frame() %>%
    'colnames<-'(c('x', 'y', 'value.x', 'value.y')) %>%
    dplyr::filter(value.x > 0, value.y > 0) %>%
    dplyr::group_by(value.x, value.y) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::left_join(., CDL_code, by = c('value.x' = 'value'),copy=T) %>%
    dplyr::left_join(., CDL_code, by = c('value.y' = 'value'),copy=T) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value.x, -value.y) %>%
    dplyr::rename(From = Crop.x, To = Crop.y) %>%
    dplyr::mutate(Acreage = Count*0.222394)%>%
  dplyr::mutate(CR_From =2009+i)%>%
  dplyr::mutate(CR_to=2010+i)
  write.csv(Dipo, paste(2009+i,"_", 2010+i,".csv"), row.names = FALSE)
  return(Dipo)
  }
}
Landuse<-Dipo(AR,1,1)


#Crop Rotation


AR_CDL_path<-"C:/Users/obemb/OneDrive/Desktop/data/Data/cdl"

AR=stack(AR2010,AR2011,AR2012)
AR
reclass_df<-c(0,0,0,
              1,1,1,
              2,2,2,
              3,3,3,
              4,4,4,
              5,5,5,
              6,9,0,
              10,10,10,
              11,60,0,
              61,61,61,
              62,255,0
)
reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)
chm_classified1 <- reclassify(AR,
                             reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified1[chm_classified1 == 0] <- NA
show(chm_classified1)
reclass_df<-c(1,1,
              2,2,
              3,3,
              4,4,
              5,5,
              6,NA,
              10,6,
              61,7
)
reclass_m <- matrix(reclass_df,
                    ncol = 2,
                    byrow = TRUE)
chm_classified2 <- reclassify(chm_classified1,
                             reclass_m)
show(chm_classified2)
Dipo<-freq(chm_classified)
color=c("yellow", "green", "red","pink", "blue","purple", "orange" )
plot(chm_classified2,
     col = color)


plot(chm_classified2,
     legend = FALSE,
     col = color, axes = FALSE)

legend("bottomright",
       legend = c("corn", "cotton", "Rice","Sorghum","Soybeans","Peanut","Fallow"),
       fill = color,
       border = FALSE,
       bty = "n") # turn off legend border


reclass_df<-c(1,NA,
              2,NA,
              3,3,
              4,NA,
              5,5,
              6,NA,
              7,NA
)
reclass_m <- matrix(reclass_df,
                    ncol = 2,
                    byrow = TRUE)
chm_classified3 <- reclassify(chm_classified2,
                             reclass_m)
show(chm_classified3)
color2=c( "red", "blue" )
plot(chm_classified3,
     legend = FALSE,
     col = color2, axes = FALSE)

legend("bottomright",
       legend = c( "Rice","Soybeans"),
       fill = color2,
       border = FALSE,
       bty = "n") # turn off legend border

diff<-chm_classified3[[1]]-chm_classified3[[2]]
plot(diff)
Freq_diff<-freq(diff)
color3=c( "red", 'green',"blue"  )
plot(diff,
     legend = F,
     col = color3, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotations- 2010-2011")

legend("bottomright",
       legend = c("To Rice from Soybeans",  "Continuos cropping","To Soybeans from Rice" ),
       fill = color3,
       border = FALSE,
       bty = "n") # turn off legend border
