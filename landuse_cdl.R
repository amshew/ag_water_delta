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
     main = "Rice-Soybeans Rotation- 2010-2011")

legend("bottomright",
       legend = c("To Rice from Soybeans",  "Continuous cropping","To Soybeans from Rice" ),
       fill = color3,
       border = FALSE,
       bty = "n") # turn off legend border


#Overlay
#2010
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
chm_classified2010_1 <- reclassify(AR2010,
                              reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2010_1[chm_classified2010_1 == 0] <- NA
show(chm_classified2010_1)
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
chm_classified2010_2<- reclassify(chm_classified2010_1,
                              reclass_m)
show(chm_classified2010_2)


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
chm_classified2010_3 <- reclassify(chm_classified2010_2,
                              reclass_m)
show(chm_classified2010_3)

#2011
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
chm_classified2011_1 <- reclassify(AR2011,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2011_1[chm_classified2011_1 == 0] <- NA
show(chm_classified2011_1)
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
chm_classified2011_2<- reclassify(chm_classified2011_1,
                                  reclass_m)
show(chm_classified2011_2)


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
chm_classified2011_3 <- reclassify(chm_classified2011_2,
                                   reclass_m)
show(chm_classified2011_3)

#2012

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
chm_classified2012_1 <- reclassify(AR2012,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2012_1[chm_classified2012_1 == 0] <- NA
show(chm_classified2012_1)
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
chm_classified2012_2<- reclassify(chm_classified2012_1,
                                  reclass_m)
show(chm_classified2012_2)


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
chm_classified2012_3 <- reclassify(chm_classified2012_2,
                                   reclass_m)
show(chm_classified2012_3)

#2013


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
chm_classified2013_1 <- reclassify(AR2013,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2013_1[chm_classified2013_1 == 0] <- NA
show(chm_classified2013_1)
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
chm_classified2013_2<- reclassify(chm_classified2013_1,
                                  reclass_m)
show(chm_classified2013_2)


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
chm_classified2013_3 <- reclassify(chm_classified2013_2,
                                   reclass_m)
show(chm_classified2013_3)

#2014


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
chm_classified2014_1 <- reclassify(AR2014,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2014_1[chm_classified2014_1 == 0] <- NA
show(chm_classified2014_1)
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
chm_classified2014_2<- reclassify(chm_classified2014_1,
                                  reclass_m)
show(chm_classified2014_2)


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
chm_classified2014_3 <- reclassify(chm_classified2014_2,
                                   reclass_m)
show(chm_classified2014_3)

#2015


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
chm_classified2015_1 <- reclassify(AR2015,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2015_1[chm_classified2015_1 == 0] <- NA
show(chm_classified2015_1)
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
chm_classified2015_2<- reclassify(chm_classified2015_1,
                                  reclass_m)
show(chm_classified2015_2)


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
chm_classified2015_3 <- reclassify(chm_classified2015_2,
                                   reclass_m)
show(chm_classified2015_3)
#2016
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
chm_classified2016_1 <- reclassify(AR2016,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2016_1[chm_classified2016_1 == 0] <- NA
show(chm_classified2016_1)
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
chm_classified2016_2<- reclassify(chm_classified2016_1,
                                  reclass_m)
show(chm_classified2016_2)


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
chm_classified2016_3 <- reclassify(chm_classified2016_2,
                                   reclass_m)
show(chm_classified2016_3)

#2017
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
chm_classified2017_1 <- reclassify(AR2017,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2017_1[chm_classified2017_1 == 0] <- NA
show(chm_classified2017_1)
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
chm_classified2017_2<- reclassify(chm_classified2017_1,
                                  reclass_m)
show(chm_classified2017_2)


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
chm_classified2017_3 <- reclassify(chm_classified2017_2,
                                   reclass_m)
show(chm_classified2017_3)

#2018

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
chm_classified2018_1 <- reclassify(AR2018,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2018_1[chm_classified2018_1 == 0] <- NA
show(chm_classified2018_1)
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
chm_classified2018_2<- reclassify(chm_classified2018_1,
                                  reclass_m)
show(chm_classified2018_2)


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
chm_classified2018_3 <- reclassify(chm_classified2018_2,
                                   reclass_m)
show(chm_classified2018_3)

#2019


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
chm_classified2019_1 <- reclassify(AR2019,
                                   reclass_m)
# assign all pixels that equal 0 to NA or no data value
chm_classified2019_1[chm_classified2019_1 == 0] <- NA
show(chm_classified2019_1)
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
chm_classified2019_2<- reclassify(chm_classified2019_1,
                                  reclass_m)
show(chm_classified2019_2)


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
chm_classified2019_3 <- reclassify(chm_classified2019_2,
                                   reclass_m)
show(chm_classified2019_3)


#2010to2019
Dipo<-function(x,y){ifelse(x>y,1, ifelse(x<y,-1,ifelse(x==y, x,y)))}

Diff<-overlay(chm_classified2010_3,chm_classified2011_3,fun=Dipo)
plot(Diff)



color3=c( "red","blue", 'green','yellow'  )
plot(Diff,
     legend = F,
     col = color3, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2010-2011")

legend("bottomright",
       legend = c("Rice to Soybeans","Soybeans to Rice",  "Rice Monocropping",  "Soybeans Monocropping"),
       fill = color3,
       border = FALSE,
       bty = "n") # turn off legend border

#2011to2012
#Dipo<-function(x,y){ifelse(x>y,x-y, ifelse(x<y,x-y,ifelse(x==y, x,y)))}
Dipo<-function(x,y){ifelse(x>y,1, ifelse(x<y,-1,ifelse(x==y, x,y)))}

Diff<-overlay(chm_classified2011_3,chm_classified2012_3,fun=Dipo)
plot(Diff)




color3=c( "red","blue", 'green','yellow'  )
plot(Diff,
     legend = F,
     col = color3, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2011-2012")

legend("bottomright",
       legend = c("Rice to Soybeans","Soybeans to Rice",  "Rice Monocropping",  "Soybeans Monocropping"),
       fill = color3,
       border = FALSE,
       bty = "n") # turn off legend border


mapview(Diff)


#2010to2012-CODE for rotation
#-1-r-s-r
#1-s-r-s
#2-s-s-r
#-2- r-r-s
#-3-r-s-s
#3-s-s-r
#3-rice
#5-S
#0rotation to other crops

Dipo<-function(x,y,z){ifelse(x==y & y==z,x,
                             ifelse(x==z& x<y,-1,ifelse(x==z& x>y,1,
                                              ifelse(x==y& x>z, 2,
                                                           ifelse(x==y& x<z,-2,ifelse(z==y& x>z, 3,
                                                                        ifelse(z==y& x<z, -3,0)))))))}


Diff2010_2012<-overlay(chm_classified2010_3,chm_classified2011_3,chm_classified2012_3,fun=Dipo)

TETE<-freq(Diff2010_2012)
color=c("yellow", "green","purple" ,"pink","orange" ,"red", "blue" )
plot(Diff2010_2012,
     legend = F,
     col = color, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2010-2012",cex.main=0.85, adj = 0., line=-0.5)

legend("bottomright",
       legend = c("R-S-S","R-R-S",  "R-S-R",  "S-R-S", "S-S-R","Rice Monocropping", "Soybeans Monocropping"),
       fill = color,
       border = FALSE,
       bty = "n") # turn off legend border
mapview(Diff)

#2010to2012-CODE for rotation
#-1-r-s-r
#1-s-r-s
#2-s-s-r
#-2- r-r-s
#-3-r-s-s
#3-s-s-r
#3-rice
#5-S
#0-rotation to other crops

#water use per pixel-convert each pixel to water use.

Rice<-2.13255*0.222394 #converts to ac/ft of water per pixel
  Soybeans<-1.79134*0.222394 #converts to ac/ft of water per pixel

Dipo<-function(x){ifelse(x==3,Rice,
                             ifelse(x==5, Soybeans,0))}
Dipo(5)
chm_classified2010_3
#water use per pixel from  2010-2012
wu_2010<-calc(chm_classified2010_3,fun=Dipo)
wu_2011<-calc(chm_classified2011_3,fun=Dipo)
wu_2012<-calc(chm_classified2012_3,fun=Dipo)
wu_2013<-calc(chm_classified2013_3,fun=Dipo)
wu_2014<-calc(chm_classified2014_3,fun=Dipo)
wu_2015<-calc(chm_classified2015_3,fun=Dipo)
wu_2016<-calc(chm_classified2016_3,fun=Dipo)
wu_2017<-calc(chm_classified2017_3,fun=Dipo)
wu_2018<-calc(chm_classified2018_3,fun=Dipo)
wu_2019<-calc(chm_classified2019_3,fun=Dipo)
wu_2010
color2=c( "red", "blue" )
plot(wu_2010,
     legend = FALSE,
     col = color2, axes = FALSE)

legend("bottomright",
       legend = c( "Rice-(0.474266ac/ft)","Soybeans-(0.398383ac/ft)"),
       fill = color2,
       border = FALSE,
       bty = "n") # turn off legend border

#Total water use from 2010-2012 rotation

#wu_2013,wu_2014,wu_2015,wu_2016,wu_2017,wu_2018,wu_2019

wateruse<-function(x,y,z){return(x+y+z)}


WU2010_2012<-overlay(wu_2010,wu_2011,wu_2012,fun=wateruse)
plot(WU2010_2012)
WU2013_2015<-overlay(wu_2013,wu_2014,wu_2015,fun=wateruse)
plot(WU2013_2015)
WU2016_2018<-overlay(wu_2016,wu_2017,wu_2018,fun=wateruse)
plot(WU2016_2018)

WU2010_2018<-overlay(WU2010_2012,WU2013_2015,WU2016_2018,fun=wateruse)
plot(WU2010_2018)
#Tolu<-freq(WU2010_2012)
#scale for rotation
#r-s-s-1.27103
#r-r-s-1.34692
#r-s-r-1.34692
#s-r-s-1.27103
#s-s-r-1.27103
#r-1.4228
#s-1.19515
color=c("blue", "pink","green" ,"red" )
cuts=c( 3.58545,3.81309,4.04076,4.2684) #set breaks
plot(WU2010_2018)
addd<-function(n, alpha = 1, begin = 0, end = 1, direction =- 1){
  viridis(n, alpha, begin, end, direction, option = "viridis")
}

mapview(WU2010_2018,layer.name = "Delta",
        map = NULL,
        maxpixels = mapviewGetOption("mapview.maxpixels"),
        col.regions = addd,
        at = NULL,
        na.color = mapviewGetOption("na.color"),
        use.layer.names = F,
        map.types = mapviewGetOption("basemaps"),
        legend = mapviewGetOption("legend"),
        legend.opacity = 1,
        trim = F,
        verbose = mapviewGetOption("verbose"),
        homebutton = TRUE,
        method = mapviewGetOption("method"),
        label = TRUE)
  
  

plot2012<-plot(WU2010_2012,
     legend = F,
     col = color, axes = FALSE,
     box = FALSE,
     main = "Distribution of the estimated total water use per pixel- 2010-2012",cex.main=0.85, adj = 0., line=-0.5)
#2010-2012
legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water use ") # turn off legend border



plot2015<-plot(WU2013_2015,
               legend = F,
               col = color, axes = FALSE,
               box = FALSE,
               main = "Distribution of the estimated total water use per pixel- 2013-2015",cex.main=0.85, adj = 0., line=-0.5)
#2010-2012
legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water use ") # turn off legend border


plot2018<-plot(WU2016_2018,
               legend = F,
               col = color, axes = FALSE,
               box = FALSE,
               main = "Distribution of the estimated total water use per pixel- 2015-2018",cex.main=0.85, adj = 0., line=-0.5)
#2010-2012
legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water use ") # turn off legend border   
  
wateruse<-function(x,y){return(x-y)}
diffWU2010_2015<-overlay(WU2010_2012,WU2013_2015,fun=wateruse)
diffWU2015_2018<-overlay(WU2013_2015,WU2016_2018,fun=wateruse)
mapview(diffWU2010_2015)
plot(diffWU2010_2015)
diffWU2010_2015[diffWU2010_2015 == 0] <- NA
diffWU2015_2018[diffWU2015_2018== 0] <- NA
plot(diffWU2010_2015)
color=c("blue", "pink","green" ,"red" )
cuts=c( -0.227649,-0.07589,0.07589,0.227649) #set breaks

plot(diffWU2010_2015,
               legend = F,
               col = color, axes = FALSE,
               box = FALSE,
               main = "Distribution of the estimated total water use per pixel- 2010-2015",cex.main=0.85, adj = 0., line=-0.5)
#2010-2012
legend("bottomright",
       legend = c("R-S","Primarily SB-R.","Primarily Rice-SB/ft.",  "S-R" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water use ") # turn off legend border   

#2015-2019
plot(diffWU2015_2018,
     legend = F,
     col = color, axes = FALSE,
     box = FALSE,
     main = "Distribution of the estimated total water use per pixel- 2015_2018",cex.main=0.85, adj = 0., line=-0.5)
#2010-2012
legend("bottomright",
       legend = c("R-S","Primarily SB-R.","Primarily Rice-SB/ft.",  "S-R" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water use ") # turn off legend border   


AR_CDL_path<-"C:/Users/obemb/OneDrive/Desktop/data/Data/cdl"

AR=stack(AR2010,AR2011,AR2012,AR2013,AR2014)
AR
freq<-function
freq<-calc(AR, fun=, na.rm=T)
#share of rice and soybean in AR
