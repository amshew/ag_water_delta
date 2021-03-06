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
setwd("C:/demandrs/obemb/OneDrive/Documents/R/ag_water_delta/Data")
list.files()
memory.limit(size=100000)

######Download CDL*******************#

(
  AR_cdl<- getCDL(5, 2009:2019,location="C:/demandrs/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl")
)
show( AR_cdl$AR2015)
crs(AR_cdl$AR2015)
projection(crs(AR_cdl$AR2015))

#----Hardway---will rewrite a  simpler code later---#
AR2010 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2010_05.tif") 
AR2011 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2011_05.tif") 
AR2012 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2012_05.tif") 
AR2013 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2013_05.tif") 
AR2014 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2014_05.tif") 
AR2015 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2015_05.tif") 
AR2016 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2016_05.tif") 
AR2017 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2017_05.tif") 
AR2018 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2018_05.tif") 
AR2019 <-raster("C:/users/obemb/OneDrive/Documents/R/ag_water_delta/Data/cdl/CDL_2019_05.tif")
library(mapview)
#*************plot#
plot(AR2010)
mapView((AR2010))

#****crop frequency/pixel count***


CDL_code<- read.csv("cdl_codes_names.csv")

duration<- data.frame(id=1:10,year=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))
str(duration)

AR<-brick(AR2010,AR2011,AR2012,AR2013,AR2014,AR2015,AR2016,AR2017,AR2018,AR2019)
AR

AR<-rast(AR)
#AR_stackfreq<-freq(AR, merge=TRUE)
#top 6crops
#brisk


reclass_df<-c(0,0,0,
              1,1,1,
              2,2,2,
              3,3,3,
              4,4,4,
              5,5,5,
              6,6,6,
              10,10,10,
              11,61,8,
              63,255,NA
)
reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)
reclass_m 


reclass_df2<-c(1,2,
               2,3,
               3,4,
               4,5,
               5,6,
               6,1,
               8,1,
               10,7
               
)

AR_classified_a<- reclassify(AR,
                             reclass_m)
# assign all pixels that equal 0 to NA or no data value
#AR_classified2010_1[AR_classified2010_1 == 6] <- 0
#AR_classified_a[AR_classified_a == 0] <- NA

show(AR_classified_a)
reclass_m2 <- matrix(reclass_df2,
                     ncol = 2,
                     byrow = TRUE)
AR_classified_b<- reclassify(AR_classified_a,      
                             reclass_m2)
show(AR_classified_b)
#for rice and soybeans
reclass_df3<-c(1,0,
               2,0,
               3,0,
               4,4,
               5,0,
               6,6,
               7,0
)


reclass_m3 <- matrix(reclass_df3,
                     ncol = 2,
                     byrow = TRUE)

AR_classified_c<- reclassify(AR_classified_b,      
                              reclass_m3)
show(AR_classified_c)
#1other crops and fallow, 2-corn, 3-cotton,4-rice,5-sorghum,6-soybeans,7-peanut
#Reclassification

#2010
AR_classified2010_a <- reclassify(AR2010,
                                  reclass_m)
AR_classified2010_a[AR_classified2010_a == 0] <- NA
show(AR_classified2010_a)
AR_classified2010_b<- reclassify(AR_classified2010_a,      
                                 reclass_m2)
show(AR_classified2010_b)
AR_classified2010_c<- reclassify(AR_classified2010_b,      
                                 reclass_m3)
show(AR_classified2010_c)

#2011
AR_classified2011_a <- reclassify(AR2011,
                                  reclass_m)
AR_classified2011_a[AR_classified2011_a == 0] <- NA
show(AR_classified2011_a)
AR_classified2011_b<- reclassify(AR_classified2011_a,      
                                 reclass_m2)
show(AR_classified2011_b)
AR_classified2011_c<- reclassify(AR_classified2011_b,      
                                 reclass_m3)
show(AR_classified2011_c)

#2012
AR_classified2012_a <- reclassify(AR2012,
                                  reclass_m)
AR_classified2012_a[AR_classified2012_a == 0] <- NA
show(AR_classified2012_a)
AR_classified2012_b<- reclassify(AR_classified2012_a,      
                                 reclass_m2)
AR_classified2012_a[AR_classified2012_a == 0] <- NA
show(AR_classified2012_b)
AR_classified2012_c<- reclassify(AR_classified2012_b,      
                                 reclass_m3)
show(AR_classified2012_c)

#2013
AR_classified2013_a <- reclassify(AR2013,
                                  reclass_m)
AR_classified2013_a[AR_classified2013_a == 0] <- NA
show(AR_classified2013_a)
AR_classified2013_b<- reclassify(AR_classified2013_a,      
                                 reclass_m2)
show(AR_classified2013_b)
AR_classified2013_c<- reclassify(AR_classified2013_b,      
                                 reclass_m3)
show(AR_classified2013_c)

#2014
AR_classified2014_a <- reclassify(AR2014,
                                  reclass_m)
AR_classified2014_a[AR_classified2014_a == 0] <- NA
show(AR_classified2014_a)
AR_classified2014_b<- reclassify(AR_classified2014_a,      
                                 reclass_m2)
show(AR_classified2014_b)

AR_classified2014_c<- reclassify(AR_classified2014_b,      
                                 reclass_m3)
show(AR_classified2014_c)
#2015
AR_classified2015_a <- reclassify(AR2015,
                                  reclass_m)
AR_classified2015_a[AR_classified2015_a == 0] <- NA
show(AR_classified2015_a)
AR_classified2015_b<- reclassify(AR_classified2015_a,      
                                 reclass_m2)
show(AR_classified2015_b)
AR_classified2015_c<- reclassify(AR_classified2015_b,      
                                 reclass_m3)
show(AR_classified2015_c)


#2016
AR_classified2016_a <- reclassify(AR2016,
                                  reclass_m)
AR_classified2016_a[AR_classified2016_a == 0] <- NA
show(AR_classified2016_a)
AR_classified2016_b<- reclassify(AR_classified2016_a,      
                                 reclass_m2)
show(AR_classified2016_b)

AR_classified2016_c<- reclassify(AR_classified2016_b,      
                                 reclass_m3)
show(AR_classified2016_c)

#2017
AR_classified2017_a <- reclassify(AR2017,
                                  reclass_m)
AR_classified2017_a[AR_classified2017_a == 0] <- NA
show(AR_classified2017_a)
AR_classified2017_b<- reclassify(AR_classified2017_a,      
                                 reclass_m2)
show(AR_classified2017_b)
AR_classified2017_c<- reclassify(AR_classified2017_b,      
                                 reclass_m3)
show(AR_classified2017_c)

#2018
AR_classified2018_a <- reclassify(AR2018,
                                  reclass_m)
AR_classified2018_a[AR_classified2018_a == 0] <- NA
show(AR_classified2018_a)
AR_classified2018_b<- reclassify(AR_classified2018_a,      
                                 reclass_m2)
show(AR_classified2018_b)
AR_classified2018_c<- reclassify(AR_classified2018_b,      
                                 reclass_m3)
show(AR_classified2018_c)
#2019
AR_classified2019_a <- reclassify(AR2019,
                                  reclass_m)
AR_classified2019_a[AR_classified2019_a == 0] <- NA
show(AR_classified2019_a)
AR_classified2019_b<- reclassify(AR_classified2019_a,      
                                 reclass_m2)
show(AR_classified2019_b)

AR_classified2019_c<- reclassify(AR_classified2019_b,      
                                 reclass_m3)
show(AR_classified2019_c)

#Two year rotation-2010-2011
RT<-function(x,y){ifelse(x>y,1, ifelse(x<y,-1,ifelse(x==y, x,y)))}

Diff<-overlay(AR_classified2010_c,AR_classified2011_c,fun=RT)
#plot(Diff)



color<-c( "green","cyan","purple","red","blue"  )
plot(Diff,
     legend = F,
     col = color, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2010-2011",cex.main=0.85, adj = 0.3, line=-0.5)

legend("bottomright",
       legend = c("Rice to Soybeans"," TO Other Crops","Soybeans to Rice",  "Rice Monocropping",  "Soybeans Monocropping"),
       fill = color,
       border = FALSE,
       bty = "n") # turn off legend border
# removing other crops
Diff[Diff == 0] <- NA
color<-c( "green","purple","red","blue"  )
plot(Diff,
     legend = F,
     col = color, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2010-2011",cex.main=0.85, adj = 0.3, line=-0.5)

legend("bottomright",
       legend = c("Rice to Soybeans","Soybeans to Rice",  "Rice Monocropping",  "Soybeans Monocropping"),
       fill = color,
       border = FALSE,
       bty = "n") # turn off legend border
#2011to2012


Diff_11_12<-overlay(AR_classified2011_c,AR_classified2012_c,fun=RT)


Diff_11_12[Diff_11_12== 0] <- NA
plot(Diff)


plot(Diff_11_12,
     legend = F,
     col = color, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2011-2012",cex.main=0.85, adj = 0.3, line=-0.5)

legend("bottomright",
       legend = c("Rice to Soybeans","Soybeans to Rice",  "Rice Monocropping",  "Soybeans Monocropping"),
       fill = color,
       border = FALSE,
       bty = "n") # turn off legend border

#2011to2012

mapview_color<-function(n, alpha = 1, begin = 0, end = 1, direction =- 1){
  viridis(n, alpha, begin, end, direction, option = "viridis")
}



#three year rotation
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

Dipo<-function(x,y,z){ifelse(x==y & y==z,x,
                             ifelse(x==z& x<y,-1,ifelse(x==z& x>y,1,#sR
                                                        ifelse(x==y& x>z, 2,
                                                               ifelse(x==y& x<z,-2,ifelse(z==y& x>z, 3,
                                                                                          ifelse(z==y& x<z, -3,0)))))))}


Diff2010_2012<-overlay(AR_classified2010_c,AR_classified2011_c,AR_classified2012_c,fun=Dipo)


color2<-c("yellow", "green","purple" ,"cyan","pink","orange" ,"red", "blue" )
plot(Diff2010_2012,
     legend = F,
     col = color2, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2010-2012",cex.main=0.85, adj = 0.3, line=-0.5)

legend("bottomright",
       legend = c("R-S-S","R-R-S",  "R-S-R", "Other Crops", "S-R-S", "S-S-R","Rice Monocropping", "Soybeans Monocropping"),
       fill = color2,
       border = FALSE,
       bty = "n") # turn off legend border
#No other crops
color2<-c("yellow", "green","purple" ,"pink","orange" ,"red", "blue" )
Diff2010_2012[Diff2010_2012==0]<-NA

plot(Diff2010_2012,
     legend = F,
     col = color2, axes = FALSE,
     box = FALSE,
     main = "Rice-Soybeans Rotation- 2010-2012",cex.main=0.85, adj = 0.3, line=-0.5)

legend("bottomright",
       legend = c("R-S-S","R-R-S",  "R-S-R", "S-R-S", "S-S-R","Rice Monocropping", "Soybeans Monocropping"),
       fill = color2,
       border = FALSE,
       bty = "n") # turn off legend border


#water demand per pixel-convert each pixel to water demand-3year rotation.

Rice<-2.13255*0.222394 #converts to ac/ft of water per pixel
Soybeans<-1.79134*0.222394 #converts to ac/ft of water per pixel

Dipo<-function(x){ifelse(x==3,Rice,
                         ifelse(x==5, Soybeans,0))}


wu_2010<-calc(AR_classified2010_c,fun=Dipo)
wu_2011<-calc(AR_classified2011_c,fun=Dipo)
wu_2012<-calc(AR_classified2012_c,fun=Dipo)
wu_2013<-calc(AR_classified2013_c,fun=Dipo)
wu_2014<-calc(AR_classified2014_c,fun=Dipo)
wu_2015<-calc(AR_classified2015_c,fun=Dipo)
wu_2016<-calc(AR_classified2016_c,fun=Dipo)
wu_2017<-calc(AR_classified2017_c,fun=Dipo)
wu_2018<-calc(AR_classified2018_c,fun=Dipo)
wu_2019<-calc(AR_classified2019_c,fun=Dipo)
wu_2010
color2=c( "red", "blue" )
#water demand per pixel-convert each pixel to water demand-2010.
plot(wu_2010,
     legend = FALSE,
     col = color2, axes = FALSE)

legend("bottomright",
       legend = c( "Rice-(0.474266ac/ft)","Soybeans-(0.398383ac/ft)"),
       fill = color2,
       border = FALSE,
       bty = "n") # turn off legend border

#Total water demand from 2010-2012 rotation


#water demand per pixel from  2010-2012
waterdemand<-function(x,y,z){return(x+y+z)}


WU2010_2012<-wu_2010+wu_2011+wu_2012
plot(WU2010_2012)
WU2013_2015<-overlay(wu_2013,wu_2014,wu_2015,fun=waterdemand)
plot(WU2013_2015)
WU2016_2018<-overlay(wu_2016,wu_2017,wu_2018,fun=waterdemand)
plot(WU2016_2018)

WU2010_2018<U2010_2012+WU2013_2015+WU2016_2018
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

cuts=c( 1.19515,1.27103,1.34692,1.4228) #set breaks
plot(WU2010_2012)
addd<-function(n, alpha = 1, begin = 0, end = 1, direction =- 1){
  viridis(n, alpha, begin, end, direction, option = "viridis")
}

mapview(WU2010_2018,layer.name = "Delta",
        map = NULL,
        maxpixels = mapviewGetOption("mapview.maxpixels"),
        col.regions = addd,
        at = NULL,
        na.color = mapviewGetOption("na.color"),
        demand.layer.names = F,
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
               main = "Distribution of the estimated total water demand per pixel- 2010-2012",cex.main=0.85, adj = 0., line=-0.5)
#2010-2012
legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water demand ") # turn off legend border



plot2015<-plot(WU2013_2015,
               legend = F,
               col = color, axes = FALSE,
               box = FALSE,
               main = "Distribution of the estimated total water demand per pixel- 2015-2018",cex.main=0.85, adj = 0., line=-0.5)
#2013-2015
legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water demand ") # turn off legend border


plot2019<-plot(WU2016_2018,
               legend = F,
               col = color, axes = FALSE,
               box = FALSE,
               main = "Distribution of the estimated total water demand per pixel- 2016-2018",cex.main=0.85, adj = 0., line=-0.5)
#2016-2019
legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water demand ") # turn off legend border   

#2010-2019
cuts=c( 3.58545,3.81309,4.04076,4.2684) #set breaks
plot2010_19<-plot(WU2010_2018,
                  legend = F,
                  col = color, axes = FALSE,
                  box = FALSE,
                  main = "Distribution of the estimated total water demand per pixel- 2016-2018",cex.main=0.85, adj = 0., line=-0.5)

legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water demand ") # turn off legend border   
#difference in water demand across time
waterdemand<-function(x,y){return(x-y)}
diffWU2010_2015<-overlay(WU2010_2012,WU2013_2015,fun=waterdemand)
diffWU2016_2018<-overlay(WU2013_2015,WU2016_2018,fun=waterdemand)
#remove the zeros
diffWU2010_2015[diffWU2010_2015 == 0] <- NA
diffWU2016_2018[diffWU2016_2018 == 0] <- NA
plot(diffWU2010_2015)
summary(diffWU2010_2015)
color=c("blue", "pink","green", "red" )
#Total water demand
cuts=c( -0.227649,-0.07589,0.07589,0.227649) #set breaks

plot2019<-plot(diffWU2016_2018,
               legend = F,
               col = color, axes = FALSE,
               box = FALSE,
               main = "Distribution of the estimated total water demand per pixel- 2010-2019",cex.main=0.85, adj = 0., line=-0.5)
#2010-2012
legend("bottomright",
       legend = c("Soybeans Monocropping-3.6 ac/ft.","Primarily SB-R-3.81ac/ft.","Primarily Rice-R-4.04ac/ft.",  "Rice Monocropping-4.27ac/ft" ),
       fill = color,
       border = FALSE,
       bty = "n",cex=.75,title="Crop Rotation/Total water demand ") # turn off legend border   
# Crop Frequency
ARBrisk<-brick(AR_classified2010_c,AR_classified2011_c,AR_classified2012_c,AR_classified2014_c,AR_classified2015_c,
               AR_classified2016_c,AR_classified2017_c,AR_classified2018_c,AR_classified2019_c)
show(ARBrisk)
reclass_df_rice<-c(4,1,
                   6,0
)
reclass_m_rice <- matrix(reclass_df_rice,
                         ncol = 2,
                         byrow = TRUE)
AR_rice<- reclassify(ARBrisk,
                     reclass_m_rice  )
show(AR_rice)
#soybean
reclass_df_soybean<-c(4,0,
                      6,1
)



reclass_m_soybean <- matrix(reclass_df_soybean,
                            ncol = 2,
                            byrow = TRUE)
AR_soybean<- reclassify(ARBrisk,
                        reclass_m_soybean )
# assign all pixels that equal 0 to NA or no data value
show(AR_soybean)

Freq_rice<-calc(AR_rice,sum)
Freq_soybean<-calc(AR_soybean,sum)
show(Freq_rice)
show(Freq_soybean)
library(RColorBrewer)
my.palette <- c('grey', '#f7fcf5','#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4',
                '#1d91c0','#225ea8','#253494','#081d58')

plot(Freq_rice,
     legend = F,
     col =my.palette, axes = FALSE,
     box = FALSE,
     main = )

legend("bottomright",
       legend = c("Zero Year"," One Year","Two Years","Three Years",  " four Years" ,
                  "Five Years", "Six Years","Seven Years", "Eight Years","Nine Years","Ten Years"),
       fill = my.palette ,
       border = FALSE,
       bty = "n",cex=.75,title="Rice Crop Frequency") # turn off legend border   
soy.palette <- c('grey','light grey', '#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')
plot(Freq_soybean,
     legend = F,
     col = soy.palette, axes = FALSE,
     box = FALSE,
     main = )

legend("bottomright",
       legend = c("Zero Year"," One Year","Two Years","Three Years",  " four Years" ,
                  "Five Years", "Six Years","Seven Years", "Eight Years","Nine Years","Ten Years"),
       fill = soy.palette,
       border = FALSE,
       bty = "n",cex=.75,title="Soybeans Cropping Frequency") # turn off legend border   

#frequency for corn

ARcorn<-brick(AR_classified2010_b,AR_classified2011_b,AR_classified2012_b,AR_classified2014_b,AR_classified2015_b,
              AR_classified2016_b,AR_classified2017_b,AR_classified2018_b,AR_classified2019_b)
show(ARBrisk)
reclass_df3_corn<-c(1,0,
                    2,1,
                    3,0,
                    4,0,
                    5,0,
                    6,0,
                    7,0
)
reclass_m3_corn <- matrix(reclass_df3_corn,
                          ncol = 2,
                          byrow = TRUE)
AR_cornR<-reclassify(AR_corn,
                     reclass_m3_corn   )

Freq_corn<-calc(AR_cornR,sum)
my.palette <- c('grey', '#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')

plot(Freq_corn,
     legend = F,
     col =my.palette, axes = FALSE,
     box = FALSE,
     main = )

legend("bottomright",
       legend = c("Zero Year"," One Year","Two Years","Three Years",  " four Years" ,
                  "Five Years", "Six Years","Seven Years", "Eight Years","Nine Years","Ten Years"),
       fill = my.palette ,
       border = FALSE,
       bty = "n",cex=.75,title="Corn Cropping Frequency") # turn off legend border   
##frequency for cotton

ARcorn<-brick(AR_classified2010_b,AR_classified2011_b,AR_classified2012_b,AR_classified2014_b,AR_classified2015_b,
              AR_classified2016_b,AR_classified2017_b,AR_classified2018_b,AR_classified2019_b)
show(ARBrisk)
reclass_df3_cotton<-c(1,0,
                      2,0,
                      3,1,
                      4,0,
                      5,0,
                      6,0,
                      7,0
)
reclass_m3_cotton <- matrix(reclass_df3_cotton,
                            ncol = 2,
                            byrow = TRUE)
AR_cotton<-reclassify(AR_corn,
                      reclass_m3_cotton )

Freq_cotton<-calc(AR_cotton,sum)
my.palette <- c('grey', '#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')

plot(Freq_cotton,
     legend = F,
     col =my.palette, axes = FALSE,
     box = FALSE,
     main = )

legend("bottomright",
       legend = c("Zero Year"," One Year","Two Years","Three Years",  " four Years" ,
                  "Five Years", "Six Years","Seven Years", "Eight Years","Nine Years","Ten Years"),
       fill = my.palette ,
       border = FALSE,
       bty = "n",cex=.75,title="Cotton Cropping Frequency") # turn off legend border   
#sequence of rotation

year_2010<-1000000000
year_2011<-100000000
year_2012<-10000000
year_2013<-1000000
year_2014<-100000
year_2015<-10000
year_2016<-1000
year_2017<-100
year_2018<-10
year_2019<-1

yr_ar_10 <-AR_classified2010_c * year_2010
yr_ar_11 <- AR_classified2011_c * year_2011
yr_ar_12 <-AR_classified2012_c * year_2012
yr_ar_13 <- AR_classified2013_c * year_2013
yr_ar_14 <-AR_classified2014_c * year_2014
yr_ar_15 <- AR_classified2015_c * year_2015
yr_ar_16 <-AR_classified2016_c * year_2016
yr_ar_17 <- AR_classified2017_c * year_2017
yr_ar_18 <-AR_classified2018_c * year_2018
yr_ar_19 <- AR_classified2019_c * year_2019
all_ar <-yr_ar_13+yr_ar_14+yr_ar_15+ yr_ar_16+ yr_ar_17+
  yr_ar_18 + yr_ar_19+yr_ar_10+yr_ar_11+yr_ar_12

plot(all_ar)

#top 6crops




yr_ar_10 <-AR_classified2010_b * year_2010
yr_ar_11 <- AR_classified2011_b * year_2011
yr_ar_12 <-AR_classified2012_b * year_2012
yr_ar_13 <- AR_classified2013_b * year_2013
yr_ar_14 <-AR_classified2014_b * year_2014
yr_ar_15 <- AR_classified2015_b * year_2015
yr_ar_16 <-AR_classified2016_b * year_2016
yr_ar_17 <- AR_classified2017_b * year_2017
yr_ar_18 <-AR_classified2018_b * year_2018
yr_ar_19 <- AR_classified2019_b * year_2019
all_ar <-yr_ar_13+yr_ar_14+yr_ar_15+ yr_ar_16+ yr_ar_17+
  yr_ar_18 + yr_ar_19+yr_ar_11+yr_ar_12

plot(all_ar)

Count<-freq(all_ar)
view(Count)
mapview(all_ar)
show(all_ar)

​
