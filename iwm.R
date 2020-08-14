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
memory.limit(size=100000)

setwd("C:/Users/obemb/OneDrive/Desktop/data/hydrologic_units_WBDHU8_ar_2521302_01/hydrologic_units")
list.files()
loadfonts(quiet = T)

list.files()
loadfonts(quiet = T)
huc_8<- read_sf("wbdhu8_a_ar.shp",
               stringsAsFactors = FALSE) %>%
  arrange(HUC8)
show(huc_8)
library(mapview)
mapview(huc_8)
crs(huc_8)
huc_8.tbl <- fortify(huc_8, region = 'HUC8')
huc_8$HUC8 <- as.numeric(huc_8.tbl$HUC8 )

IWM_loc<- read.csv( "C:/Users/obemb/OneDrive/Desktop/data/Data/practices/IWM_location.csv" )
crs(IWM_loc)
head(IWM_loc)
plot(IWM_loc$longitude, IWM_loc$latitude )


library(gganimate)
require(magick)
(aa=ggplot()+
  geom_sf(data=huc_8)+
    geom_point(data=IWM_loc,aes(y=latitude, x=longitude,color=fy))+
    scale_color_viridis_c(name="Year",)+
    theme_minimal() +  # apply minimal theme
    theme(panel.grid = element_blank())+
    theme_map()+transition_time(fy) + 
  ease_aes('linear', interval = 0.000003) +labs(title = "Year: {frame_time}") 
)
library(gifski)
animate(aa, nframes = 12, duration=10,renderer = gifski_renderer("C:/Users/obemb/OneDrive/Desktop/data/Data/practices/gganim/gganim.gif"))
animate(aa,nframes = 12, device = "png",
        renderer = file_renderer("C:/Users/obemb/OneDrive/Desktop/data/Data/practices/gganim", prefix = "gganim_plot", overwrite = TRUE))



(ab<-left_join(huc_8,IWM_loc, by=c("HUC8" = "HUC_8")))
mapview(ab)
plot(ab)