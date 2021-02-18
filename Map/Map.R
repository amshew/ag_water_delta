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
  rspatial,
  cowplot,
  grid,
  gridExtra,
  cdlTools,
  rgdal,
  raster,
  sp,
  gstat,
  dismo,
  ggspatial,
  readxl
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
setwd("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map")
list.files()
#ARkansas Shapefile
AR<- readOGR("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map/AR_shapefile/tl_2010_05_county10.shp",
             stringsAsFactors = FALSE)
plot(AR)
#dissolved state shapefile
AR_d<-aggregate(AR, dissolve = TRUE)
nestates <-c("Arkansas","Chicot","Clay","Craighead","Desha","Drew","Greene","Lee","Mississippi","Monroe",
             "Phillips","Poinsett","St. Francis","Jackson","Lawrence", "Jefferson","Lonoke","Crittenden","Woodruff",
             "Prairie","Randolph","White","Pulaski","Lincoln","Ashley","Cross","Lonoke")
gp <-c("Arkansas","Prairie","Jefferson","Lonoke","Phillips")
cp<-c("Clay","Greene","Craighead","Poinsett","Cross","St. Francis","Lee")
#Arkansas Delta
AR.Delta <- AR[as.character(AR@data$NAME10) %in% nestates, ]
plot(AR.Delta)
#Fully designated counties
gp_AR<-AR.Delta[as.character(AR.Delta@data$NAME10) %in% gp, ]
plot(gp_AR)
#partial designated counties
cp_AR<-AR.Delta[as.character(AR.Delta@data$NAME10) %in% cp, ]

plot(cp_AR)
#dissolve shapefile
#AR.Delta_d<-aggregate(AR.Delta, dissolve = TRUE)

#Delta Shapefile showing Crowley Ridge
Delta<- readOGR("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map/Delta_Alluvial_Shapefile/Delta.shp",
                stringsAsFactors = FALSE)


cp_AR_d<-aggregate(cp_AR, dissolve = TRUE)
plot(cp_AR_d)
plot(Delta, add=T)
memory.limit(size=20000000) 
crs(cp_AR_d)


#bring in the data and plot the DTW for the fall on 2019 

avav=raster("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map/IDW_Fall2019.tif")
test_spdf <- as(avav, "SpatialPixelsDataFrame")
plot(test_spdf)
crs(test_spdf)
crs(AR.Delta)
plot(AR.Delta)
AR_d<-aggregate(AR, dissolve = TRUE)
gp_AR_d<-aggregate(gp_AR, dissolve = TRUE)
plot(gp_AR_d)
(test_sf<-st_as_sf(test_spdf))
(Delta_sf<-st_as_sf(AR_d))
(Delta_sd<-st_as_sf(AR.Delta))
(Delta_gp<-st_as_sf(gp_AR_d))
(Delta_cp<-st_as_sf(cp_AR_d))
plot(AR_d)
(test_sf<-st_transform(test_sf,crs = crs(AR.Delta)))
crs(test_sf)
crs(Delta_sd)


#plot
library(ggspatial)
colnames(test_sf)[1]<-"value"
( DTW<-ggplot()+  
  geom_sf(data=Delta_sd,fill="grey70", color=NA) +
  geom_sf(data=test_sf, aes(color = value),size=0.02)+
  geom_sf(data=Delta_sd,fill=NA, color="grey50", size=1) +
  geom_sf(data=Delta_gp,fill=NA, color = "red", alpha = 5, linetype = 1, size=1)+
  geom_sf(data=Delta_cp,fill=NA, color = "blue", alpha = 5, linetype = 1, size=1)+
  scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                        name ="Depth to water (ft.)")+theme_bw()+
  annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
  theme(legend.title = element_text(size=10, face="italic"))+
  xlab("Longitude")+ ylab("Latitude")+theme(legend.justification = c(1, 0), 
                                            legend.position = c(1, 0))+
  theme(legend.title.align=0.8)  )+ theme(axis.text = element_text(size=8))

DTW


#bring in the data and plot the DTW for the fall on 2019

#Rice_freq<-readRDS("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map/Rice_frequency.rds")

#crs(Rice_freq)
#Freq_rice<-aggregate(Rice_freq, fact=5) ### aggregate resolution to 150m
memory.limit(size=20000000) 
#saveRDS(Freq_rice, file="C:/Users/obemb/OneDrive/Desktop/data/Resources/manuscript/Map/Freq_rice.rds")
Freq_rice<-readRDS("C:/Users/obemb/OneDrive/Desktop/data/Resources/manuscript/Map/Freq_rice.rds")
rice_spdf <- as(Freq_rice, "SpatialPixelsDataFrame")
plot(rice_spdf)
crs(rice_spdf)
crs(AR.Delta)
crs(AR_d)

(rice_sf<-st_as_sf(rice_spdf))
crs(rice_sf)
#--- Albert conic projection ---# st_set_crs("+proj=longlat +datum=NAD83 +no_defs")

(Delta_sf<-st_as_sf(AR_d))
(Delta_sd<-st_as_sf(AR.Delta))

(rice_sf<-st_transform(rice_sf,crs = crs(AR.Delta)))
crs(AR.Delta)
crs(AR_d)
crs(rice_sf)
library(cowplot)
library(ggspatial)
Rice<-ggplot()+
  geom_sf(data=Delta_sd,fill="grey70", color=NA) +
  geom_sf(data=rice_sf, aes(color = layer),size=0.02)+
  geom_sf(data=Delta_sd,fill=NA, color="grey50", size=1) +
  geom_sf(data=Delta_gp,fill=NA, color = "red", alpha = 5, linetype = 1, size=1)+
  geom_sf(data=Delta_cp,fill=NA, color = "blue", alpha = 5, linetype = 1, size=1)+
  scale_color_distiller(palette = "Spectral",type = "seq", direction = -1,
                        name ="Rice cropping frequency")+
  annotation_scale(location = "bl", width_hint = 0.2,height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering)+
    theme_bw()+
  theme(legend.title.align = 0.5)+theme(legend.text=element_text(size=8))+
  theme(legend.title = element_text(size=10, face="italic"))+
  xlab("Longitude")+ ylab("Latitude")+theme(legend.justification = c(1, 0), 
                                            legend.position = c(1, 0))+
  theme(legend.title.align=0.8) + theme(axis.text = element_text(size=8))

Rice

figure <- plot_grid(DTW,Rice,
                     labels = c('A', 'B')
                     , label_fontface = "plain",ncol = 2,label_size=12)
figure


#### 3rd plot
###### Total Costshare Spent
cost_region<-read.csv("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map/Data_plot.csv")

require(scales)

ggplot(cost_region,aes(x=year,y=cost_share/1000,color=as.factor(region)))+
  geom_line() +
  labs(y = "Total Costshare Spent ($,000)",
      x = "Years",
      colour = "CGAs")+
  scale_colour_manual(values = c("red","blue","green"), breaks=c("1", "2","3"),
                      labels=c("Full", "Partial", "Non"))+
  theme_classic()+scale_y_continuous(labels = comma, breaks = seq(0,14000,2000),limits=c(0,14000)
                                     )+
  theme(legend.justification = c(1, 0), 
                                  legend.position = c(.95, 0.7),
        legend.box.margin=margin(c(0,10,0,10)))+
          theme(legend.text=element_text(size=rel(1)))+
          theme(legend.key.size = unit(1.5,"line"))+
  scale_x_continuous(breaks=seq(2006,2019,2),limits = c(2006,2019))+ theme(axis.title  = element_text(size = 11))+
  theme(axis.text = element_text(size = 11))
 
     ###### nos of IWM practices 
cost_region<-read.csv("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map/Data_plot.csv")

require(scales)

ggplot(cost_region,aes(x=year,y=nos_practices,color=as.factor(region)))+
  geom_line() +
    labs(y = "Total number of IWM practices",
       x = "Years",
       colour = "CGAs")+
  scale_colour_manual(values = c("red","blue","green"), breaks=c("1", "2","3"),
                      labels=c("Full", "Partial", "Non"))+
  theme_classic()+scale_y_continuous(labels = comma, breaks = seq(0,1000,100),limits=c(0,1000) )+
  theme(legend.justification = c(1, 0), 
        legend.position = c(.95, 0.7),
        legend.box.margin=margin(c(0,10,0,10)))+
  theme(legend.text=element_text(size=rel(1)))+
  theme(legend.key.size = unit(1.5,"line"))+
  scale_x_continuous(breaks=seq(2006,2019,2),limits = c(2006,2019))+ theme(axis.title  = element_text(size = 11))+
  theme(axis.text = element_text(size = 11))


     
 ###rice share
Rice<-read.csv("C:/Users/obemb/OneDrive/Documents/R/ag_water_delta/Map/Rice.csv")

Rice$region <- factor(Rice$region,levels = c("Full CGA","Partial CGA","Non CGA"))
Region<-c(a="Full CGA",b= "Partial CGA",c= "Non CGA")
( cga_croprotations <- ggplot(Rice,aes( x=factor(Year) , y=Rotate,fill=type))  +
    geom_boxplot()+
    facet_wrap(~region, ncol = 3, scales = "free")+
    theme(panel.grid.minor.x = element_blank()) +
    # remove facet spacing on x-direction
    theme(panel.spacing.x = unit(1,"line"))+
    theme(strip.placement = 'outside',
          strip.background.x = element_blank())+
    labs(x = "Year", y =  "Share of rice (t-1) in rice (t) or soybean(t)", size = 10) +
    scale_y_continuous(breaks = seq(0,1,0.1), limits=c(0,1)) +
    scale_x_discrete(breaks=seq(2009, 2019, 2))+
    scale_fill_manual(values = c("#006D2C", "#0868AC"), name= element_blank(),
                      breaks=c("R_S", "R_R"), labels=c("Rice-soybeans", "Rice-rice")) +
    theme(axis.text = element_text(size=8), axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.5, 0.92), legend.background = element_rect(fill="transparent"))
)
ggsave(filename = "cga_croprotations.png", plot = cga_croprotations,
       device = "png", height = 4.75, width = 6.5 , units = "in", dpi = 500)


