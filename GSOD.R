
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
  rgeos,
  GSODR,
  zoo # download PRISM data
)   

#code starts here
# the end bound

library(dplyr)
          library("GSODR") 
  ( ks<-  nearest_stations(39.382214, -101.046194, 100) )   
       
Data <- get_GSOD(years = 1976:2019, station = ks,max_missing=1)

Colby<-Data%>%subset( ., select =c(YEAR, MONTH,DAY,MAX))%>%subset(., MONTH>=3 & MONTH<=5)


Colby<-Colby[Colby$YEAR > "1979",]%>%
  mutate(approx = na.approx(MAX))



Colby<-Colby%>%mutate( ., Year=ifelse(YEAR>=1980&YEAR<=1989,"1980-1989",ifelse(YEAR>=1990&YEAR<=1999,
                                                                               "1990-1999",ifelse(YEAR>=2000&YEAR<=2009,
                                                                                                  "2000-2009",ifelse(YEAR>=2010&YEAR<=2019,
                                                                                                                     "2010-2019",0)))))




Colby$N=ave(1:length(Colby$YEAR ), Colby$YEAR , FUN = seq_along)
#Colby <- na.omit(Colby) 
Colby$anomaly=as.numeric(as.character((Colby$approx)))


Colby<-Colby%>% group_by(Year)%>%within(., {mean1 = ave(approx,Year,FUN=mean)} )%>%within(., {sd1 = ave(approx,Year,FUN=sd)} )

##using 30-year deviation to standardized
years_30<-Colby[Colby$YEAR > "1979" &Colby$YEAR<= "2009",]
mean2=mean(years_30$approx)
sd2=sd(years_30$approx)
Colby$anomaly=as.numeric(as.character((Colby$approx-mean2)/sd2))
#Colby$anomaly2<-scale(Colby$approx)
##using base-year deviation to standardized
Colby$anomaly2=as.numeric(as.character((Colby$approx-Colby$mean1)/Colby$sd1))
Colby$Nas=ave(1:length(Colby$Year ), Colby$Year, FUN = seq_along)

Colby_30<-Colby%>%group_by(anomaly,Year)%>%summarise(count= n())
Colby_base<-Colby%>%group_by(anomaly2,Year)%>%summarise(count= n())



Colby_30=Colby_30%>%mutate( .,  Change=ifelse(anomaly>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)%>%mutate( .,count_change=100*count/total)
Colby_base=Colby_base%>%mutate( .,  Change=ifelse(anomaly2>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)%>%mutate( .,count_change=100*count/total)

arrows <- 
  tibble(
    x1 = c(2.45, 2.85,2.85),
    x2 = c(2.05, 2.05,1.85),
    y1 = c(2.9,3.9,7.8),
    y2 = c(2, 3.4,7)
  )
mycolor<-c("#fecc5c", "#fd8d3c", "#f03b20","#bd0026")
###30-years standardization
(Plot_30<-ggplot(data = Colby_30, aes(x = anomaly, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized Difference from 1980-2009 Mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)

###base-10year standardization
(Plot_base<-ggplot(data = Colby_base, aes(x = anomaly2, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized difference based on each period mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)



##################Niger_Maradi
library("GSODR") 
( NG_station<-  nearest_stations(13.709462, -13.693913, 100) )   

NG_data <- get_GSOD(years = 1979:2019, station = NG_station)

NG_analy<-NG_data%>%subset( ., select =c(YEAR, MONTH,DAY,MAX))%>%subset(., MONTH>=7 & MONTH<=9)


NG_analy<-NG_analy[NG_analy$YEAR > "1979" ,]%>%
  mutate(approx = na.approx(MAX))

NG_analy<-NG_analy%>%mutate( ., Year=ifelse(YEAR>=1980&YEAR<=1989,"1980-1989",ifelse(YEAR>=1990&YEAR<=1999,
                                                                               "1990-1999",ifelse(YEAR>=2000&YEAR<=2009,
                                                                                                  "2000-2009",ifelse(YEAR>=2010&YEAR<=2019,
                                                                                                                     "2010-2019",0)))))





NG_analy$N=ave(1:length(NG_analy$YEAR ), NG_analy$YEAR , FUN = seq_along)
#Colby <- na.omit(Colby) 
NG_analyDAY=NG_analy%>%group_by(Year)%>%summarise(max =n())
NG_analy$anomaly=as.numeric(as.character((NG_analy$approx)))


NG_analy<-NG_analy%>% group_by(Year)%>%within(., {mean1 = ave(approx,Year,FUN=mean)} )%>%within(., {sd1 = ave(approx,Year,FUN=sd)} )

##using 30-year deviation to standardized
years_30<-NG_analy[NG_analy$YEAR > "1979" &NG_analy$YEAR<= "2009",]
mean2=mean(years_30$approx)
sd2=sd(years_30$approx)
NG_analy$anomaly=as.numeric(as.character((NG_analy$approx-mean2)/sd2))
#NG_analy$anomaly2<-scale(NG_analy$approx)
##using base-year deviation to standardized
NG_analy$anomaly2=as.numeric(as.character((NG_analy$approx-NG_analy$mean1)/NG_analy$sd1))
NG_analy$Nas=ave(1:length(NG_analy$Year ), NG_analy$Year, FUN = seq_along)

NG_analy_30<-NG_analy%>%group_by(anomaly,Year)%>%summarise(count= n())
NG_analy_base<-NG_analy%>%group_by(anomaly2,Year)%>%summarise(count= n())



NG_analy_30=NG_analy_30%>%mutate( .,  Change=ifelse(anomaly>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)%>%mutate( .,count_change=100*count/total)
NG_analy_base=NG_analy_base%>%mutate( .,  Change=ifelse(anomaly2>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)%>%mutate( .,count_change=100*count/total)
arrows <- 
  tibble(
    x1 = c(2.45, 2.75),
    x2 = c(1.8, 1.9),
    y1 = c(2.9,2.5),
    y2 = c(1.85, 1.7)
  )

mycolor<-c("#fecc5c", "#fd8d3c", "#f03b20","#bd0026")

###30-years standardization
(Plot_30<-ggplot(data = NG_analy_30, aes(x = anomaly, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized Difference from 1980-2009 Mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)

###base-10year standardization
(Plot_base<-ggplot(data = NG_analy_base, aes(x = anomaly2, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized difference based on each period mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)



##################Sinthiou Malem, Senegal
library("GSODR") 
( SG_station<-  nearest_stations(14.242193, -13.425908, 200) )   

SG_data <- get_GSOD(years = 1979:2019, station = SG_station)

SG_analy<-SG_data%>%subset( ., select =c(YEAR, MONTH,DAY,MAX))%>%subset(., MONTH>=8 & MONTH<=10)


SG_analy<-SG_analy[SG_analy$YEAR > "1979" ,]%>%
  mutate(approx = na.approx(MAX))

SG_analy<-SG_analy%>%mutate( ., Year=ifelse(YEAR>=1980&YEAR<=1989,"1980-1989",ifelse(YEAR>=1990&YEAR<=1999,
                                                                                     "1990-1999",ifelse(YEAR>=2000&YEAR<=2009,
                                                                                                        "2000-2009",ifelse(YEAR>=2010&YEAR<=2019,
                                                                                                                           "2010-2019",0)))))





SG_analy$N=ave(1:length(SG_analy$YEAR ), SG_analy$YEAR , FUN = seq_aloSG)
#Colby <- na.omit(Colby) 
SG_analyDAY=SG_analy%>%group_by(Year)%>%summarise(max =n())
SG_analy$anomaly=as.numeric(as.character((SG_analy$approx)))


SG_analy<-SG_analy%>% group_by(Year)%>%within(., {mean1 = ave(approx,Year,FUN=mean)} )%>%within(., {sd1 = ave(approx,Year,FUN=sd)} )

##usiSG 30-year deviation to standardized
years_30<-SG_analy[SG_analy$YEAR > "1979" &SG_analy$YEAR<= "2009",]
mean2=mean(years_30$approx)
sd2=sd(years_30$approx)
SG_analy$anomaly=as.numeric(as.character((SG_analy$approx-mean2)/sd2))
#SG_analy$anomaly2<-scale(SG_analy$approx)
##usiSG base-year deviation to standardized
SG_analy$anomaly2=as.numeric(as.character((SG_analy$approx-SG_analy$mean1)/SG_analy$sd1))
SG_analy$Nas=ave(1:leSGth(SG_analy$Year ), SG_analy$Year, FUN = seq_aloSG)

SG_analy_30<-SG_analy%>%group_by(anomaly,Year)%>%summarise(count= n())
SG_analy_base<-SG_analy%>%group_by(anomaly2,Year)%>%summarise(count= n())



SG_analy_30=SG_analy_30%>%mutate( .,  Change=ifelse(anomaly>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)%>%mutate( .,count_change=100*count/total)
SG_analy_base=SG_analy_base%>%mutate( .,  Change=ifelse(anomaly2>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)%>%mutate( .,count_change=100*count/total)
arrows <- 
  tibble(
    x1 = c(2.45, 2.75),
    x2 = c(1.8, 1.9),
    y1 = c(2.9,2.5),
    y2 = c(1.85, 1.7)
  )

mycolor<-c("#fecc5c", "#fd8d3c", "#f03b20","#bd0026")

###30-years standardization
(Plot_30<-ggplot(data = SG_analy_30, aes(x = anomaly, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized Difference from 1980-2009 Mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)

###base-10year standardization
(Plot_base<-ggplot(data = SG_analy_base, aes(x = anomaly2, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized difference based on each period mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)





##################Rajshahi, Bangladesh
library("GSODR") 

  
BG_station <- nearest_stations(LAT = 24.413124,
                                  LON = 88.610133,
                                  distance = 100)
BG_data <- get_GSOD(years = 1979:2019, station = NG_station)
BG_analy<-BG_data%>%subset( ., select =c(YEAR, MONTH,DAY,MAX))%>%subset(., MONTH>=3 & MONTH<=5)


BG_analy<-BG_analy[BG_analy$YEAR > "1979" ,]%>%
  mutate(approx = na.approx(MAX))

BG_analy<-BG_analy%>%mutate( ., Year=ifelse(YEAR>=1980&YEAR<=1989,"1980-1989",ifelse(YEAR>=1990&YEAR<=1999,
                                                                                     "1990-1999",ifelse(YEAR>=2000&YEAR<=2009,
                                                                                                        "2000-2009",ifelse(YEAR>=2010&YEAR<=2019,
                                                                                                                           "2010-2019",0)))))





BG_analy$N=ave(1:leBGth(BG_analy$YEAR ), BG_analy$YEAR , FUN = seq_aloBG)
#Colby <- na.omit(Colby) 
BG_analyDAY=BG_analy%>%group_by(Year)%>%summarise(max =n())
BG_analy$anomaly=as.numeric(as.character((BG_analy$approx)))


BG_analy<-BG_analy%>% group_by(Year)%>%within(., {mean1 = ave(approx,Year,FUN=mean)} )%>%within(., {sd1 = ave(approx,Year,FUN=sd)} )

##usiBG 30-year deviation to standardized
years_30<-BG_analy[BG_analy$YEAR > "1979" &BG_analy$YEAR<= "2009",]
mean2=mean(years_30$approx)
sd2=sd(years_30$approx)
BG_analy$anomaly=as.numeric(as.character((BG_analy$approx-mean2)/sd2))
#BG_analy$anomaly2<-scale(BG_analy$approx)
##usiBG base-year deviation to standardized
BG_analy$anomaly2=as.numeric(as.character((BG_analy$approx-BG_analy$mean1)/BG_analy$sd1))
BG_analy$Nas=ave(1:leBGth(BG_analy$Year ), BG_analy$Year, FUN = seq_aloBG)

BG_analy_30<-BG_analy%>%group_by(anomaly,Year)%>%summarise(count= n())
BG_analy_base<-BG_analy%>%group_by(anomaly2,Year)%>%summarise(count= n())



BG_analy_30=BG_analy_30%>%mutate( .,  ChaBGe=ifelse(anomaly>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(ChaBGe,Year,FUN=sum)} )%>%
  mutate( .,per_chaBGe=100*total_c/total)%>%mutate( .,count_change=100*count/total)
BG_analy_base=BG_analy_base%>%mutate( .,  ChaBGe=ifelse(anomaly2>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(ChaBGe,Year,FUN=sum)} )%>%
  mutate( .,per_chaBGe=100*total_c/total)%>%mutate( .,count_change=100*count/total)

arrows <- 
  tibble(
    x1 = c(2.45, 2.75),
    x2 = c(1.8, 1.9),
    y1 = c(2.9,2.5),
    y2 = c(1.85, 1.7)
  )

mycolor<-c("#fecc5c", "#fd8d3c", "#f03b20","#bd0026")
###30-years standardization
(Plot_30<-ggplot(data = BG_analy_30, aes(x = anomaly, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized Difference from 1980-2009 Mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)

###base-10year standardization
(Plot_base<-ggplot(data = BG_analy_base, aes(x = anomaly2, y = count_change,color = as.factor(Year))) +
    geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4,5),breaks = seq(-4,5,1),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,3.5,0.5),limits=c(0,3.5))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly\n(Standardized difference based on each period mean)  ",
                                                     y = "Percent frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 0, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    scale_color_manual(values=mycolor)
)

#############Dont run

##########################stop-need use the code below later

#############Dont run

(ggplot(Colby, aes(x=anomaly, fill=Year)) +
    #geom_density()+
    geom_vline(xintercept = 1, size = 1.5, 
               show_guide = FALSE,linetype="dashed")+
    geom_histogram(position = "identity", stat = 'bin',binwidth=1,alpha=0.6)
)


+
  geom_vline(xintercept = 1, size = 0.5, 
             show_guide = FALSE,linetype="dashed")+
  annotate(
    "text", x = -4, y = 11, family = "Poppins", size = 3, color = "gray20", lineheight = .9, 
    label = "Total number of days=1288\n" )+
  annotate(
    "text", x =2.8, y = 3, family = "Poppins", size = 3.2, color = "gray20", lineheight = .9, 
    label =  "21.74% of days with anomaly>1\u00B0C"  )
+
  annotate(
    "text", x =3.4, y = 2.5, family = "Poppins", size = 3.2, color = "gray20", lineheight = .9, 
    label =  "13.04% of days with anomaly>1\u00B0C" )
+ 
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.2
  ) + theme(legend.position=c(0.85, 0.85))

(Plot<-ggplot(data = Colby, aes(x = anomaly, y = count,color = Year)) +
    #geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-4.5,3),breaks = seq(-4.5,3,1.5),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,12,2),limits=c(0,12))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly (Standardized)  ",
                                                     y = "Frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 1, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    annotate(
      "text", x = -4, y = 11, family = "Poppins", size = 3, color = "gray20", lineheight = .9, 
      label = "Total number of days=1288\n" )+
    annotate(
      "text", x =2.8, y = 3, family = "Poppins", size = 3.2, color = "gray20", lineheight = .9, 
      label =  "11.96% of days with anomaly>1\u00B0C"  )
  +
    annotate(
      "text", x =2.8, y = 4, family = "Poppins", size = 3.2, color = "gray20", lineheight = .9, 
      label =  "16.77% of days with anomaly>1\u00B0C" )
  +
    annotate(
      "text", x =2.8, y = 8, family = "Poppins", size = 3.2, color = "gray20", lineheight = .9, 
      label =  "17.94% of days with anomaly>1\u00B0C" )+ 
    geom_curve(
      data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
      arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
      color = "gray20", curvature = -0.2
    ) + theme(legend.position=c(0.85, 0.85))
)


(ggplot(Colby, aes(x=anomaly, fill=Year)) +
    #geom_density()+
    geom_vline(xintercept = 1, size = 1.5, 
               show_guide = FALSE,linetype="dashed")+
    geom_histogram(position = "identity", stat = 'bin',binwidth=1,alpha=0.6)
)


(Plot<-ggplot(data = Colby, aes(x = anomaly, y = count,color = Year)) +
    #geom_point()+
    #geom_smooth(se=F)+
    stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),se=F) +
    
    scale_x_continuous(limits=c(-3.5,3.5),breaks = seq(-3.50,3.5,1.5),
                       labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = '.'))+
    
    scale_y_continuous(breaks = seq(0,4,1),limits=c(0,4))+
    theme(axis.title  = element_text(size = 13))+
    theme(axis.text = element_text(size = 10))+ labs(x = "Monthly tempearture Anomaly (Standardized)  ",
                                                     y = "Frequency of occurence",colour = "Year")+ theme_light() +
    geom_vline(xintercept = 1, size = 0.5, 
               show_guide = FALSE,linetype="dashed")+
    annotate(
      "text", x = -4, y = 11, family = "Poppins", size = 3, color = "gray20", lineheight = .9, 
      label = "Total number of days=1288\n" )+
    annotate(
      "text", x =2.8, y = 3, family = "Poppins", size = 3.2, color = "gray20", lineheight = .9, 
      label =  "21.74% of days with anomaly>1\u00B0C"  )
  +
    annotate(
      "text", x =3.4, y = 2.5, family = "Poppins", size = 3.2, color = "gray20", lineheight = .9, 
      label =  "13.04% of days with anomaly>1\u00B0C" )
  + 
    geom_curve(
      data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
      arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
      color = "gray20", curvature = -0.2
    ) + theme(legend.position=c(0.85, 0.85))
)


(ggplot(Colby, aes(x=anomaly, fill=Year)) +
    #geom_density()+
    geom_vline(xintercept = 1, size = 1.5, 
               show_guide = FALSE,linetype="dashed")+
    geom_histogram(position = "identity", stat = 'bin',binwidth=1,alpha=0.6)
)
