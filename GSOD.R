
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
  GSODR # download PRISM data
)   

#code starts here
# the end bound


          library("GSODR") 
  ( ks<-  nearest_stations(39.382214, -101.046194, 100) )   
       
ks <- get_GSOD(years = 1976:2019, station = "724650-23065",max_missing=1)

Colby<-ks%>%subset( ., select =c(YEAR, MONTH,DAY,MAX))%>%subset(., MONTH>=3 & MONTH<=5)


Colby<-Colby[Colby$YEAR > "1976",]%>%
  mutate(approx = na.approx(MAX))

Colby<-Colby%>%mutate( ., Year=ifelse(YEAR>=1977&YEAR<=1990,"1976-1990",ifelse(YEAR>=1991&YEAR<=2004,
                                                                               "1991-2004",ifelse(YEAR>=2005&YEAR<=2018,
                                                                                                 "2005-2018",0))))




Colby$N=ave(1:length(Colby$YEAR ), Colby$YEAR , FUN = seq_along)
#Colby <- na.omit(Colby) 
ColbyDAY=Colby%>%group_by(Year)%>%summarise(max =n())


#, Colby$YEAR , FUN =MAX)))%>% group_by(Year)%>%within(., {sd = ave(Day,Year,FUN=sum)} )%>%group_by(Year,sd)%>%summarise(DAY= n())
Colby<-Colby%>%
  group_by(Year)%>%within(., {mean = ave(approx,Year,FUN=mean)} )%>%within(., {sd = ave(approx,Year,FUN=sd)} )



Colby$anomaly<-as.numeric(as.character(scale(Colby$approx)))

Colby$Nas=ave(1:length(Colby$Year ), Colby$Year, FUN = seq_along)

Colby<-Colby%>%group_by(anomaly,Year)%>%summarise(count= n())




Colby=Colby%>%mutate( .,  Change=ifelse(anomaly>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
mutate( .,per_change=100*total_c/total)

arrows <- 
  tibble(
    x1 = c(2.45, 2.85,2.85),
    x2 = c(2.05, 2.05,1.85),
    y1 = c(2.9,3.9,7.8),
    y2 = c(2, 3.4,7)
  )


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


##################Niger_Maradi
library("GSODR") 
( ks<-  nearest_stations(13.709462, -13.693913, 100) )   

ks <- get_GSOD(years = 1979:2019, station = "617310-99999",max_missing=1)

Colby<-ks%>%subset( ., select =c(YEAR, MONTH,DAY,MAX))%>%subset(., MONTH>=7 & MONTH<=9)


Colby<-Colby[Colby$YEAR > "1978" &Colby$YEAR< "2019",]%>%
  mutate(approx = na.approx(MAX))

Colby<-Colby%>%mutate( ., Year=ifelse(YEAR>=1979&YEAR<=1990,"1976-1990",ifelse(YEAR>=1991&YEAR<=2004,
                                                                               "1991-2004",ifelse(YEAR>=2005&YEAR<=2018,
                                                                                                  "2005-2018",0))))




Colby$N=ave(1:length(Colby$YEAR ), Colby$YEAR , FUN = seq_along)
#Colby <- na.omit(Colby) 
ColbyDAY=Colby%>%group_by(Year)%>%summarise(max =n())


#, Colby$YEAR , FUN =MAX)))%>% group_by(Year)%>%within(., {sd = ave(Day,Year,FUN=sum)} )%>%group_by(Year,sd)%>%summarise(DAY= n())
Colby<-Colby%>%
  group_by(Year)%>%within(., {mean = ave(approx,Year,FUN=mean)} )%>%within(., {sd = ave(approx,Year,FUN=sd)} )%>%subset(., Year=="1976-1990"|Year=="2005-2018")



Colby$anomaly<-as.numeric(as.character(scale(Colby$approx)))

Colby$Nas=ave(1:length(Colby$Year ), Colby$Year, FUN = seq_along)

Colby<-Colby%>%group_by(anomaly,Year)%>%summarise(count= n())




Colby=Colby%>%mutate( .,  Change=ifelse(anomaly>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)

arrows <- 
  tibble(
    x1 = c(2.45, 2.75),
    x2 = c(1.8, 1.9),
    y1 = c(2.9,2.5),
    y2 = c(1.85, 1.7)
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

##################Rajshahi, Bangladesh
library("GSODR") 
( station<-  nearest_stations(24.387498, 88.586274, 100) )   

ks <- get_GSOD(years = 1976:2018, station ="426030-99999",max_missing=1)

Colby<-ks%>%subset( ., select =c(YEAR, MONTH,DAY,MAX))%>%subset(., MONTH>=3 & MONTH<=5)


Colby<-Colby[Colby$YEAR > "1981" &Colby$YEAR< "2019",]%>%
  mutate(approx = na.approx(MAX))

Colby<-Colby%>%mutate( ., Year=ifelse(YEAR>=1979&YEAR<=1990,"1976-1990",ifelse(YEAR>=1991&YEAR<=2004,
                                                                               "1991-2004",ifelse(YEAR>=2005&YEAR<=2018,
                                                                                                  "2005-2018",0))))




Colby$N=ave(1:length(Colby$YEAR ), Colby$YEAR , FUN = seq_along)
#Colby <- na.omit(Colby) 
ColbyDAY=Colby%>%group_by(Year)%>%summarise(max =n())


#, Colby$YEAR , FUN =MAX)))%>% group_by(Year)%>%within(., {sd = ave(Day,Year,FUN=sum)} )%>%group_by(Year,sd)%>%summarise(DAY= n())
Colby<-Colby%>%
  group_by(Year)%>%within(., {mean = ave(approx,Year,FUN=mean)} )%>%within(., {sd = ave(approx,Year,FUN=sd)} )%>%subset(., Year=="1976-1990"|Year=="2005-2018")



Colby$anomaly<-as.numeric(as.character(scale(Colby$approx)))

Colby$Nas=ave(1:length(Colby$Year ), Colby$Year, FUN = seq_along)

Colby<-Colby%>%group_by(anomaly,Year)%>%summarise(count= n())




Colby=Colby%>%mutate( .,  Change=ifelse(anomaly>=1,count,0))%>%within(., {total = ave(count,Year,FUN=sum)} )%>%within(., {total_c = ave(Change,Year,FUN=sum)} )%>%
  mutate( .,per_change=100*total_c/total)

arrows <- 
  tibble(
    x1 = c(2.45, 2.75),
    x2 = c(1.8, 1.9),
    y1 = c(2.9,2.5),
    y2 = c(1.85, 1.7)
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



