# ag_water_delta

This initial repo hosts code for the Arkansas Groundwater Initiative project. We're mapping groundwater and estimating models to investigate the impacts of conservation.

######***Data download and wrangling
######**Weather related data dependen on Prism.
#* Prism_data_county
This code downloads the weather information (precipitation, minimum and maximum temperature) 
from PRISM and extract information for counties in the AR Delta. Line 186-199 obtains the centroid for AR Delta.
#* outcome
There are three data generated from the code-precipitation, minimum and maximum temperature.

#* Degree_days_county
This code is executed once the Prism_data_county is completed. 
This code calculates the daily degree days (-10 to 39 degree celcius) and temperature bins from -10 to 39 degree celcius at 3 degree between 1981-2019 for the AR Delta.
#* outcome
The data generated consist date, countyname, countyfp10,latitude, longitude, tavg, ppt, min temp, max temp, degree days and bins.

#* ET_VPD_county
 This code is executed once the Prism_data_county is completed. This code calculates Penman evapotransipiration and vapor pressure density. The elevation used in the estimation of ET is obtain from SSURGO.

######*****Nass related data- Cropland layer Data.

I used the code below to download the CDL layers. 
######Download CDL*******************#

(
  AR_cdl<- getCDL(5, 2006:2019,location="E:/transfer/cdl")
)

#*acreage_share_county
This code is used to share of corn, rice, soybean, cotton and sorghum in each of the counties in AR Delta.
The function in the code do the following; cropping, masking and extraction of relevant informatiom. We masked the noncropland in the raster using the 2016 NLCD raster. For the 2006-2007 download, the process is alittle bit different as we have to resample the raster as the resolution for the raster between 2006-2007 is 56m which is different for the resolution of rasters between 2008-2019. 
#* outcome
This code generates the share of major crops, crop acres, and the total cropland within each county. 
***This procedure is time consuming

#*Crop_rotation
This code calculates the share of rice that is rotated within each county of the AR Delta. This majority of the process explained above in the acreage_share_county code plus classification, overlay of rasters and extraction of information. The first step involves cropping, masking and reclassification. The second step involves the use of overlay function in raster to determine if there is a change in the rasters.
#* outcome
This code generated the share of rice within each county.
######*****Soil data from gSSURGO and SSurgo
This code extracts county soil variables from gSSURGO and SSurgo. We used the NLCD 2016 to mask noncropland areas. It involves the resampling of soil raster from 10-(soil_Delta) to 30-(nlcd) using nearest neighbor using the nearest neighbor. The Statgo and SSurgo csv files, and the STATA code used in creating some of these variables can be provided upon request.
#* outcome
Soil data
######*****Cost Share
#*Iwm_costshare_county_updated
This code calculated the total cost share by county. There second portion of the code is not to be ran.
#* outcome
Total cost share of all practices related to IWM per county in the AR Delta
######*****Cropland return

The estimation was done in Stata. Code and data can be provided upon request.

######*****Average saturated thickness
#*Saturated_thickness_Torak
This code extracts and calculate the county average saturated thickness from Torak and Painter (2019)

######*****Merging of data
#*Costshare_merged_data
This code brings all the outcome together. Once the data are merged together, we can use the data for estimation. 
Table 3 is also created by running a line of code in line 110.

######*****Figure
#*r_s_intensification_fiqure
This code calculates the share of rice and soybean between 2010-2019 that is rotated within each county of the AR Delta. This majority of the process explained above in the acreage_share_county code plus classification, overlay of rasters and extraction of information. The first step involves cropping, masking and reclassification. The second step involves the extraction of rice and soybeans within each county and the share of rice or soybean rotated.
The third portion of the code produces figure 2 in the manuscript

#***Map
This folder consist of data and code used in producing the maps in the manuscript.

#***codes not added
I have not included the code I used to generate the rice frequency between 2009-2019.
This will be updated and provided soon.
Data wrangling for figure 3 was done in Stata.
#*Interpolation_code 
This code is used to conduct IDW and kriging interpolation. The raster file produced using the IDW technique for Fall 2019  is used to make Figure 1A in the manuscript.

