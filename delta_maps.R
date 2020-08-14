### Maps for Lower Mississippi River Alluvial Plain

# Contributes to studies of weather, agriculture, and groundwater/surfacewater projects

# Clear Environment and Load Functional Libraries/Dependencies
rm(list=ls())
get <- c("RColorBrewer","raster","sf","ggplot2", "GSODR","tidyverse")
have   <- get %in% rownames(installed.packages())
if(any(!have)) install.packages(get[!have])
sapply(get, function(i) require(i, character.only=TRUE))
options(digits = 6)


# Get shapefiles
us <-  getData("GADM", country="USA", path= "Data", level=1)
ar
ear

# Get ag data



# Get water data



# County Map



# Watershed Map



# Raster Map
