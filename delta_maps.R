### Maps for Lower Mississippi River Alluvial Plain

# Contributes to studies of weather, agriculture, and groundwater/surfacewater projects

# Clear Environment and Load Functional Libraries/Dependencies
rm(list=ls())
get <- c("raster", "sf", "GSODR", "tidyUSDA",
         "vtable", "tidyverse", "tools", "broom", "dplyr", "readr",
         "ggplot2","RColorBrewer","mapproj", "scales", "ggmap", "viridis")
have   <- get %in% rownames(installed.packages())
if(any(!have)) install.packages(get[!have])
sapply(get, function(i) require(i, character.only=TRUE))
options(digits = 6)


# Get Arkansas shapefile
us <-  getData("GADM", country="USA", path= "Data", level=2)
ar <- us[us$NAME_1=="Arkansas",]

# Get county-level rice data
ar_rice_area <- getQuickstat(key = "2E139E04-368D-39C4-997A-1BBD0864B878",
                        program = "SURVEY", commodity = "rice", category = "area harvested",
                        geographic_level = "county", state = "Arkansas")

ar_rice_prod <- getQuickstat(key = "2E139E04-368D-39C4-997A-1BBD0864B878",
                        program = "SURVEY", commodity = "rice", category = "production",
                         geographic_level = "county", state = "Arkansas")

ar_rice <- rbind(ar_rice_area, ar_rice_prod)
ar_rice <- ar_rice[ar_rice$year>2000,]

ar_rice_w <- pivot_wider(ar_rice,
  names_from = c(statisticcat_desc, year),
  values_from = Value
)


names(ar_rice_area)[names(ar_rice_area)=="Value"] <- "area_ac"
names(ar_rice_prod)[names(ar_rice_prod)=="Value"] <- "prod_cwt"
ar_rice <- merge(ar_rice_area, ar_rice_prod, by="county_name")
ar_rice <- ar_rice[,c("county_name", "area_ac", "prod_cwt")]
ar_rice$county_name <- toTitleCase(tolower(ar_rice$county_name))
ar@data <- merge(ar@data, ar_rice, by.x="NAME_2", by.y="county_name", all.x=T)

shapefile(ar, filename='Data/ar_rice_2018.shp')

ar@data$id <- rownames(ar@data)
newar <- fortify(ar, region="id")
p_df <- merge(newar, ar@data, by="id")
p <- ggplot() +
  geom_polygon(data=p_df, aes(fill = area_ac, x = long, y = lat, group=group)) +
  theme_nothing(legend = TRUE) + coord_map() +
  ggtitle("Rice acres by county in Arkansas, 2018") + 
  theme(plot.title = element_text(hjust =0.5))

p + scale_fill_viridis(option = "viridis", direction = -1)




# Get water data



# County Map



# Watershed Map



# Raster Map
