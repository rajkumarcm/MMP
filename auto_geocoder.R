# Code used to automatically geocode
source('geocoder_helper.R', local=T)
library(tidygeocoder)

# This code expects df_nodes loaded and found in the environment

locations <- unique(df_nodes[, c('hq_city', 'hq_province', 'hq_country')])
addresses <- encode_address(locations)

lat_longs <- addresses %>%
       geocode(addr, method = 'osm', lat = latitude , long = longitude)