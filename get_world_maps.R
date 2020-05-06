library(dplyr)
library(maps)


get_world_map <- function(s){
  world_map <- map_data(s)
  world_map$country = world_map$region
  
  world_map[world_map$region == "Grenadines","country"] <- "Saint Vincent and the Grenadines"
  world_map[world_map$region == "Saint Vincent","country"] <- "Saint Vincent and the Grenadines"
  world_map[world_map$region == "Antigua","country"] <- "Antigua and Barbuda"
  world_map[world_map$region == "Barbuda","country"] <- "Antigua and Barbuda"
  
  world_map[world_map$region == "Aruba","country"] <- "Netherlands"
  world_map[world_map$region == "Curacao","country"] <- "Netherlands"
  world_map[world_map$region == "Bonaire","country"] <- "Netherlands"
  world_map[world_map$region == "Sint Eustatius","country"] <- "Netherlands"
  world_map[world_map$region == "Saba","country"] <- "Netherlands"
  world_map[world_map$region == "Sint Maarten","country"] <- "Netherlands"
  
  world_map[world_map$region == "Anguilla","country"] <- "UK"
  world_map[world_map$region == "Bermuda","country"] <- "UK"
  world_map[world_map$region == "Falkland Islands","country"] <- "UK"
  world_map[world_map$region == "Chagos Archipelago","country"] <- "UK"
  world_map[world_map$region == "Pitcairn Islands","country"] <- "UK"
  world_map[world_map$region == "South Sandwich Islands","country"] <- "UK"
  world_map[world_map$region == "Saint Helena","country"] <- "UK"
  world_map[world_map$region == "Ascension Island","country"] <- "UK"
  world_map[world_map$region == "Turks and Caicos Islands","country"] <- "UK"
  
  world_map[world_map$region == "French Southern and Antarctic Lands","country"] <- "France"
  world_map[world_map$region == "Saint Barthelemy","country"] <- "France"
  world_map[world_map$region == "Reunion","country"] <- "France"
  world_map[world_map$region == "Mayotte","country"] <- "France"
  world_map[world_map$region == "French Guiana","country"] <- "France"
  world_map[world_map$region == "Martinique","country"] <- "France"
  world_map[world_map$region == "Guadeloupe","country"] <- "France"
  world_map[world_map$region == "Saint Martin","country"] <- "France"
  world_map[world_map$region == "New Caledonia","country"] <- "France"
  world_map[world_map$region == "French Polynesia","country"] <- "France"
  world_map[world_map$region == "Saint Pierre and Miquelon","country"] <- "France"
  world_map[world_map$region == "Wallis and Futuna","country"] <- "France"
  
  world_map[world_map$region == "Canary Islands","country"] <- "Spain"
  world_map[world_map$region == "Montserrat","country"] <- "Spain"
  
  world_map[world_map$region == "Azores","country"] <- "Portugal"
  
  world_map[world_map$region == "Guam","country"] <- "USA"
  world_map[world_map$region == "Puerto Rico","country"] <- "USA"
  
  world_map[world_map$region == "Heard Island","country"] <- "Australia"
  world_map[world_map$region == "Cocos Islands","country"] <- "Australia"
  world_map[world_map$region == "Christmas Island","country"] <- "Australia"
  world_map[world_map$region == "Norfolk Island","country"] <- "Australia"
  
  world_map[world_map$region == "Siachen Glacier","country"] <- "India"
  
  world_map[world_map$region == "Trinidad","country"] <- "Trinidad and Tobago"
  world_map[world_map$region == "Tobago","country"] <- "Trinidad and Tobago"
  return(world_map)
}