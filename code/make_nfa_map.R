

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)  
library(tmap)    # for static and interactive maps
library(tmaptools)
library(viridis)
library(USAboundaries)
library(ggspatial)
library(ggthemes)
library(hrbrthemes)

# download package to interact with natural earth data
#devtools::install_github("ropenscilabs/rnaturalearth")
#devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
#                 repos = "http://packages.ropensci.org",
#                 type = "source")
library(rnaturalearth)


# Base US Map -------------------------------------------------------------

west_states<- us_states(states = c("OR","NV","AZ","ID")) %>% 
  st_transform(2163)

# get CA only
ca <- us_states(states="CA") %>% st_transform(2163)


# make basic map
(us_states_map <- 
    tm_shape(ca) +
    tm_polygons(border.col = "black", lwd=1.2) +
    tm_shape(west_states) +
    tm_polygons(border.col = "gray", col = NA, alpha = 0.2) + 
    tm_layout(frame = FALSE)+
    tm_shape(ca) +
    tm_polygons(border.col = "black", lwd=1.2))

# GET RIVER DATA ----------------------------------------------------------------

#rivers
rivs_sf <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf") %>% st_transform(2163)

# st_intersects to us/ca. Can use st_touches, st_within, etc
rivs_sf_ca <- rivs_sf[ca, , op=st_intersects]

# make basic map
us_states_map + tm_shape(rivs_sf_ca) + tm_lines(col="blue")

