

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
library(mapview)

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



# Get NHDPlus Info --------------------------------------------------------

# nfa
library(nhdplusTools)

# get gage comid
nfa_comid <- nhdplusTools::discover_nhdplus_id(nldi_feature = list(featureSource="nwissite", featureID="USGS-11427000"))

# get XY comid
nfa_pt <- st_sfc(st_point(c(-120.91986, 39.10665)), crs = 4326) %>% st_as_sf()

# now use this to get info upstream
discover_nhdplus_id(nfa_pt) # 14992581

# or plot upstream from a single location
nfa_flowline <- nhdplusTools::plot_nhdplus(outlets = nfa_pt,
                                           streamorder=1,
                                           gpkg = "data/nfa_nhd.gpkg",
                                           actually_plot = TRUE)

# st_write out pieces: outlets
st_write(nfa_flowline$outlets, dsn = "data/nfa_nhd.gpkg", layer = "outlet")

# st_write: the flowline 
st_write(nfa_flowline$flowline, dsn = "data/nfa_nhd.gpkg", layer = "flowline", delete_layer = TRUE)

# st_write: the basin
st_write(nfa_flowline$basin, dsn="data/nfa_nhd.gpkg", layer="basin", delete_layer = TRUE)

# check layers
st_layers("data/nfa_nhd.gpkg")


# tmap Plot of nhd data ---------------------------------------------------
nfa_riv <- st_read("data/nfa_nhd.gpkg", "flowline")
nfa_basin <- st_read("data/nfa_nhd.gpkg", "basin")
nfa_out <- st_read("data/nfa_nhd.gpkg", "outlet")

# map
nfa_base <- tmaptools::read_osm(nfa_riv, type="esri-topo", raster=TRUE)

# basemap
(map1 <- tm_shape(nfa_base) + tm_rgb())
(map2 <- map1 + 
    tm_shape(nfa_riv) + tm_lines(col="blue") +
    tm_shape(nfa_basin) + tm_polygons(border.col = "steelblue",alpha = 0, border.alpha = 0.8) +
    tm_shape(nfa_out) + tm_dots(col="black", shape=19) +
    tm_compass(size = 3, type="8star", position=c("right", "bottom"), show.labels = 2, color.light = "transparent") +
    tm_scale_bar(position=c("right", "bottom")) +
    tm_legend(title="NF American:\nUpstream of Iowa Hill Bridge",fontfamily="Bodoni 72 Smallcaps Book", title.size=10)
)

## fun fonts:
# Bodoni 72 Smallcaps Book (oldtimey)
# Irish Grover Regular

tmap_save(map2, filename = "figs/nfa_base_map_flowlines.png", width = 11, height = 8, units = "in", dpi = 300)



# Mapview Version ---------------------------------------------------------
mapviewOptions(fgb=FALSE)
nfa_riv_simple <- nfa_riv %>% select(id:Hydroseq)

mapview(nfa_basin, layer.name="Basin", color="darkblue", lwd=2,  alpha.regions=0) +
  mapview(nfa_riv_simple, color="dodgerblue", alpha=0.8, layer.name="Streamlines") +
  mapview(nfa_out, col.region="maroon", alpha.regions=0.8,color="black", cex=5, layer.name="Study Site")
