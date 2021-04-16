# mosaic raster

library(sf)
library(raster)
library(tidyverse)
library(fs)
library(gdalUtils)
library(stars)

# List GeoTiffs -----------------------------------------------------------


tifs <- fs::dir_ls("~/Downloads/all_tifs/", glob = "*TM_geo.tif")
tifs

tif_list <- as.vector(tifs)
tif_list

# check proj
proj4string(raster(tif_list[1]))

# set extent
bbox <- st_read("~/Downloads/bbox.kml", "bbox.kml")
bbox <- st_zm(bbox)
st_crs(bbox)
st_write(bbox, "~/Downloads/bbox_quads.shp")

bbox_ext <- st_bbox(bbox)
e <- extent(bbox_ext[[1]], bbox_ext[[3]], bbox_ext[[2]], bbox_ext[[4]])
template <- raster(e)

# set the projection
proj4string(template) <- "+proj=tmerc +lat_0=0 +lon_0=-121.3125 +k=0.9996 +x_0=500000 +y_0=0 +datum=NAD83 +units=m +no_defs"

# write template raster
writeRaster(template, file="data_output/bbox_nfa.tif", format="GTiff")


# now mosaic in
mosaic_rasters(gdalfile=tif_list, dst_dataset="data_output/bbox_nfa.tif", of="GTiff")

# check it worked
gdalinfo("data_output/bbox_nfa.tif")

a1 <- raster::stack("data_output/bbox_nfa.tif")

# check layers
a1@layers

# pull out as DF
a1_df <- as.data.frame(a1, xy = TRUE)

str(a1_df) # check layers

# plot one layer w ggplot
ggplot() +
  geom_raster(data = a1_df,
              aes(x = x, y = y, alpha = bbox_nfa.3)) + 
  coord_quickmap()

# print stack
plotRGB(a1,
        r = 1, g = 2, b = 3)
