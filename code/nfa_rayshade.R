# rayshade experiment on NFA

# see forlorn hope expedition
#https://www.forlornhope.org/
# https://www.forlornhope.org/research-resources/
# https://share.garmin.com/MTN200S

library(rayshader)
library(dplyr)
library(elevatr)
library(sf)
library(mapedit)
library(mapview)
mapviewOptions(fgb=FALSE)

# Get nhd data -----------------------------------------------
nfa_riv <- st_read("data/nfa_nhd.gpkg", "flowline")
nfa_basin <- st_read("data/nfa_nhd.gpkg", "basin")
nfa_out <- st_read("data/nfa_nhd.gpkg", "outlet")

# get bbox
nfa_bb <- st_bbox(nfa_basin)

# map
#nfa_base <- tmaptools::read_osm(nfa_riv, type="esri-topo", raster=TRUE)


# Trace a Track -----------------------------------------------------------

# m1 <- mapview(nfa_riv)
# 
# m1e <- mapedit::drawFeatures(m1)
# m1e <- m1e@object
# 
# nfa_trk <- m1shp %>% st_sf() %>% 
#   mutate(id = row_number())
# 
# # merge lines
# nfa_trk <- filter(nfa_trk, id %in% c(3,4,5))
# mapview(nfa_trk)
# 
# nfa_trk1 <- st_combine(nfa_trk)
# mapview(nfa_trk1)
# 
# save(nfa_trk1, file = "data_output/forlorn_hope_upper_trail.rda")

# Make Area for DEM Download ----------------------------------------------

# use sf to create area of interest
nfa_area_sf <- nfa_basin %>% 
  st_transform(3310) %>% st_bbox(nfa_basin) %>% st_as_sfc() %>% st_buffer(1000)

# make sp
nfa_area_sp <- nfa_area_sf %>% sf::as_Spatial()

# Get Elevatr DEM --------------------------------------------

(nfaproj4 <- st_crs(nfa_area_sf)$proj4string)

# get dem data
dem <- elevatr::get_elev_raster(locations = nfa_area_sp, 
                                prj = nfaproj4, 
                                z = 11, clip = "bbox")


# Make Hillshade Raster ---------------------------------------------------

# create matrix
# elmat <- matrix(
#   raster::extract(dem, raster::extent(dem)),
#   nrow = ncol(dem), 
#   ncol = nrow(dem)
# )

# convert to matrix
elmat <- raster_to_matrix(dem)

# We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  plot_map()

# make the ray shade w ambient shading
#raymat <- ray_shade(elmat)
#ambmat <- ambient_shade(elmat)

# make a hillshade
hillshade <- elmat %>%
  sphere_shade(texture = "imhof4", sunangle = 45) %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0)

plot_map(hillshade)

# Transform hillshade to raster -------------------------------------------

# transform hillshade to georeferenced raster
e <- raster::extent(dem)
hillshade_raster <- raster::brick(hillshade, xmn = e[1], xmx = e[2], ymn = e[3], ymx = e[4], crs = raster::crs(dem))

class(hillshade_raster)
raster::writeRaster(hillshade_raster, filename = "data_output/nfa_hillshade_z12.tif")


# 3d plot -----------------------------------------------------------------

hillshade %>% 
  plot_3d(heightmap = elmat, baseshape = "rectangle",
          zscale = 10, fov = 0, theta = 320, 
          zoom = .8, phi = 45, windowsize = c(1000, 800))

# get pt for label
(snow_mountain <- st_sfc(st_point(c(-120.46397, 39.24116))) %>%
  st_set_crs(4326) %>% 
  st_sf() %>% st_transform(3310) %>% 
  st_coordinates())
  
# render
render_label(elmat, long = snow_mountain[1], lat=snow_mountain[2], 
             z=3000, extent = e, 
             zscale = 10, 
             text = "Snow Mountain", 
             textsize = 1.5, linewidth = 2, 
             linecolor = "white",
             textcolor = "black",
             dashed = TRUE)

Sys.sleep(0.2)

# save to static
render_snapshot(clear=TRUE)

#devtools::install_github("tylermorganwall/rayrender")
#library(rayrender)
#render_highquality(samples=200, clear=TRUE)
#rayshader::save
# save_3dprint("nfa_3d.stl", maxwidth = 4, unit = "in")


# Plot with ggplot2 -------------------------------------------------------

library(ggplot2)
library(ggthemes)


# plot
ggplot() +
  ggspatial::layer_spatial(hillshade_raster) +
  geom_sf(data=nfa_basin, color = "darkblue", fill = NA) +
  geom_sf(data=nfa_riv, color="skyblue", lwd=.25, alpha=0.9) +
  theme_minimal()



