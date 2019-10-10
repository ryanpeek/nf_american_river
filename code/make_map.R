

# Libraries ---------------------------------------------------------------

library(dplyr)
library(sf)  
library(ggplot2)
library(tmap)    # for static and interactive maps
library(viridis)
#library(spData) # contains datasets used in this example
#library(spDataLarge)
library(grid)
library(USAboundaries)
library(ggspatial)
library(ggthemes)


# download package to interact with natural earth data
#devtools::install_github("ropenscilabs/rnaturalearth")
#devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
#                 repos = "http://packages.ropensci.org",
#                 type = "source")
library(rnaturalearth)


# Base US Map -------------------------------------------------------------

us_states<- us_states() %>% filter(!stusps=="HI", !stusps=="AK", !stusps=="PR") #%>% st_transform(2163)
us_states_map = tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
us_states_map

# GET DATA ----------------------------------------------------------------

#rivers
hucs_sf <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")
#plot(hucs_sf$geometry, col="blue2")
st_crs(hucs_sf)
us_states_map + tm_shape(hucs_sf) + tm_lines(col="blue")

# import fish sampling points
locations <- read.csv("https://raw.githubusercontent.com/johnsolk/RNAseq_15killifish/master/map/FishSamplingLOCATIONS.csv",stringsAsFactors = FALSE) %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326, remove=F) # make spatial as SF

st_crs(locations)


# Make a Map --------------------------------------------------------------

#map a color to locations:
locations$NP_col <- ifelse(locations$Native.Physiology=="FW", "black", "#E69F00")

ggplot() + 
  geom_sf(data= us_states, fill="gray50", alpha=0.5) + 
  geom_point(data=locations, aes(x=Long, y=Lat, 
                                 fill=Native.Physiology), pch=21, size=3) + 
  theme_bw(base_size = 8) +
  annotation_scale(location = "bl",style = "bar", pad_y=unit(0.2, "cm")) +
  coord_sf() + 
  theme(plot.background = element_blank(),
        legend.position = c(0.1, 0.2),
        legend.key = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0 ,0), "mm"))+
  ggthemes::scale_fill_colorblind("Native Physiology") +
  ggrepel::geom_label_repel(data=locations, 
                            aes(x = Long, y=Lat, label=Species), 
                            segment.colour = "gray30",
                            #segment.colour=quote(ifelse(locations$Native.Physiology=="FW", "black", "#E69F00")),
                            cex=3, 
                            segment.alpha = 0.7, force=3,  
                            box.padding=1, nudge_y = 0.3)

ggsave(filename = "figs/map_example_killifish.png", dpi=300, width = 8, height = 6, units="in")


# Make a tmap Map ---------------------------------------------------------

tmap_mode("plot")
colors = c("orange","forestgreen")
shapes = c(19,17)
tm_shape(us_states, projection = 2163) +
  tm_polygons() +
  tm_shape(locations) +
  tm_symbols(col="Native.Physiology", palette=colors, size=2, title.size=4) +
  tm_layout(frame = FALSE) +
  tm_text("Species", clustering = F, remove.overlap = FALSE, xmod=-.2,ymod=1.2, auto.placement=TRUE) +
  tm_scale_bar(position = c(0.01,0.01), text.size = 1)


# Nice Composite Map ------------------------------------------------------

library(tidyverse)
library(sf)
library(albersusa) # devtools::install_github('hrbrmstr/albersusa')
library(ggrepel)

usa_sf <-
  st_as_sf(usa_composite("laea")) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

usa_sf$nudge_x <- 0
usa_sf$nudge_y <- 0

x_range <- abs(Reduce("-", range(usa_sf$COORDS_X)))
y_range <- abs(Reduce("-", range(usa_sf$COORDS_Y)))

ix <- usa_sf$name %in% c("New Hampshire", "Vermont", "Massachusetts")
usa_sf$nudge_x[ix] <- -1 * 0.15 * x_range
usa_sf$nudge_y[ix] <- 1 * 0.15 * y_range

ix <- usa_sf$name %in% c(
  "Massachusetts",
  "Rhode Island", "Connecticut", "New Jersey", "West Virginia",
  "Maryland", "Delaware", "District of Columbia"
)
usa_sf$nudge_x[ix] <- 1 * 0.2 * x_range
usa_sf$nudge_y[ix] <- -1 * 0.15 * y_range

ggplot(data = usa_sf) +
  geom_sf() +
  geom_text_repel(
    mapping = aes(
      x = COORDS_X,
      y = COORDS_Y,
      label = name
    ),
    nudge_x = usa_sf$nudge_x,
    nudge_y = usa_sf$nudge_y,
    size = 3,
    min.segment.length = 0,
    point.padding = NA,
    segment.color = "grey50"
  ) +
  coord_sf(crs = st_crs(usa_sf), datum = NA) +
  theme_void() +
  xlim(min(usa_sf$COORDS_X) * 1.1, max(usa_sf$COORDS_X) * 1.15)
