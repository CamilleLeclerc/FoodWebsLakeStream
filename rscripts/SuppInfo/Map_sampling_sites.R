rm(list=ls())

## R set-up
library(dplyr)
library(ggplot2)
library(ggspatial)
library(rgdal)
library(rnaturalearth)
library(sf)


mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))


##---------
## DATASETS
##---------
myload(dataset_lake_stream, dir = mypath("data")) 

##-----------------
## SPATIAL COVERAGE
##-----------------

worldmap <- ne_countries(continent = 'europe', scale = 'large', type = 'countries', returnclass = 'sf')
fr <- data.frame(Country = "France", Focus = "YES") 
world_joined <- left_join(worldmap, fr, by = c("name" = "Country"))
francemap <- ne_countries(country = 'france', scale = 'large', type = 'countries', returnclass = 'sf')
ogrListLayers("data-raw/ne_10m_lakes/ne_10m_lakes.shp")
lakes <- readOGR("data-raw/ne_10m_lakes/ne_10m_lakes.shp", layer="ne_10m_lakes") 
lakes <- st_as_sf(lakes)
ogrListLayers("data-raw/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
rivers <- readOGR("data-raw/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp", layer="ne_10m_rivers_lake_centerlines") 
rivers <- st_as_sf(rivers)
sf::sf_use_s2(FALSE)
francelakes <- st_intersection(st_as_sf(lakes), st_as_sf(francemap))
francerivers <- st_intersection(st_as_sf(rivers), st_as_sf(francemap))
ogrListLayers("data-raw/BassinsHydrographiques_Métropole2019_BDTopage/BassinHydrographique.shp")
BassinHydrographique <- readOGR("data-raw/BassinsHydrographiques_Métropole2019_BDTopage/BassinHydrographique.shp", layer="BassinHydrographique") 
BassinHydrographique <- st_as_sf(BassinHydrographique)
BassinHydrographique <- st_transform(BassinHydrographique, crs = st_crs(francemap))
BassinHydrographique <- st_intersection(st_as_sf(BassinHydrographique), st_as_sf(francemap))

ggplot() +
  geom_sf(data = world_joined, fill = "white", color = "black", size = 0.05) +
  geom_sf(data = BassinHydrographique, fill = "#f0f0f0", color = "#bdbdbd", size = 0.25) +
  #geom_sf(data = francerivers, col = '#6baed6', size = 0.25) +  
  geom_sf(data = francelakes, col = '#6baed6', fill = '#6baed6', size = 0.05) +
  geom_sf(data = francemap, fill = alpha("white", 0), color = "black", size = 0.6) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(which_north = "true", location = "tr", height = unit(0.5, "cm"), width = unit(0.5, "cm"), style = north_arrow_orienteering(fill = c("black", "black"), text_size = 6)) +
  geom_point(data = dataset_lake_stream,
             aes(x = long, y = lat, fill = type), shape = 21, colour = "#000000", size = 2) +
  scale_fill_manual(values = c("#8da0cb", "#66c2a5"), name = "Communities samples") +
  coord_sf(xlim = c(-5, 9.75), ylim = c(41.3, 51.5), expand = FALSE) +
  theme(title = element_text(),
        plot.title = element_text(margin = margin(20,20,20,20), size = 18, hjust = 0.5),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.background = element_blank(),
        strip.background = element_rect(fill = "#000000", color = "#000000", size = 1, linetype = "solid"),
        strip.text.x = element_text(size = 12, color = "#ffffff", face = "bold"),
        panel.border = element_rect(colour = "#000000", fill = NA, size = 1)) +
  theme(legend.position = c(0.15, 0.10), legend.background = element_blank(), legend.key = element_blank())
