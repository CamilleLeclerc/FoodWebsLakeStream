rm(list=ls())

## R set-up
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggspatial)
library(kableExtra)
library(knitr)
library(magrittr)
library(maptools)
library(PerformanceAnalytics)
library(raster)
library(rgdal)
library(rgeos)
library(rnaturalearth)
library(rstatix)
library(sf)
library(tidyverse)


mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

##--------
## DATASET
##--------
# Lake data
#----------
myload(yearly_avg_dbo5_lake, yearly_avg_temp_lake, dir = mypath("outputs"))    
myload(network_lake_metrics, dir = mypath("data"))
plando_spatial_coordinates <- read_csv("data-raw/plando_spatial_coordinates.csv")
colnames(plando_spatial_coordinates)[1] <- "code_lac"
lake_analysis <- read.csv("outputs/lake_list.txt", sep = "")
lake_analysis$station_date <- paste(lake_analysis$code_lac, lake_analysis$camp_annee, sep = "_")


dbo_lake <- yearly_avg_dbo5_lake
dbo_lake$station_date <- paste(dbo_lake$code_lac, dbo_lake$year, sep = "_")
yearly_avg_temp_lake$Lake_ID[yearly_avg_temp_lake$Lake_ID == "LER27"] <- "LER27a"
temp_lake <- yearly_avg_temp_lake
temp_lake$station_date <- paste(temp_lake$Lake_ID, temp_lake$year, sep = "_")

env_lake <- left_join(lake_analysis, dbo_lake %>% dplyr::select(raw_value, station_date), by = "station_date")
colnames(env_lake)[5] <- "dbo" 
env_lake <- left_join(env_lake, temp_lake %>% dplyr::select(raw_value, station_date), by = "station_date")
colnames(env_lake)[6] <- "temp"
env_lake <- env_lake %>% drop_na(temp)
sort(unique(env_lake$camp_annee))
env_lake <- env_lake %>% dplyr:: filter(camp_annee < 2018)
length(unique(env_lake$code_lac)) ; length(unique(env_lake$id_campagne))


length(unique(env_lake$id_campagne)) ; length(unique(network_lake_metrics$id_campagne))
network_lake_metrics$richness <- NA
for(i in 1: nrow(network_lake_metrics)){
  data <- network_lake_metrics[[6]][[i]]
  data <- data %>% filter(species_name != "biof" & species_name != "det" & species_name != "macroph" & species_name != "phytob" & species_name != "phytopl" & species_name != "zoob" & species_name != "zoopl")
  data$species_name <- substr(data$species_name, 1, 3)
  network_lake_metrics$richness[i] <- length(unique(data$species_name))
  rm(data)
}
rm(i)
metric_lake <- network_lake_metrics %>% dplyr::select(connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl)


data_lake <- left_join(env_lake, metric_lake, by = "id_campagne")
data_lake <- left_join(data_lake, plando_spatial_coordinates, by = "code_lac")
data_lake$type <- "lake"
data_lake <- data_lake %>% dplyr::select(type, code_lac, id_campagne, camp_annee, dbo, temp, connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl, lat_plando, long_plando)
colnames(data_lake) <- c("type", "station", "opcod", "year", "dbo", "temp", "connectance", "richness", "nbnode", "w_trph_lvl_avg", "max_troph_lvl", "lat", "long")


rm(dbo_lake, yearly_avg_dbo5_lake, temp_lake, yearly_avg_temp_lake)


# Stream data
#------------
myload(op_analysis, station_analysis, dir = mypath("data"))
myload(yearly_avg_water_chemical_stream, yearly_avg_water_temperature_stream, dir = mypath("data/environment"))    
myload(network_stream_metrics, dir = mypath("data"))


op_analysis <- op_analysis[which(op_analysis$date > "2004-12-31"),]
op_analysis <- op_analysis[which(op_analysis$date < "2019-01-01"),]
op_analysis$station_date <- paste(op_analysis$station, op_analysis$year, sep = "_")


dbo_stream <- yearly_avg_water_chemical_stream %>% dplyr::filter(category == "DBO")
dbo_stream$station_date <- paste(dbo_stream$id, dbo_stream$year, sep = "_")
temp_stream <- yearly_avg_water_temperature_stream %>%
  group_by(id, year) %>%
  slice(which.min(value.predSE))
temp_stream$station_date <- paste( temp_stream$id,  temp_stream$year, sep = "_")


env_stream <- left_join(op_analysis %>% as.data.frame(.) %>% dplyr::select(opcod, station, year, station_date), dbo_stream %>% as.data.frame(.) %>% dplyr::select(press, station_date), by = "station_date")
env_stream <- left_join(env_stream, temp_stream %>% as.data.frame(.) %>% select(value_corrected, station_date), by = "station_date")
colnames(env_stream)[5] <- "dbo" ; colnames(env_stream)[6] <- "temp"
env_stream <- env_stream %>% drop_na(dbo) %>% drop_na(temp)
sort(unique(env_stream$year))
length(unique(env_stream$station)) ; length(unique(env_stream$opcod))


length(unique(env_stream$opcod)) ; length(unique(network_stream_metrics$opcod))
network_stream_metrics <- network_stream_metrics %>% dplyr::filter(opcod %in% env_stream$opcod)
length(unique(env_stream$opcod)) ; length(unique(network_stream_metrics$opcod))
env_stream <- env_stream %>% dplyr::filter(opcod %in% network_stream_metrics$opcod)
length(unique(env_stream$opcod)) ; length(unique(network_stream_metrics$opcod))

network_stream_metrics$richness <- NA
for(i in 1: nrow(network_stream_metrics)){
  data <- network_stream_metrics[[2]][[i]]
  network_stream_metrics$richness[i] <- length(unique(data$species))
  rm(data)
}
rm(i)
metric_stream <- network_stream_metrics %>% dplyr::select(connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl)


stream_spatial_coordinates <- station_analysis %>%
  mutate(lat = unlist(map(station_analysis$geometry, 2)),
         long = unlist(map(station_analysis$geometry, 1)))
stream_spatial_coordinates <- as.data.frame(stream_spatial_coordinates)
colnames(stream_spatial_coordinates)[1] <- "station"


data_stream <- left_join(env_stream, metric_stream, by = "opcod")
data_stream <- left_join(data_stream, stream_spatial_coordinates, by = "station")
data_stream <- as.data.frame(data_stream)
data_stream$type <- "stream"
data_stream <- data_stream %>% dplyr::select(type, station, opcod, year, dbo, temp, connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl, lat, long)


data_final <- rbind(data_lake, data_stream)
coordinates_freshwater_ecosystems <- unique(data_final %>% select(type, station, lat, long))
freshwater_ecosystems <- coordinates_freshwater_ecosystems

rm(data_final, dbo_stream, yearly_avg_water_chemical_stream, temp_stream, yearly_avg_water_temperature_stream, data_lake, data_stream, env_lake, env_stream, metric_lake, metric_stream, lake_analysis, network_lake_metrics, network_stream_metrics, op_analysis, station_analysis, plando_spatial_coordinates, stream_spatial_coordinates)


##-------------------------------------------------------------------------------------
## EXTRACT INFORMATION ABOUT 'BASSIN VERSANT TOPOGRAPHIQUE' AND 'BASSIN HYDROGRAPHIQUE'
##-------------------------------------------------------------------------------------
#https://bdtopage.eaufrance.fr/page/donnees
coordinates(coordinates_freshwater_ecosystems) <- ~ long + lat
proj4string(coordinates_freshwater_ecosystems) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# Bassins hydrographiques
#------------------------
ogrListLayers("data-raw/BassinsHydrographiques_Métropole2019_BDTopage/BassinHydrographique.shp")
BassinHydrographique <- readOGR("data-raw/BassinsHydrographiques_Métropole2019_BDTopage/BassinHydrographique.shp", layer="BassinHydrographique") 
plot(BassinHydrographique)
summary(BassinHydrographique)
BassinHydrographique <- spTransform(BassinHydrographique, CRS= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(BassinHydrographique)
points(coordinates_freshwater_ecosystems)

freshwater_ecosystems <- cbind(freshwater_ecosystems, over(coordinates_freshwater_ecosystems, BassinHydrographique))
freshwater_ecosystems <- freshwater_ecosystems %>% select(type, station, lat, long, CdBH, LbBH)
unique(freshwater_ecosystems %>% select(CdBH, LbBH))
freshwater_ecosystems$LbBH[freshwater_ecosystems$LbBH == "RhÃ´ne-MÃ©diterranÃ©e"] <- "Rhone-Mediterranee"

#LEM74 data are missing
gDists <- gDistance(coordinates_freshwater_ecosystems[130,], BassinHydrographique, byid = TRUE)
BassinHydrographique@data$LbBH[which.min(gDists)]
BassinHydrographique@data$CdBH[which.min(gDists)]
freshwater_ecosystems$CdBH[freshwater_ecosystems$station == "LEM74"] <- "06"
freshwater_ecosystems$LbBH[freshwater_ecosystems$station == "LEM74"] <- "Rhone-Mediterranee"
rm(gDists)
#CdBH: Code du bassin hydrographique
#LbBH: Libelle du bassin hydrographique


# Bassin versant topographique
#-----------------------------
ogrListLayers("data-raw/BassinVersantTopographique_Métropole2019_BDTopage/BassinVersantTopographique_FXX.shp")
BassinVersant <- readOGR("data-raw/BassinVersantTopographique_Métropole2019_BDTopage/BassinVersantTopographique_FXX.shp", layer="BassinVersantTopographique_FXX") 
plot(BassinVersant)
summary(BassinVersant)
BassinVersant <- spTransform(BassinVersant, CRS= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(BassinVersant)
points(coordinates_freshwater_ecosystems)

freshwater_ecosystems <- cbind(freshwater_ecosystems, over(coordinates_freshwater_ecosystems, BassinVersant))
colnames(freshwater_ecosystems)[18] <- "CdBH.2"
freshwater_ecosystems <- freshwater_ecosystems %>% select(type, station, lat, long, CdBH, LbBH, CdOH, TopoOH)
nrow(unique(freshwater_ecosystems %>% select(CdOH, TopoOH))) #572 BV sur 629 systèmes d'eau douce (lac + riviere)

#LEM74 data are missing
gDists <- gDistance(coordinates_freshwater_ecosystems[130,], BassinVersant, byid = TRUE)
BassinVersant@data$CdOH[which.min(gDists)]
BassinVersant@data$TopoOH[which.min(gDists)]
#freshwater_ecosystems$CdOH[freshwater_ecosystems$station == "LEM74"] <- "XX"
#freshwater_ecosystems$TopoOH[freshwater_ecosystems$station == "LEM74"] <- "XX"
#CdOH: Code de l'objet hydrographique - bassin versant topographique
#TopoOH: Toponyme de l'objet hydrographique bassin versant topographique


##-------------------------------------------------------------------------------------
## EXTRACT INFORMATION ABOUT 'BASSIN VERSANT TOPOGRAPHIQUE' AND 'BASSIN HYDROGRAPHIQUE'
##-------------------------------------------------------------------------------------

#alt <- getData('alt', country = 'FRA', mask = FALSE) #'alt' stands for altitude (elevation); the data were aggregated from SRTM 90 m resolution data between -60 and 60 latitude.
#freshwater_ecosystems <- cbind(freshwater_ecosystems, as.data.frame(raster::extract(alt, coordinates_freshwater_ecosystems)))
#colnames(freshwater_ecosystems)[9] <- "altitude"

freshwater_ecosystems$altitude <- NA

for (i in 1:nrow(freshwater_ecosystems)) {
  
  srtm <- getData('SRTM', lon = freshwater_ecosystems[i, 4], lat = freshwater_ecosystems[i, 3]) #'SRTM' refers to the hole-filled CGIAR-SRTM (90 m resolution).
  freshwater_ecosystems[i, 9] <- raster::extract(srtm, coordinates_freshwater_ecosystems[i,])
  
}




#mysave(supp_data_lake_stream, dir = mypath("data"), overwrite = TRUE)