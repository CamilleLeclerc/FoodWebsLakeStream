rm(list=ls())

## R sert-up
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggspatial)
library(kableExtra)
library(knitr)
library(magrittr)
library(PerformanceAnalytics)
library(rgdal)
library(rnaturalearth)
library(rstatix)
library(sf)
library(tidyverse)


mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

## Time period
  #2005 - 2017


## Dataset
  # Lake data
  myload(yearly_avg_dbo5_lake, yearly_avg_temp_lake, dir = mypath("outputs"))    
  myload(network_lake_metrics, dir = mypath("data"))
  plando_spatial_coordinates <- read_csv("data-raw/plando_spatial_coordinates.csv")
  colnames(plando_spatial_coordinates)[1] <- "code_lac"
  lake_analysis <- read.csv("C:/Users/Camille/Desktop/FoodWebsRiverLake/outputs/lake_list.txt", sep = "")
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
  

  plando_spatial_coordinates <- unique(data_lake %>% dplyr::select(lat, long, type))


  rm(dbo_lake, yearly_avg_dbo5_lake, temp_lake, yearly_avg_temp_lake)
  
  
  # Stream data
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
  
  
  stream_spatial_coordinates <- unique(data_stream %>% dplyr::select(lat, long, type))
  
  
  data_final <- rbind(data_lake, data_stream)
  
  dataset_lake_stream <- data_final
  mysave(dataset_lake_stream,
         dir = mypath("data"), overwrite = TRUE)
  
  rm(dataset_lake_stream, dbo_stream, yearly_avg_water_chemical_stream, temp_stream, yearly_avg_water_temperature_stream, data_lake, data_stream, env_lake, env_stream, metric_lake, metric_stream)
  
  
  
## Spatial distribution
  coordinates <- rbind(plando_spatial_coordinates, stream_spatial_coordinates)

  worldmap <- ne_countries(continent = 'europe', scale = 'large', type = 'countries', returnclass = 'sf')
  fr <- data.frame(Country = "France", Focus = "YES") 
  world_joined <- left_join(worldmap, fr, by = c("name" = "Country"))
  francemap <- ne_countries(country = 'france', scale = 'large', type = 'countries', returnclass = 'sf')
  lakes <- ne_download(scale = 10, type = 'lakes', category = 'physical')
  rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')
  sf::sf_use_s2(FALSE)
  francelakes <- st_intersection(st_as_sf(lakes), st_as_sf(francemap))
  francerivers <- st_intersection(st_as_sf(rivers), st_as_sf(francemap))

  ggplot() +
    geom_sf(data = world_joined, fill = "white", color = "black", size = 0.05) +
    geom_sf(data = francemap, fill = gray(0.9), color = "black", size = 0.25) +
    geom_sf(data = francerivers, col = '#6baed6', size = 0.25) +  
    geom_sf(data = francelakes, col = '#6baed6', fill = '#6baed6', size = 0.05) +
    geom_point(data = coordinates,
             aes(x = long, y = lat, fill = type), shape = 21, colour = "#000000", size = 2) +
    scale_fill_manual(values=c("#482878FF", "#1F9E89FF"), name = "Communities samples") +
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
    theme(legend.position = c(0.16, 0.08), legend.background = element_blank(), legend.key = element_blank())

  
    nrow(unique(coordinates %>% dplyr::filter(type == "stream")))  
    nrow(unique(coordinates %>% dplyr::filter(type == "lake"))) 
    
    rm(plando_spatial_coordinates, stream_spatial_coordinates, coordinates, worldmap, fr, world_joined, francemap, lakes, rivers, francelakes, francerivers)

    
## Environmental variables
tbl.env <- data_final %>%
            dplyr::select(type, station, opcod, year, dbo, temp) %>%
            gather(key = 'env_variable', value = 'value', dbo:temp) %>%
            mutate(env_variable = replace(env_variable, env_variable == "temp", "temperature (°C)")) %>%
            mutate(env_variable = replace(env_variable, env_variable == "dbo", "dbo (mg/L)")) %>%
            select(type, value, env_variable) %>%
            filter_all(all_vars(!is.na(.))) %>%
            group_by(env_variable, type) %>%
            summarise(mean = sprintf("%.1f", mean(value, na.rm = TRUE)),
                      sd = sprintf("%.1f", sd(value, na.rm = TRUE)),
                      median = sprintf("%.1f", median(value, na.rm = TRUE)),
                      min = sprintf("%.1f", min(value, na.rm = TRUE)),
                      max = sprintf("%.1f", max(value, na.rm = TRUE))) %>%
            gather(key, value, mean:max) %>%
            unite(Group, type, key) %>%  
            mutate(Group = fct_relevel(Group, "lake_mean", "lake_sd", "lake_median", "lake_min", "lake_max",
                                       "stream_mean", "stream_sd", "stream_median", "stream_min", "stream_max")) %>%
            spread(Group, value)

tbl.env <- setNames(tbl.env, nm = sub(".+_", "", names(tbl.env)))

kable(tbl.env, align = c('l', rep('c', 10))) %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "Lake" = 5, "Stream" = 5))     

  #correlation environmental variables - stream
chart.Correlation(data_final %>%
                    dplyr::filter(type == "stream") %>%
                    dplyr::select(dbo, temp),
                  histogram = TRUE, method = "pearson")    

  #correlation environmental variables - lake
chart.Correlation(data_final %>%
                    dplyr::filter(type == "lake") %>%
                    dplyr::select(dbo, temp),
                  histogram = TRUE, method = "pearson") 

#correlation environmental variables - stream & lake
chart.Correlation(data_final %>%
                    dplyr::select(dbo, temp),
                  histogram = TRUE, method = "pearson") 


## Food web metrics
tbl.fw <- data_final %>%
            dplyr::select(type, station, opcod, year, connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl) %>%
            gather(key = 'fw_metric', value = 'value', connectance:max_troph_lvl) %>%
            mutate(fw_metric = replace(fw_metric, fw_metric == "nbnode", "number of nodes")) %>%
            mutate(fw_metric = replace(fw_metric, fw_metric == "w_trph_lvl_avg", "mean trophic level")) %>%
            mutate(fw_metric = replace(fw_metric, fw_metric == "max_troph_lvl", "max trophic level")) %>%
            select(type, value, fw_metric) %>%
            filter_all(all_vars(!is.na(.))) %>%
            group_by(fw_metric, type) %>%
            summarise(mean = sprintf("%.1f", mean(value, na.rm = TRUE)),
                      sd = sprintf("%.1f", sd(value, na.rm = TRUE)),
                      median = sprintf("%.1f", median(value, na.rm = TRUE)),
                      min = sprintf("%.1f", min(value, na.rm = TRUE)),
                      max = sprintf("%.1f", max(value, na.rm = TRUE))) %>%
            gather(key, value, mean:max) %>%
            unite(Group, type, key) %>%  
            mutate(Group = fct_relevel(Group, "lake_mean", "lake_sd", "lake_median", "lake_min", "lake_max",
                                        "stream_mean", "stream_sd", "stream_median", "stream_min", "stream_max")) %>%
            spread(Group, value)

tbl.fw <- setNames(tbl.fw, nm = sub(".+_", "", names(tbl.fw)))

kable(tbl.fw, align = c('l', rep('c', 10))) %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "Lake" = 5, "Stream" = 5)) 

#correlation food web metrics - stream
chart.Correlation(data_final %>%
                    dplyr::filter(type == "stream") %>%
                    dplyr::select(connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl),
                  histogram = TRUE, method = "pearson")    

#correlation food web metrics - lake
chart.Correlation(data_final %>%
                    dplyr::filter(type == "lake") %>%
                    dplyr::select(connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl),
                  histogram = TRUE, method = "pearson") 

#correlation food web metrics - lake
chart.Correlation(data_final %>%
                    dplyr::select(connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl),
                  histogram = TRUE, method = "pearson") 
    
## Food web metrics vs. Environmental variables
data <- data_final %>%
          dplyr::select(type, station, opcod, year, dbo, temp, connectance, richness, nbnode, w_trph_lvl_avg, max_troph_lvl) %>%
          gather(key = 'env_variable', value = 'env_value', dbo:temp) %>%
          mutate(env_variable = replace(env_variable, env_variable == "temp", "temperature (°C)")) %>%
          mutate(env_variable = replace(env_variable, env_variable == "dbo", "dbo (mg/L)")) %>%
          gather(key = 'fw_metric', value = 'metric_value', connectance:max_troph_lvl) %>%
          mutate(fw_metric = replace(fw_metric, fw_metric == "nbnode", "number of nodes")) %>%
          mutate(fw_metric = replace(fw_metric, fw_metric == "w_trph_lvl_avg", "mean trophic level")) %>%
          mutate(fw_metric = replace(fw_metric, fw_metric == "max_troph_lvl", "max trophic level"))

    
ggplot(data , aes(x = env_value, y = metric_value, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_grid(fw_metric ~ env_variable, scales = "free", switch = "y") +   # Put the y facet strips on the left
  scale_y_continuous(position = "right") +   # Put the y-axis labels on the right
theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = NULL, x = NULL)

  