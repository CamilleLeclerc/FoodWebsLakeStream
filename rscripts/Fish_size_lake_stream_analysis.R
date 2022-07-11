rm(list=ls())

##----------------------------------------
##PACKAGES, FUNCTIONS, & WORKING DIRECTORY
##----------------------------------------
library(tidyverse)
library(magrittr)
library(kableExtra)
library(igraph)
#install.packages("~/Documents/post-these/packages/sizeTrophicInteractions_0.0.0.9000.tar.gz")
#library(sizeTrophicInteractions)

mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source_dir(mypath("R"))


##----
##DATA
##----
myload(metaweb_lake, ind_size, metaweb_stream, length_analysis, dataset_lake_stream, dir = mypath("data"))
code_species_river_lake <- read.delim(paste0(mypath("outputs"), "/code_species_river_lake.txt"))

myload( dir = mypath("data"))


##------------
##LAKE DATASET
##------------
code_species_lake <- code_species_river_lake %>% select(sp_code, sp_lake) %>% drop_na(.)  
code_species_lake$sp_lake <- gsub(" ", "_", code_species_lake$sp_lake)
colnames(code_species_lake)[2] <- "species"
ind_size <- left_join(ind_size, code_species_lake, by = "species")
colnames(ind_size)

sp_lake_metaweb <- as.data.frame(metaweb_lake$species)
rownames(sp_lake_metaweb) <- NULL ; colnames(sp_lake_metaweb) <- "sp_code"

ind_size_lake_metaweb <- ind_size %>% dplyr::filter(sp_code %in% sp_lake_metaweb$sp_code)

dataset_lake <- dataset_lake_stream %>% dplyr::filter(type == "lake")
length(unique(dataset_lake$opcod))
length(unique(ind_size_lake_metaweb$id_campagne))
ind_size_lake_metaweb <- ind_size_lake_metaweb %>% dplyr::filter(id_campagne %in% dataset_lake$opcod)
length(unique(dataset_lake$opcod))
length(unique(ind_size_lake_metaweb$id_campagne))

rm(code_species_lake, code_species_river_lake, ind_size, sp_lake_metaweb, metaweb_lake, dataset_lake)


##--------------
##STREAM DATASET
##--------------
sp_stream_metaweb <- as.data.frame(metaweb_stream$species)
rownames(sp_stream_metaweb) <- NULL ; colnames(sp_stream_metaweb) <- "sp_code"
colnames(length_analysis)[2] <- "sp_code"

ind_size_stream_metaweb <- length_analysis %>% dplyr::filter(sp_code %in% sp_stream_metaweb$sp_code)

dataset_stream <- dataset_lake_stream %>% dplyr::filter(type == "stream")
length(unique(dataset_stream$opcod))
length(unique(ind_size_stream_metaweb$opcod))
ind_size_stream_metaweb <- ind_size_stream_metaweb %>% dplyr::filter(opcod %in% dataset_stream$opcod)
length(unique(dataset_stream$opcod))
length(unique(ind_size_stream_metaweb$opcod))

rm(length_analysis, sp_stream_metaweb, metaweb_stream, dataset_stream)


##-----------------
##SIZE DISTRIBUTION
##-----------------
ind_size_stream_metaweb$type <- "stream"
ind_size_lake_metaweb$type <- "lake"
ind_size <- rbind(ind_size_stream_metaweb, ind_size_lake_metaweb %>% dplyr::select(id_campagne, sp_code, fish, type) %>% setNames(., c("opcod", "sp_code", "length", "type")))
ind_size <- ind_size %>% dplyr::select(type, opcod, sp_code, length)

ind_size <- left_join(ind_size, dataset_lake_stream %>% dplyr::select(opcod, year) %>% unique(.), by = "opcod")


