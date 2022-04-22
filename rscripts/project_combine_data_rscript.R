rm(list=ls())

## R set-up
library(dplyr)


mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))

##---------
## DATASETS
##---------
myload(supp_data_lake_stream, dataset_lake_stream, dir = mypath("data")) 


# Combine datasets
dataset_lake_stream <- left_join(dataset_lake_stream, supp_data_lake_stream %>% select(lat, long, CdBH, LbBH, CdOH, TopoOH, altitude), by = c("lat" = "lat", "long" = "long"))

#CdBH: Code du bassin hydrographique
#LbBH: Libelle du bassin hydrographique
#CdOH: Code de l'objet hydrographique - bassin versant topographique
#TopoOH: Toponyme de l'objet hydrographique bassin versant topographique

mysave(dataset_lake_stream,
       dir = mypath("data"), overwrite = TRUE)