library(tidyverse)
library(magrittr)
library(kableExtra)
library(igraph)
#install.packages("~/Documents/post-these/packages/sizeTrophicInteractions_0.0.0.9000.tar.gz")
#library(sizeTrophicInteractions)

mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source_dir(mypath("R"))


##FISH DIET SHIFT
myload(fish_diet_shift, dir = mypath("data"))
fish_diet_shift_missing_lake_species <- read.delim("C:/Users/camille.leclerc/Desktop/FoodWebsRiverLake/data-raw/fish_diet_shift_missing_lake_species.txt")

fish_diet_shift <- rbind(as.data.frame(fish_diet_shift), fish_diet_shift_missing_lake_species)

mysave(fish_diet_shift, dir = mypath("data"), overwrite = TRUE)


##PREDATION WINDOW
myload(pred_win, dir = mypath("data"))
pred_win_missing_lake_species <- read.delim("C:/Users/camille.leclerc/Desktop/FoodWebsRiverLake/data-raw/pred_win_missing_lake_species.txt")

pred_win <- rbind(as.data.frame(pred_win), pred_win_missing_lake_species)

mysave(pred_win, dir = mypath("data"), overwrite = TRUE)

