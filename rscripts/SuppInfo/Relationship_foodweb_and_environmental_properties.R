rm(list=ls())

## R set-up
library(dplyr)
library(GGally)


mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))


##---------
## DATASETS
##---------
myload(dataset_lake_stream, dir = mypath("data")) 


##-------------------
## SCATTERPLOT MATRIX
##-------------------
ggpairs(dataset_lake_stream, columns = c(8, 9, 7 , 11, 6, 5), ggplot2::aes(colour = type),
        lower = list(continuous = wrap("smooth", alpha = 0.2)),
        columnLabels = c("Rich.", "Nodes", "Connect.", "Max. troph. level", "Temp. (°C)", "DBO (mg/L)")) +
  scale_colour_manual(values = c("#8da0cb", "#66c2a5")) +
  scale_fill_manual(values = c("#8da0cb", "#66c2a5")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 10, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"))
