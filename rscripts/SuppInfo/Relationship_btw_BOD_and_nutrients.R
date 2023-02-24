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
ggpairs(dataset_lake_stream, columns = c(5, 20:21), ggplot2::aes(colour = type),
        lower = list(continuous = wrap("smooth", alpha = 0.2)),
        columnLabels = c("BOD (mg/L)", "TN (mg/L)", "TP (mg/L)")) +
  scale_colour_manual(values = c("#8da0cb", "#66c2a5")) +
  scale_fill_manual(values = c("#8da0cb", "#66c2a5")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 14, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 16, face = "bold"),
        strip.text.y = element_text(size = 16, face = "bold"))
#500 x 500
#https://r-graph-gallery.com/199-correlation-matrix-with-ggally.html#:~:text=The%20ggpairs()%20function%20of,is%20available%20on%20the%20diagonal.