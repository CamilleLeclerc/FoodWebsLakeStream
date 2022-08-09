rm(list=ls())

##----------------------------------------
##PACKAGES, FUNCTIONS, & WORKING DIRECTORY
##----------------------------------------
library(ggplot2)
library(igraph)
library(kableExtra)
library(sjstats)
library(tidyverse)
mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
source(mypath("R", "splitviolin.R"))
source(mypath("R", "theme_niwot_ggplot2.R"))


##----
##DATA
##----
myload(ind_size_lake_stream,
       piscivory_lake_stream,
       dataset_lake_stream,
       dir = mypath("data"))

ind_size_lake_stream_uniquevalue <- ind_size_lake_stream %>%
                                      select(type, opcod, length) %>%
                                      group_by(type, opcod) %>%
                                      summarise_at(vars(length), list('mean_fishlength' = ~ mean(.),
                                                                      'median_fishlength' = ~ median(.),
                                                                      'min_fishlength' = ~ min(.),
                                                                      'max_fishlength' = ~ max(.))) %>%
                                      ungroup(.) %>% as.data.frame(.)


dataset_lake_stream <- left_join(dataset_lake_stream, piscivory_lake_stream %>% dplyr::select(type, opcod, nb_pisc_nodes, prop_pisc_nodes, nb_pisc_ind, prop_pisc_ind), by = c("type", "opcod"))
dataset_lake_stream <- left_join(dataset_lake_stream, ind_size_lake_stream_uniquevalue, by = c("type", "opcod"))


##----
##PLOT
##----
##Mean Fish length (mm) vs. Prop of piscivores individuals
ggplot(dataset_lake_stream, aes(x = mean_fishlength, y = prop_pisc_ind, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Proportion of piscivores individuals", x = "Mean fish length (mm)")


##Median Fish length (mm) vs. Prop of piscivores individuals
ggplot(dataset_lake_stream, aes(x = median_fishlength, y = prop_pisc_ind, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Proportion of piscivores individuals", x = "Median fish length (mm)")


##Mean Fish length (mm) vs. Prop of piscivores nodes
ggplot(dataset_lake_stream, aes(x = mean_fishlength, y = prop_pisc_nodes, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Proportion of piscivores nodes", x = "Mean fish length (mm)")


##Median Fish length (mm) vs. Prop of piscivores nodes
ggplot(dataset_lake_stream, aes(x = median_fishlength, y = prop_pisc_nodes, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Proportion of piscivores nodes", x = "Median fish length (mm)")


##Max trophic level vs. Prop of piscivores individuals
ggplot(dataset_lake_stream, aes(x = max_troph_lvl, y = prop_pisc_ind, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Proportion of piscivores individuals", x = "Max trophic level")


##Max trophic level vs. Prop of piscivores nodes
ggplot(dataset_lake_stream, aes(x = max_troph_lvl, y = prop_pisc_nodes, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Proportion of piscivores nodes", x = "Max trophic level")


##Max trophic level vs. Mean Fish length (mm)
ggplot(dataset_lake_stream, aes(x = max_troph_lvl, y = mean_fishlength, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Mean fish length (mm)", x = "Max trophic level")


##Max trophic level vs. Median Fish length (mm)
ggplot(dataset_lake_stream, aes(x = max_troph_lvl, y = median_fishlength, fill = type, color = type)) +
  geom_point(shape = 21, colour = "#000000", size = 2, alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  scale_colour_manual(values = c("#482878FF", "#1F9E89FF"), name = " ") +
  facet_wrap( ~ year, scales = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16, colour = "#000000"), 
        axis.line.x = element_line(color = "#000000"), 
        axis.line.y = element_line(color = "#000000"),
        strip.text.x = element_text(size = 20, face = "bold"),
        strip.text.y = element_text(angle = 180, size = 20, face = "bold")) +
  labs(y = "Median fish length (mm)", x = "Max trophic level")


