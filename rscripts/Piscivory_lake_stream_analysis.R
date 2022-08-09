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
myload(metaweb_lake, metaweb_analysis,
       network_lake, network_stream,
       dataset_lake_stream,
       dir = mypath("data"))
metaweb_stream <- metaweb_analysis
rm(metaweb_analysis)


##------------
##LAKE DATASET
##------------
piscivory_lake <- data.frame(matrix(nrow = nrow(network_lake), ncol = 6,
                                    dimnames = list(c(), c("type", "opcod",
                                                           "nb_pisc_nodes", "prop_pisc_nodes",
                                                           "nb_pisc_ind", "prop_pisc_ind"))))
piscivory_lake$type <- "lake"
piscivory_lake$opcod <- network_lake$id_campagne


for (i in 1:nrow(network_lake)) {
  ind <- network_lake$data[[i]] 
  ind$sp_class <- paste(ind$species, ind$class_id, sep= "_")
  
  network <- network_lake$network[[i]] %>% 
            graph_from_data_frame() %>%
            as_adjacency_matrix(., sparse = FALSE)
  #network
  #str(network)
  
  mask_fish_network <- str_remove(colnames(network), "_\\d+") %in% metaweb_lake$species
  #mask_fish_network
  fish_fish_network <- network[mask_fish_network, mask_fish_network]
  #fish_fish_network

  #Piscivorous nodes in the network:
  piscivory_lake$nb_pisc_nodes[[i]] <- length(which(colSums(fish_fish_network) > 0))
  piscivory_lake$prop_pisc_nodes[[i]] <- length(which(colSums(fish_fish_network) > 0)) / length(colSums(fish_fish_network))

  #Piscivorous individuals in the network:
  pisc_class <- as.data.frame(which(colSums(fish_fish_network) > 0)) %>% rownames(.)
  ind_pisc <- ind %>% dplyr::filter(sp_class %in% pisc_class)
    #check
    #length(pisc_class) == length(unique(ind_pisc$sp_class))
  piscivory_lake$nb_pisc_ind[[i]] <- nrow(ind_pisc)
  piscivory_lake$prop_pisc_ind[[i]] <- nrow(ind_pisc) / nrow(ind)
 
  rm(ind, network, mask_fish_network, fish_fish_network, pisc_class, ind_pisc)
  print(i)
}
rm(i)

piscivory_lake <- left_join(piscivory_lake, dataset_lake_stream %>% dplyr::filter(type == "lake") %>% dplyr::select(opcod, year) %>% unique(.), by = "opcod")
summary(piscivory_lake)
piscivory_lake <- piscivory_lake %>% drop_na()
summary(piscivory_lake)
class(piscivory_lake$type) ; piscivory_lake$type <- as.factor(piscivory_lake$type) ; class(piscivory_lake$type)
class(piscivory_lake$opcod)
class(piscivory_lake$nb_pisc_nodes)
class(piscivory_lake$prop_pisc_nodes)
class(piscivory_lake$nb_pisc_ind)
class(piscivory_lake$prop_pisc_ind)
class(piscivory_lake$year) ; piscivory_lake$year <- as.factor(piscivory_lake$year) ; class(piscivory_lake$year)
piscivory_lake$period <-"2005-2017"




##--------------
##STREAM DATASET
##--------------
piscivory_stream <- data.frame(matrix(nrow = nrow(network_stream), ncol = 6,
                                      dimnames = list(c(), c("type", "opcod",
                                                             "nb_pisc_nodes", "prop_pisc_nodes",
                                                             "nb_pisc_ind", "prop_pisc_ind"))))
piscivory_stream$type <- "stream"
piscivory_stream$opcod <- network_stream$opcod


for (i in 1:nrow(network_stream)) {
  ind <- network_stream$data[[i]] 
  ind$sp_class <- paste(ind$species, ind$class_id, sep= "_")
  
  network <- network_stream$network[[i]] %>% 
    graph_from_data_frame() %>%
    as_adjacency_matrix(., sparse = FALSE)
  #network
  #str(network)
  
  mask_fish_network <- str_remove(colnames(network), "_\\d+") %in% metaweb_stream$species
  #mask_fish_network
  fish_fish_network <- network[mask_fish_network, mask_fish_network]
  #fish_fish_network
  
  if(class(fish_fish_network) == "numeric") {
    #Piscivorous nodes in the network:
    piscivory_stream$nb_pisc_nodes[[i]] <- 0
    piscivory_stream$prop_pisc_nodes[[i]] <- 0
    
    #Piscivorous individuals in the network:
    piscivory_stream$nb_pisc_ind[[i]] <- 0
    piscivory_stream$prop_pisc_ind[[i]] <- 0
    
    rm(ind, network, mask_fish_network, fish_fish_network)
  } else{#Piscivorous nodes in the network:
    piscivory_stream$nb_pisc_nodes[[i]] <- length(which(colSums(fish_fish_network) > 0))
    piscivory_stream$prop_pisc_nodes[[i]] <- length(which(colSums(fish_fish_network) > 0)) / length(colSums(fish_fish_network))
    
    #Piscivorous individuals in the network:
    pisc_class <- as.data.frame(which(colSums(fish_fish_network) > 0)) %>% rownames(.)
    ind_pisc <- ind %>% dplyr::filter(sp_class %in% pisc_class)
    #check
    #length(pisc_class) == length(unique(ind_pisc$sp_class))
    piscivory_stream$nb_pisc_ind[[i]] <- nrow(ind_pisc)
    piscivory_stream$prop_pisc_ind[[i]] <- nrow(ind_pisc) / nrow(ind)
    
    rm(ind, network, mask_fish_network, fish_fish_network, pisc_class, ind_pisc)}
  
  print(i)
}
rm(i)

piscivory_stream <- left_join(piscivory_stream, dataset_lake_stream %>% dplyr::filter(type == "stream") %>% dplyr::select(opcod, year) %>% unique(.), by = "opcod")
summary(piscivory_stream)
piscivory_stream <- piscivory_stream %>% drop_na()
summary(piscivory_stream)
class(piscivory_stream$type) ; piscivory_stream$type <- as.factor(piscivory_stream$type) ; class(piscivory_stream$type)
class(piscivory_stream$opcod)
class(piscivory_stream$nb_pisc_nodes)
class(piscivory_stream$prop_pisc_nodes)
class(piscivory_stream$nb_pisc_ind)
class(piscivory_stream$prop_pisc_ind)
class(piscivory_stream$year) ; piscivory_stream$year <- as.factor(piscivory_stream$year) ; class(piscivory_stream$year)
piscivory_stream$period <-"2005-2017"


piscivory_lake_stream <- rbind(piscivory_lake, piscivory_stream)
mysave(piscivory_lake_stream,
       dir = mypath("data"), overwrite = TRUE)


##PISCIVORY BY TYPE
ggplot(data = piscivory_lake_stream, aes(x = period, y = prop_pisc_ind, fill = type)) +
  geom_split_violin(alpha = .4, trim = FALSE) +
  geom_boxplot(width = .2, alpha = .6, show.legend = FALSE) +
  stat_summary(fun = "mean", mapping = aes(color = type), geom = "point", show.legend = F, position = position_dodge(.2)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Proportion of piscivores individuals in networks") +
  scale_fill_manual(values = c("#d9d9d9", "#252525"), breaks = c('stream', 'lake')) +
  scale_colour_manual(values = c("#252525", "#969696")) +
  coord_flip() +
  #guides(fill = FALSE, color = FALSE) +
  theme_niwot() +
  theme(axis.title.y = element_text(face = "bold", vjust = 3),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(face = "bold", vjust = -1))

piscivory_lake_stream %>%
  select(type, prop_pisc_ind) %>%
  group_by(type) %>%
  summarise_at(vars(prop_pisc_ind), list('N' = ~length(.),
                                  'Mean' = ~ mean(.) %>% round(., digits = 3),
                                  'Std. Dev.' = ~ se(.) %>% round(., digits = 3),
                                  'Median' = ~ median(.) %>% round(., digits = 3),
                                  'Min' = ~ min(.),
                                  'Max' = ~ max(.))) %>%
  kable(., "simple")



piscivory_lake_stream %>%
  select(type, prop_pisc_nodes) %>%
  group_by(type) %>%
  summarise_at(vars(prop_pisc_nodes), list('N' = ~length(.),
                                         'Mean' = ~ mean(.) %>% round(., digits = 3),
                                         'Std. Dev.' = ~ se(.) %>% round(., digits = 3),
                                         'Median' = ~ median(.) %>% round(., digits = 3),
                                         'Min' = ~ min(.),
                                         'Max' = ~ max(.))) %>%
  kable(., "simple")




##PISCIVORY BY TYPE & YEAR
ggplot(data = piscivory_lake_stream, aes(x = year, y = prop_pisc_ind, fill = type)) +
  geom_split_violin(alpha = .4, trim = FALSE) +
  geom_boxplot(width = .3, alpha = .6, show.legend = FALSE) +
  stat_summary(fun = "mean", mapping = aes(color = type), geom = "point", show.legend = F, position = position_dodge(.3)) +
  scale_x_discrete(name = "Sampling year") +
  scale_y_continuous(name = "Proportion of piscivores individuals in networks") +
  scale_fill_manual(values = c("#d9d9d9", "#252525"), breaks = c('stream', 'lake')) +
  scale_colour_manual(values = c("#252525", "#969696")) +
  coord_flip() +
  #guides(fill = FALSE, color = FALSE) +
  theme_niwot() +
  theme(axis.title.y = element_text(face = "bold", vjust = 3),
        axis.title.x = element_text(face = "bold", vjust = -1))


piscivory_lake_stream %>%
  select(type, year, prop_pisc_ind) %>%
  group_by(type, year) %>%
  summarise_at(vars(prop_pisc_ind), list('N' = ~length(.),
                                  'Mean' = ~ mean(.) %>% round(., digits = 3),
                                  'Std. Dev.' = ~ sd(.) %>% round(., digits = 3),
                                  'Median' = ~ median(.) %>% round(., digits = 3),
                                  'Min' = ~ min(.) %>% round(., digits = 3),
                                  'Max' = ~ max(.) %>% round(., digits = 3))) %>%
  kable(., "simple")
