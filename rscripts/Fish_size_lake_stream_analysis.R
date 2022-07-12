rm(list=ls())

##----------------------------------------
##PACKAGES, FUNCTIONS, & WORKING DIRECTORY
##----------------------------------------
library(ggplot2)
library(kableExtra)
library(tidyverse)
library(introdataviz)

mypath <- rprojroot::find_package_root_file
source(mypath("R", "misc.R"))
#source(mypath("R", "geom_flat_violin.R"))
source(mypath("R", "theme_niwot_ggplot2.R"))


##----
##DATA
##----
myload(metaweb_lake, ind_size, metaweb_stream, length_analysis, dataset_lake_stream, dir = mypath("data"))
code_species_river_lake <- read.delim(paste0(mypath("outputs"), "/code_species_river_lake.txt"))


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
ind_size_lake_metaweb <- ind_size_lake_metaweb %>% drop_na(fish)

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
ind_size_stream_metaweb <- ind_size_stream_metaweb %>% dplyr::filter(length > 0)

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
summary(ind_size)
class(ind_size$type) ; ind_size$type <- as.factor(ind_size$type) ; class(ind_size$type)
class(ind_size$opcod)
class(ind_size$sp_code)
class(ind_size$length)
class(ind_size$year) ; ind_size$year <- as.factor(ind_size$year) ; class(ind_size$year)
ind_size$period <-"2005-2017"


##SIZE DISTRIBUTION BY TYPE
ggplot(data = ind_size, aes(x = period, y = length, fill = type)) +
  introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
  geom_boxplot(width = .2, alpha = .6, show.legend = FALSE) +
  stat_summary(fun = "mean", mapping = aes(color = type), geom = "point", show.legend = F, position = position_dodge(.2)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Fish size (mm)") +
  scale_fill_manual(values = c("#d9d9d9", "#252525"), breaks = c('stream', 'lake')) +
  scale_colour_manual(values = c("#252525", "#969696")) +
  coord_flip() +
  #guides(fill = FALSE, color = FALSE) +
  theme_niwot() +
  theme(axis.title.y = element_text(face = "bold", vjust = 3),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(face = "bold", vjust = -1))


#rain_height <- .1
#ggplot(ind_size, aes(x = "", y = length, fill = type)) +
  # clouds
#  introdataviz::geom_flat_violin(trim = FALSE, alpha = 0.4, position = position_nudge(x = rain_height + .05)) +
  # rain
  #geom_point(aes(colour = type), size = 2, alpha = .5, show.legend = FALSE, position = position_jitter(width = rain_height, height = 0)) +
  # boxplots
#  geom_boxplot(width = rain_height, alpha = 0.5, show.legend = FALSE, outlier.shape = NA, position = position_nudge(x = -rain_height*2)) +
  # mean and SE point in the cloud
#  stat_summary(fun.data = mean_se, mapping = aes(color = type), show.legend = FALSE, position = position_nudge(x = rain_height * 3)) +
  # adjust layout
#  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
#  scale_y_continuous(name = "Fish size (mm)") + #, breaks = seq(200, 800, 100), limits = c(200, 800)) +
#  coord_flip() +
  #facet_wrap(~factor(condition, 
  #                   levels = c("word", "nonword"), 
  #                   labels = c("Words", "Non-Words")), 
  #           nrow = 2) +
  # custom colours and theme
#  scale_fill_manual(values = c("#d9d9d9", "#252525")) +
#  scale_colour_manual(values = c("#d9d9d9", "#252525")) +
#  guides(fill = FALSE, color = FALSE) +
#  theme_niwot() 


#ggplot(data = ind_size, aes(x = type, y = length, fill = type)) +
#  geom_flat_violin(trim = FALSE, position = position_nudge(x = 0.15, y = 0), alpha = 0.8) +
#  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
#  labs(y = "Fish size (mm)") +
#  guides(fill = FALSE, color = FALSE) +
#  scale_fill_manual(values = c("#d9d9d9", "#252525")) +
#  coord_flip() +
#  theme_niwot() +
#  theme(axis.title.y = element_text(color = "white"),
#        axis.title.x = element_text(face = "bold", vjust = -1))


ind_size %>%
  select(type, length) %>%
  group_by(type) %>%
  summarise_at(vars(length), list('N' = ~length(.),
                                  'Mean' = ~ mean(.) %>% round(., digits = 3),
                                  'Std. Dev.' = ~ se(.) %>% round(., digits = 3),
                                  'Median' = ~ median(.) %>% round(., digits = 3),
                                  'Min' = ~ min(.),
                                  'Max' = ~ max(.))) %>%
  kable(., "simple")




##SIZE DISTRIBUTION BY TYPE & YEAR
ggplot(data = ind_size, aes(x = year, y = length, fill = type)) +
  introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
  geom_boxplot(width = .2, alpha = .6, show.legend = FALSE) +
  stat_summary(fun = "mean", mapping = aes(color = type), geom = "point", show.legend = F, position = position_dodge(.2)) +
  scale_x_discrete(name = "Sampling year") +
  scale_y_continuous(name = "Fish size (mm)") +
  scale_fill_manual(values = c("#d9d9d9", "#252525"), breaks = c('stream', 'lake')) +
  scale_colour_manual(values = c("#252525", "#969696")) +
  coord_flip() +
  #guides(fill = FALSE, color = FALSE) +
  theme_niwot() +
  theme(axis.title.y = element_text(face = "bold", vjust = 3),
        axis.title.x = element_text(face = "bold", vjust = -1))


ind_size %>%
  select(type, year, length) %>%
  group_by(type, year) %>%
  summarise_at(vars(length), list('N' = ~length(.),
                                  'Mean' = ~ mean(.) %>% round(., digits = 3),
                                  'Std. Dev.' = ~ sd(.) %>% round(., digits = 3),
                                  'Median' = ~ median(.),
                                  'Min' = ~ min(.),
                                  'Max' = ~ max(.))) %>%
  kable(., "simple")



