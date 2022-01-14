##--------------
## LOAD PACKAGES
##--------------
library(ggplot2)
library(knitr)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthhires)
library(tidyverse)

##-------------
## LOAD DATASET
##-------------
db.lake <- read_delim("outputs/lake_dataset.txt", delim = " ")

##-------------------------
## LIST OF SPECIES TO CHECK
##-------------------------
sp.to.check <- c("Acipenser ruthenus", "Carassius auratus", "Carassius gibelio", "Chelon auratus", "Chelon ramada", "Coregonus lavaretus", "Hypophthalmichthys molitrix", "Leuciscus aspius", "Neogobius melanostomus", "Ponticola kessleri", "Salvelinus alpinus", "Salvelinus namaycush", "Scardinius hesperidicus")

##-------------------------------------------
## NUMBER OF LAKES WHERE THE 13 SPECIES OCCUR
##-------------------------------------------
db.lake %>%
  filter(taxon %in% sp.to.check) %>%
  select(taxon, cd.lac) %>%
  unique(.) %>%
  group_by(taxon) %>%
  summarise(occurrence = n()) %>%
  knitr::kable()

##-----------------------------------------
## LIST OF LAKES WHERE THE 13 SPECIES OCCUR
##-----------------------------------------
db.lake %>%
  filter(taxon %in% sp.to.check) %>%
  select(taxon, cd.lac) %>%
  unique(.) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = taxon, values_from = present) %>%
  mutate_at(vars(sp.to.check), ~ifelse(is.na(.), 0, 1)) %>%
  knitr::kable()

##-----------------------------------------
## PLOT OF LAKES WHERE THE 13 SPECIES OCCUR
##-----------------------------------------
worldmap <- ne_countries(continent = 'europe', scale = 'large', type = 'countries', returnclass = 'sf')
francemap <- ne_countries(country = 'france', scale = 'large', type = 'countries', returnclass = 'sf')


ggplot() +
  geom_sf(data = worldmap, fill = "#ffffff", color = "#000000", size = 0.05) +
  geom_sf(data = francemap, fill = "#d9d9d9", color = "#000000", size = 0.25) +
  geom_point(data = db.lake %>%
                      filter(taxon %in% sp.to.check) %>%
                      select(taxon, lat_plando, long_plando) %>%
                      unique(.),
             aes(x = long_plando, y = lat_plando), shape = 21, colour = "#000000", fill = "#9e0142", size = 1) +
  coord_sf(xlim = c(-5, 9.75), ylim = c(41.3, 51.5), expand = FALSE) +
  facet_wrap(~ taxon) +
  theme(title = element_text(),
        plot.title = element_text(margin = margin(20,20,20,20), size = 18, hjust = 0.5),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),  axis.title.y = element_blank(),
        panel.grid.major = element_blank(), panel.background = element_blank(),
        strip.background = element_rect(fill = "#000000", color = "#000000", size = 1, linetype = "solid"),
        strip.text.x = element_text(size = 12, color = "#ffffff", face = "bold"),
        panel.border = element_rect(colour = "#000000", fill = NA, size = 1)) 
