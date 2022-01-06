##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(devtools)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(magrittr)
library(rnaturalearth)
library(sf)
library(tidyverse)
source_url("https://raw.githubusercontent.com/alaindanet/SizeTrophicInteractions/main/R/building_dataset.R")


##-------------
## FISH DATASET
##-------------
db.fish <- read.csv("data/fish_data.csv", sep = ";") #fish_data.csv is too large to be on github ; it is listed in gitignore
head(db.fish)

##Focused period for the project is 2005 - 2018
sort(unique(db.fish$camp_annee))
db.fish <- db.fish %>% filter(camp_annee < 2019)
nrow(db.fish)
sort(unique(db.fish$camp_annee))

##Delete the years of partial fish sampling within Annecy and Leman lakes
db.fish <- db.fish %>% filter(! ((code_lac == "ANN74" & camp_annee == "2012") |
                                   (code_lac == "LEM74" & camp_annee == "2015")))

##Rename fish name
length(unique(db.fish$code_lac))
length(unique(db.fish$nom_latin_taxon))
sort(unique(db.fish$nom_latin_taxon))

db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Aspius aspius"] <- "Leuciscus aspius"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Blennius fluviatilis"] <- "Salaria fluviatilis"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Carassius auratus auratus"] <- "Carassius auratus"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Carassius auratus gibelio"] <- "Carassius gibelio"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Chondrostoma toxostoma"] <- "Parachondrostoma toxostoma"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Coregonus"] <- "Coregonus lavaretus"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Cyprinidaes"] <- "Cyprinidae"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Gasterosteus aculeatus aculeatus"] <- "Gasterosteus aculeatus"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Gymnocephalus cernuus"] <- "Gymnocephalus cernua"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Hybride breme-gardon"] <- "Cyprinidae"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Hybrides de cyprinides"] <- "Cyprinidae"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Leuciscus cephalus"] <- "Squalius cephalus"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Leuciscus souffia"] <- "Telestes souffia"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Liza aurata"] <- "Chelon auratus"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Liza ramada"] <- "Chelon ramada"
db.fish$nom_latin_taxon[db.fish$nom_latin_taxon == "Salvelinus umbla"] <- "Salvelinus alpinus"

##Delete no-fish species
db.fish <- db.fish %>% filter(!(nom_latin_taxon %in% c("Astacus astacus", "Eriocheir sinensis", "Orconectes limosus", "Pacifastacus leniusculus", "Procambarus clarkii")))


##--------------------------
## PHYSICO-CHEMISTRY DATASET
##--------------------------
db.physico.chemistry <- read.csv("data/physico_chemistry_data.csv", sep = ",")
sort(unique(db.physico.chemistry$camp_annee))
db.physico.chemistry <- db.physico.chemistry %>% filter(camp_annee > 2004)

DBO5 <- db.physico.chemistry %>% filter(nom_parametre == "Demande Biochimique en oxygène en 5 jours (D.B.O.5)") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3) ##filter euphotic zone and chlorophyll a concentration and water as analysis support
length(unique(DBO5$code_lac)) #277 lakes

PT <- db.physico.chemistry %>% filter(nom_parametre == "Phosphore total") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3) ##filter euphotic zone and total phosphorus concentration and water as analysis support
length(unique(PT$code_lac)) #284 lakes

NO3 <- db.physico.chemistry %>% filter(nom_parametre == "Nitrates") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3) ##filter euphotic zone and nitrate concentration and water as analysis support
length(unique(NO3$code_lac)) #284 lakes


##--------------------------------------------
## COMMON LAKES BETWEEN FISH AND DBO5 DATASETS
##-----------------------------------------------------------------------------------------
## 1.common association lake-year between fish / DBO5 / total phosphorus / nitrate datasets
##-----------------------------------------------------------------------------------------
length(unique(paste(db.fish$code_lac, db.fish$camp_annee, sep = "_"))) #451 associations lake-year
length(unique(paste(DBO5$code_lac, DBO5$camp_annee, sep = "_"))) #597 associations lake-year
length(unique(paste(PT$code_lac, PT$camp_annee, sep = "_"))) #686 associations lake-year
length(unique(paste(NO3$code_lac, NO3$camp_annee, sep = "_"))) #686 associations lake-year

length(intersect(unique(paste(DBO5$code_lac, DBO5$camp_annee, sep = "_")), unique(paste(db.fish$code_lac, db.fish$camp_annee, sep = "_")))) #116 associations lake-year in common between db.fish and DBO5

length(intersect(unique(paste(PT$code_lac, PT$camp_annee, sep = "_")), unique(paste(db.fish$code_lac, db.fish$camp_annee, sep = "_")))) #143 associations lake-year in common between db.fish and PT
length(intersect(unique(paste(PT$code_lac, PT$camp_annee, sep = "_")), unique(paste(DBO5$code_lac, DBO5$camp_annee, sep = "_")))) #597 associations lake-year in common between DBO5 and PT

length(intersect(unique(paste(NO3$code_lac, NO3$camp_annee, sep = "_")), unique(paste(db.fish$code_lac, db.fish$camp_annee, sep = "_")))) #143 associations lake-year in common between db.fish and NO3
length(intersect(unique(paste(NO3$code_lac, NO3$camp_annee, sep = "_")), unique(paste(DBO5$code_lac, DBO5$camp_annee, sep = "_")))) #597 associations lake-year in common between DBO5 and NO3
length(intersect(unique(paste(NO3$code_lac, NO3$camp_annee, sep = "_")), unique(paste(PT$code_lac, PT$camp_annee, sep = "_")))) #686 associations lake-year in common between PT and NO3


db.fish$lac_year <- paste(db.fish$code_lac, db.fish$camp_annee, sep = "_")
DBO5$lac_year <- paste(DBO5$code_lac, DBO5$camp_annee, sep = "_")
PT$lac_year <- paste(PT$code_lac, PT$camp_annee, sep = "_")
NO3$lac_year <- paste(NO3$code_lac, NO3$camp_annee, sep = "_")

db.fish$info_fish <- 1
DBO5$info_dbo5 <- 1
PT$info_pt <- 1
NO3$info_no3 <- 1

final.lake.year <- left_join(unique(db.fish %>% select(code_lac, camp_annee, lac_year, info_fish)),
                             unique(DBO5 %>% select(lac_year, info_dbo5)),
                             by = "lac_year")
final.lake.year <- final.lake.year %>% mutate_all(~replace(., is.na(.), 0))
final.lake.year <- final.lake.year %>% filter(info_fish == 1 & info_dbo5 == 1)
length(unique(final.lake.year$code_lac)) #97 lakes
final.lake.year <- final.lake.year %>% filter(! code_lac %in% c("CAN77c", "CER95", "GON57", "MUN67", "STO57", "VSM77")) #Lakes without temperature data from OKP model
length(unique(final.lake.year$code_lac)) #91 lakes

lake_info <- read.csv("outputs/lake_dataset.txt", sep="")
lake_info <- unique(lake_info %>% select(cd.lac, lat_plando, long_plando))
colnames(lake_info)[1] <- "code_lac"
final.lake.year <- left_join(final.lake.year, lake_info, by = "code_lac")


# 1.1.map showing the location of the potential study lakes 
worldmap <- ne_countries(continent = 'europe', scale = 'large', type = 'countries', returnclass = 'sf')
fr <- data.frame(Country = "France", Focus = "YES") 
world_joined <- left_join(worldmap, fr, by = c("name" = "Country"))
francemap <- ne_countries(country = 'france', scale = 'large', type = 'countries', returnclass = 'sf')
lakes <- ne_download(scale = 10, type = 'lakes', category = 'physical')
rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')
sf::sf_use_s2(FALSE)
francelakes <- st_intersection(st_as_sf(lakes), st_as_sf(francemap))
francerivers <- st_intersection(st_as_sf(rivers), st_as_sf(francemap))
rm(worldmap, fr, lakes, rivers)

pfinal.lake.year <- ggplot() +
  geom_sf(data = world_joined, fill = "white", color = "black", size = 0.05) +
  geom_sf(data = francemap, fill = gray(0.9), color = "black", size = 0.25) +
  geom_sf(data = francerivers, col = '#6baed6', size = 0.25) +  
  geom_sf(data = francelakes, col = '#6baed6', fill = '#6baed6', size = 0.05) +  
  geom_point(data = final.lake.year, aes(x = long_plando, y = lat_plando), shape = 21, colour = "black", fill = "#e6f598", size = 3) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(which_north = "true", location = "tr", height = unit(0.5, "cm"), width = unit(0.5, "cm"), style = north_arrow_orienteering(fill = c("black", "black"), text_size = 6)) +           
  coord_sf(xlim = c(-5, 9.75), ylim = c(41.3, 51.5), expand = FALSE) +
  theme(title=element_text(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major= element_blank(), 
        panel.background= element_blank()) 
pfinal.lake.year


# 1.2.plot showing sampling years of the potential study lakes
camp.annee <- ggplot(data = final.lake.year, aes(x = camp_annee)) +
                geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", color = "black", fill = "#252525") +
                labs(x = paste("Année de campagne\n", "d'échantillonnage"), y = "Nombre de plans d'eau") +
                scale_x_continuous(limits = c(2004, 2017), breaks = seq(2005, 2016, 5)) +
                theme(axis.title.x = element_text(face = "bold", vjust = -1, size = 18, colour="black"),
                axis.title.y = element_text(face = "bold", vjust = 3, size = 18, colour="black"),
                axis.text = element_text(size = 16, colour="black"), 
                axis.line.x = element_line(color="black"), 
                axis.line.y = element_line(color="black"),
                panel.border = element_blank(),
                panel.grid.major.x = element_blank(),                                          
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.background = element_rect(fill="white"))
camp.annee


# 1.3.plot showing the number of sampling years per potential study lakes 
camp.biol.per.lake <- final.lake.year
camp.biol.per.lake$nb_camp <- 1
camp.biol.per.lake <- as.data.frame(camp.biol.per.lake %>% select(code_lac, nb_camp) %>% group_by(code_lac) %>% summarise_each(list(sum)))

nb.camp.annee <- ggplot(data = camp.biol.per.lake, aes(x = nb_camp)) +
                  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", color = "black", fill = "#252525") +
                  labs(x = paste("Nombre de campagne\n", "d'échantillonnage"), y = "Nombre de plans d'eau") +
                  theme(axis.title.x = element_text(face = "bold", vjust = -1, size = 18, colour="black"),
                  axis.title.y = element_text(face = "bold", vjust = 3, size = 18, colour="black"),
                  axis.text = element_text(size = 16, colour="black"), 
                  axis.line.x = element_line(color="black"), 
                  axis.line.y = element_line(color="black"),
                  panel.border = element_blank(),
                  panel.grid.major.x = element_blank(),                                          
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.background = element_rect(fill="white"))
nb.camp.annee


## 2.final DBO5 dataset
##-----------------------------------------------------------------------------------------
DBO5_subset <- DBO5 %>% filter(lac_year %in% final.lake.year$lac_year)
unique(DBO5_subset$nom_parametre) #Demande Biochimique en oxygène en 5 jours (D.B.O.5)
unique(DBO5_subset$nom_zone) #Zone euphotique
unique(DBO5_subset$nom_support_ana) #Eau
unique(DBO5_subset$nom_fraction_ana) #Eau brute ; Fraction inconnue de l eau
unique(DBO5_subset$unite) #mg(O2)/L ; mg/L
summary(DBO5_subset$resultat_ana)

DBO5_dbfinal <- DBO5_subset %>%
  select(code_lac, camp_annee, resultat_ana) %>%
  group_by(code_lac, camp_annee) %>% 
  summarise(mean(resultat_ana)) %>%
  set_colnames(c("code_lac", "camp_annee", "dbo5_mean"))


##--------------------------------------
## RELATIONSHIPT BETWEEN DBO5 / PT / NO3
##------------------------------------------------------------
## 1.preparation of DBO5 / total phosphorus / nitrate datasets
##------------------------------------------------------------
dbo5 <- DBO5
unique(dbo5$nom_parametre) #Demande Biochimique en oxygène en 5 jours (D.B.O.5)
unique(dbo5$nom_zone) #Zone euphotique
unique(dbo5$nom_support_ana) #Eau
unique(dbo5$nom_fraction_ana) #Eau brute ; Fraction inconnue de l eau
unique(dbo5$unite) #mg(O2)/L ; mg/L
summary(dbo5$resultat_ana)

dbo5 <- dbo5 %>%
  select(code_lac, camp_annee, resultat_ana) %>%
  group_by(code_lac, camp_annee) %>% 
  summarise(mean(resultat_ana)) %>%
  set_colnames(c("code_lac", "camp_annee", "dbo5_mean"))


pt <- PT
unique(pt$nom_parametre) #Phosphore total
unique(pt$nom_zone) #Zone euphotique
unique(pt$nom_support_ana) #Eau
unique(pt$nom_fraction_ana) #Eau brute ; Fraction inconnue de l eau
unique(pt$unite) #mg(P)/L ; mg/L
summary(pt$resultat_ana)

pt <- pt %>%
  select(code_lac, camp_annee, resultat_ana) %>%
  group_by(code_lac, camp_annee) %>% 
  summarise(mean(resultat_ana)) %>%
  set_colnames(c("code_lac", "camp_annee", "pt_mean"))


no3 <- NO3
unique(no3$nom_parametre) #Nitrates
unique(no3$nom_zone) #Zone euphotique
unique(no3$nom_support_ana) #Eau
unique(no3$nom_fraction_ana) #Eau brute ; Fraction inconnue de l eau ; Phase aqueuse de l eau (filtrée, centrifugée...)
unique(no3$unite) #mg(NO3)/L ; mg(N)/L ; mg/L
summary(no3$resultat_ana)
no3 <- no3 %>% drop_na(resultat_ana)
summary(no3$resultat_ana)

no3 <- no3 %>%
  select(code_lac, camp_annee, resultat_ana) %>%
  group_by(code_lac, camp_annee) %>% 
  summarise(mean(resultat_ana)) %>%
  set_colnames(c("code_lac", "camp_annee", "no3_mean"))


nutrients <- left_join(dbo5, pt, by = c("code_lac", "camp_annee"))
nutrients <- left_join(nutrients, no3, by = c("code_lac", "camp_annee"))
summary(nutrients)

shapiro.test(nutrients$dbo5_mean) #non-normal
shapiro.test(nutrients$pt_mean) #non-normal
shapiro.test(nutrients$no3_mean) #non-normal
nutrients$dbo5_mean <- scale(log(nutrients$dbo5_mean))
nutrients$pt_mean <- scale(log(nutrients$pt_mean))
nutrients$no3_mean <- scale(log(nutrients$no3_mean))

ggplot(nutrients, aes(x = dbo5_mean, y = pt_mean)) + 
  geom_point()+
  geom_smooth(method = lm)
cor(nutrients$dbo5_mean, nutrients$pt_mean, method = "spearman")

ggplot(nutrients, aes(x = dbo5_mean, y = no3_mean)) + 
  geom_point()+
  geom_smooth(method = lm)
cor(nutrients$dbo5_mean, nutrients$no3_mean, method = "spearman")
