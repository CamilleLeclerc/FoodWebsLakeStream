rm(list=ls()) #Removes all objects from the current workspace (R memory)


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(devtools)
library(dplyr)
library(lubridate)
library(magrittr)
library(stringi)
library(tidyverse)
library(zoo) 

mypath <- rprojroot::find_package_root_file
source("./R/misc.R")


##-------------
## FISH DATASET
##-------------
db.fish <- read.csv("data-raw/fish_data.csv", sep = ";") #fish_data.csv is too large to be on github ; it is listed in gitignore
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
db.physico.chemistry <- read.csv("data-raw/physico_chemistry_data.csv", sep = ",")
sort(unique(db.physico.chemistry$camp_annee))
DBO5 <- unique(db.physico.chemistry %>% filter(nom_parametre == "Demande Biochimique en oxygène en 5 jours (D.B.O.5)") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3)) ##filter euphotic zone and chlorophyll a concentration and water as analysis support
length(unique(DBO5$code_lac)) #277 lakes
DBO5 <- DBO5 %>% filter(code_lac %in% unique(db.fish$code_lac))
length(unique(DBO5$code_lac)) #189 lakes that fish and DBO5 samples (/!\ samples are not necessarily taken in the same year)


analysis_total <- DBO5 %>% dplyr::select(code_lac, id_prelev, id_analyse, date_debut_prel, nom_zone, nom_support_ana, nom_fraction_ana, nom_parametre, unite, resultat_ana)
#ind <- as.data.frame(duplicated(analysis_total))
#analysis_total_bis <- analysis_total
#analysis_total_bis$year <- year(analysis_total_bis$date_debut_prel)
#analysis_total_bis$sample <- 1
#ATB <- analysis_total_bis %>%
#  select(code_lac, year, sample) %>%
#  group_by(code_lac, year) %>% 
#  summarise(sum(sample)) %>%
#  set_colnames(c("code_lac", "camp_annee", "nb_sample"))
#rm(analysis_total_bis, ATB)


##From Alain's script
prep_data <- analysis_total %>%
  mutate(
    value = as.numeric(resultat_ana),
    year_month = ymd(paste0(year(date_debut_prel),"-", month(date_debut_prel), "-15"))
  ) %>%
  group_by(code_lac, year_month) %>%
  summarise(value = mean(value))

prep_data %<>%
  group_by(code_lac) %>%
  arrange(desc(year_month)) 

prep_data %<>%
  nest()


options(mc.cores = 15)
prep_data$moving_avg <- parallel::mclapply(prep_data$data, function(x) {
  rollapplyr(data = x$value, width = 5, FUN = mean, na.rm = TRUE, fill = NA, partial = 1)
})
prep_data %<>%
  unnest()

monthly_avg_polluants <- prep_data

# Yearly avg
yearly_avg_polluants <- monthly_avg_polluants %>%
  mutate(year = year(year_month)) %>%
  filter(!year %in% c(1994, 2018)) %>% # To epure moving avg NA
  group_by(code_lac, year) %>%
  summarise(value = mean(moving_avg, na.rm = TRUE), raw_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# To play:
#sample_monthly_avg_polluants <- monthly_avg_polluants %>%
#  filter(code_lac %in% sample(monthly_avg_polluants$code_lac, 100))

monthly_avg_dbo5_lake <- monthly_avg_polluants
monthly_avg_dbo5_lake <- monthly_avg_dbo5_lake %>% filter(code_lac %in% c("ABB39","AIG15","AIG73","ALE2B","ALL01","ALZ81","AMA10","ANN74","ARJ40","AST32",
                                                                          "AUR40","AVE34","BAG12","BAI08","BIS40","BMC16","BMS40","BOU73","BOU88","BSA58",
                                                                          "BSF31","CAL2B","CAR66","CAZ40","CHA25","CHA38","CHO04","COI39","ECH33","ECL28",
                                                                          "ENT25","FIG2A","FIL09","GER88","GLC39","HAU19","ILA39","JOU11","LAC33","LAD16",
                                                                          "LAF38","LAR31","LDC25","LDC63","LEO40","LER27a","LGM27","LIE52","LKW68","LON88",
                                                                          "LPC38","LRO39","MAD55","MAU12","MIC68","MON38","MON71","NAN01","OSP2A","PAL38",
                                                                          "PAR12","PAR40","PAR54","PET38","PPE14","PRA33","QUI04","RAB61","RAV34","RCB01",
                                                                          "RCS70","REM25","SAL34","SAU38","SCA83","SCR04","SET58","SOU40","SPO25","SYL01",
                                                                          "TOL2A","TOR82","TRA02","TRE19","TSC32","VAL39","VEZ50","VFO08","VIL12","VIN66",
                                                                          "VOU39")) #list of lakes that have one common year of fish and DBO5 samples
length(unique(monthly_avg_dbo5_lake$code_lac)) #91 lakes

yearly_avg_dbo5_lake <- yearly_avg_polluants
yearly_avg_dbo5_lake <- yearly_avg_dbo5_lake %>% filter(code_lac %in% c("ABB39","AIG15","AIG73","ALE2B","ALL01","ALZ81","AMA10","ANN74","ARJ40","AST32",
                                                                        "AUR40","AVE34","BAG12","BAI08","BIS40","BMC16","BMS40","BOU73","BOU88","BSA58",
                                                                        "BSF31","CAL2B","CAR66","CAZ40","CHA25","CHA38","CHO04","COI39","ECH33","ECL28",
                                                                        "ENT25","FIG2A","FIL09","GER88","GLC39","HAU19","ILA39","JOU11","LAC33","LAD16",
                                                                        "LAF38","LAR31","LDC25","LDC63","LEO40","LER27a","LGM27","LIE52","LKW68","LON88",
                                                                        "LPC38","LRO39","MAD55","MAU12","MIC68","MON38","MON71","NAN01","OSP2A","PAL38",
                                                                        "PAR12","PAR40","PAR54","PET38","PPE14","PRA33","QUI04","RAB61","RAV34","RCB01",
                                                                        "RCS70","REM25","SAL34","SAU38","SCA83","SCR04","SET58","SOU40","SPO25","SYL01",
                                                                        "TOL2A","TOR82","TRA02","TRE19","TSC32","VAL39","VEZ50","VFO08","VIL12","VIN66",
                                                                        "VOU39")) #list of lakes that have one common year of fish and DBO5 samples
length(unique(yearly_avg_dbo5_lake$code_lac)) #91 lakes

mysave(monthly_avg_dbo5_lake, yearly_avg_dbo5_lake,
       dir = mypath("outputs"), overwrite = TRUE)

