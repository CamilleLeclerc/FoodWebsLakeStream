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
myload(dataset_lake_stream,
       dir = mypath("data"))
dataset_lake_stream <- dataset_lake_stream %>% dplyr::select(type, station, opcod, year, dbo, temp)
env_lake <-  dataset_lake_stream %>% dplyr::filter(type == "lake")
rm(dataset_lake_stream)




##--------------------------
## PHYSICO-CHEMISTRY DATASET
##--------------------------
db.physico.chemistry <- read.csv("data-raw/physico_chemistry_data.csv", sep = ",")
sort(unique(db.physico.chemistry$camp_annee))




#---------------
#OXYGENE DISSOUS
Oxy <- unique(db.physico.chemistry %>% filter(nom_parametre == "Oxygène dissous"))# %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3)) ##filter euphotic zone and chlorophyll a concentration and water as analysis support
length(unique(Oxy$code_lac)) #448 / 22 lakes
unique(Oxy$cd_unite)
Oxy <- Oxy %>% filter(code_lac %in% (env_lake %>% select(station) %>% unique(.) %>% t(.) %>% as.character(.)))
length(unique(Oxy$code_lac)) #255 / 10 lakes that fish and Oxy samples (/!\ samples are not necessarily taken in the same year)


analysis_total <- Oxy %>% dplyr::select(code_lac, id_prelev, id_analyse, date_debut_prel, nom_zone, nom_support_ana, nom_fraction_ana, nom_parametre, unite, resultat_ana)


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

colnames(yearly_avg_polluants)[1] <- "station"
colnames(yearly_avg_polluants)[4] <- "oxygen"


env_lake <- left_join(env_lake, yearly_avg_polluants %>% dplyr::select(station, year, oxygen), by = c('station', 'year'))
summary(env_lake)
rm(analysis_total, monthly_avg_polluants, Oxy, prep_data, yearly_avg_polluants)




#----------------
#TOTAL PHOSPHORUS
PT <- unique(db.physico.chemistry %>% filter(nom_parametre == "Phosphore total") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3)) ##filter euphotic zone and chlorophyll a concentration and water as analysis support
#PT <- PT %>% filter(cd_unite %in% c("177", "162"))
length(unique(PT$code_lac)) #446 / 284 lakes
unique(PT$cd_unite)
PT <- PT %>% filter(code_lac %in% (env_lake %>% select(station) %>% unique(.) %>% t(.) %>% as.character(.)))
length(unique(PT$code_lac)) #255 / 174 lakes that fish and Oxy samples (/!\ samples are not necessarily taken in the same year)


analysis_total <- PT %>% dplyr::select(code_lac, id_prelev, id_analyse, date_debut_prel, nom_zone, nom_support_ana, nom_fraction_ana, nom_parametre, unite, resultat_ana)


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

colnames(yearly_avg_polluants)[1] <- "station"
colnames(yearly_avg_polluants)[4] <- "phosphore"


env_lake <- left_join(env_lake, yearly_avg_polluants %>% dplyr::select(station, year, phosphore), by = c('station', 'year'))
summary(env_lake)
rm(analysis_total, monthly_avg_polluants, PT, prep_data, yearly_avg_polluants)




#-----------------
#KJELDAHL NITROGEN
NK <- unique(db.physico.chemistry %>% filter(nom_parametre == "Azote Kjeldahl") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3)) ##filter euphotic zone and chlorophyll a concentration and water as analysis support
length(unique(NK$code_lac)) #284 lakes
unique(NK$cd_unite)
NK <- NK %>% filter(code_lac %in% (env_lake %>% select(station) %>% unique(.) %>% t(.) %>% as.character(.)))
length(unique(NK$code_lac)) #174 lakes that fish and Oxy samples (/!\ samples are not necessarily taken in the same year)


analysis_total <- NK %>% dplyr::select(code_lac, id_prelev, id_analyse, date_debut_prel, nom_zone, nom_support_ana, nom_fraction_ana, nom_parametre, unite, resultat_ana)


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

colnames(yearly_avg_polluants)[1] <- "station"
colnames(yearly_avg_polluants)[4] <- "azote_kjeldahl"


env_lake <- left_join(env_lake, yearly_avg_polluants %>% dplyr::select(station, year, azote_kjeldahl), by = c('station', 'year'))
summary(env_lake)
rm(analysis_total, monthly_avg_polluants, NK, prep_data, yearly_avg_polluants)




#--------
#NITRATES
Na <- unique(db.physico.chemistry %>% filter(nom_parametre == "Nitrates") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3)) ##filter euphotic zone and chlorophyll a concentration and water as analysis support
length(unique(Na$code_lac)) #284 lakes
unique(Na$cd_unite)
Na <- Na %>% filter(code_lac %in% (env_lake %>% select(station) %>% unique(.) %>% t(.) %>% as.character(.)))
length(unique(Na$code_lac)) #174 lakes that fish and Oxy samples (/!\ samples are not necessarily taken in the same year)
#Na <- Na %>% dplyr::filter(resultat_ana < 10)

analysis_total <- Na %>% dplyr::select(code_lac, id_prelev, id_analyse, date_debut_prel, nom_zone, nom_support_ana, nom_fraction_ana, nom_parametre, unite, resultat_ana)


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

colnames(yearly_avg_polluants)[1] <- "station"
colnames(yearly_avg_polluants)[4] <- "nitrates"


env_lake <- left_join(env_lake, yearly_avg_polluants %>% dplyr::select(station, year, nitrates), by = c('station', 'year'))
summary(env_lake)
rm(analysis_total, monthly_avg_polluants, Na, prep_data, yearly_avg_polluants)




#--------
#NITRITES
Ni <- unique(db.physico.chemistry %>% filter(nom_parametre == "Nitrites") %>% filter(cd_zone == 1) %>% filter(cd_support_ana == 3)) ##filter euphotic zone and chlorophyll a concentration and water as analysis support
length(unique(Ni$code_lac)) #284 lakes
unique(Ni$cd_unite)
Ni <- Ni %>% filter(code_lac %in% (env_lake %>% select(station) %>% unique(.) %>% t(.) %>% as.character(.)))
length(unique(Ni$code_lac)) #174 lakes that fish and Oxy samples (/!\ samples are not necessarily taken in the same year)


analysis_total <- Ni %>% dplyr::select(code_lac, id_prelev, id_analyse, date_debut_prel, nom_zone, nom_support_ana, nom_fraction_ana, nom_parametre, unite, resultat_ana)


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

colnames(yearly_avg_polluants)[1] <- "station"
colnames(yearly_avg_polluants)[4] <- "nitrites"


env_lake <- left_join(env_lake, yearly_avg_polluants %>% dplyr::select(station, year, nitrites), by = c('station', 'year'))
summary(env_lake)
rm(analysis_total, monthly_avg_polluants, Ni, prep_data, yearly_avg_polluants)


env_lake$nitrogen <- (env_lake$azote_kjeldahl + env_lake$nitrates + env_lake$nitrites)
env_lake <- select(env_lake, -azote_kjeldahl, -nitrates, -nitrites)
summary(env_lake)
mysave(env_lake, dir = mypath("data"), overwrite = TRUE)




##------------
## SEM AND PCA
##------------
env_lake <- na.omit(env_lake)

## R sert-up
library(sf)
library(tidyverse)
library(magrittr)
library(piecewiseSEM)
library(corrplot)
library(ade4)
library(factoextra)

source(mypath("R", "misc.R"))


## SEM
lm_list <- list(
  lm(oxygen ~ dbo + temp, env_lake),
  lm(dbo ~ nitrogen + phosphore + temp, env_lake)
)
sem <- as.psem(lm_list)
summary(sem)
plot(sem)

performance::check_collinearity(lm_list[[1]])
performance::check_collinearity(lm_list[[2]])

## PCA
env_var <- c("dbo", "temp", "oxygen", "nitrogen", "phosphore")
corrplot(cor(env_lake[, env_var]), type = "upper", diag = FALSE)

pca <- dudi.pca(env_lake[, env_var], nf = 3, scannf = FALSE)
cowplot::plot_grid(
  fviz_pca_var(pca, axes = c(1, 2)),
  fviz_pca_var(pca, axes = c(2, 3))
)
