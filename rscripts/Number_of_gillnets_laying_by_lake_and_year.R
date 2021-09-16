rm(list=ls())

getwd()

library(plyr)
library(tidyverse)
library(lubridate)

##get all text files
list_of_files <- list.files(path = "outputs/lake_individual_size/", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)


txt_files_df <- lapply(list_of_files, function(x) {read.table(file = x, header = T, sep ="")})
# Combine them
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame))

head(combined_df)
length(unique(combined_df$code_lac)) #284 lakes
length(unique(combined_df$camp_annee)) #15 years
length(unique(combined_df$id_campagne)) #459 id campagnes
length(unique(combined_df$id_prelev_poisson)) #12151 id_prelev_poisson
length(unique(combined_df$id_point_prelev)) #12027 id_point_prelev

nrow(unique(combined_df %>% select(code_lac, id_campagne))) #459
nrow(unique(combined_df %>% select(code_lac, camp_annee))) #457

unique(combined_df$strate)


##--------------------------------------------------------
## COMPILATION SAMPLING EFFORT - NUMBER OF GILLNETS LAYING
##--------------------------------------------------------

##Sampling effort
sampling_effort_benthic <- data.frame(code_lac = character(),
                                      camp_annee = numeric(),
                                      nb_id_campagne = numeric(),
                                      nb_id_prelev_poisson = numeric(),
                                      nb_id_point_prelev = numeric())

sampling_effort_pelagic <- data.frame(code_lac = character(),
                                      camp_annee = numeric(),
                                      nb_id_campagne = numeric(),
                                      nb_id_prelev_poisson = numeric(),
                                      nb_id_point_prelev = numeric())




for (i in 1:length(unique(combined_df$code_lac))){
  sub_data <- combined_df %>% filter(code_lac == unique(combined_df$code_lac)[i])
  
  for (j in 1:length(unique(sub_data$camp_annee))){
    subset_data <- sub_data %>% filter(camp_annee == unique(sub_data$camp_annee)[j])
    #subset.data.benthic <- subset.data %>% filter(strate %in% c("< 3 m [Protocole CEN]", "3 - 5,9 m [Protocole CEN]", "6 -11,9 m [Protocole CEN]", "12 - 19,9 m [Protocole CEN]", "20 - 34,9 m [Protocole CEN]", "35 - 49,9 m  [Protocole CEN]", "50 - 74,9 m  [Protocole CEN]", "> 75 m [Protocole CEN]"))
    #subset.data.pelagic <- subset.data %>% filter(strate %in% c("0 - 5,9 m Pelagique", "6 - 11,9 m Pelagique", "12 - 17,9 m Pelagique", "18 - 23,9 m Pelagique", "24 - 29,9 m Pelagique", "30 - 35,9 m Pelagique", "36 - 41,9 m Pelagique", "42 - 47,9 m Pelagique", "48 - 53,9 m Pelagique", "54 - 59,9 m Pelagique", "60 - 65,9 m Pelagique", "66 - 71,9 m Pelagique"))       
    subset_data_benthic <- subset.data %>% filter(cd_engin_peche == "FB")
    subset_data_pelagic <- subset.data %>% filter(cd_engin_peche %in% c("FP", "FPa"))    
    
    if(nrow(subset_data_benthic) !=0 ){ sub_sampling_effort_benthic <- data.frame(as.list(c(unique(subset_data_benthic$code_lac), unique(subset_data_benthic$camp_annee), length(unique(subset_data_benthic$id_campagne)),
                                                                                            length(unique(subset_data_benthic$id_prelev_poisson)), length(unique(subset_data_benthic$id_point_prelev))
    )))
    colnames(sub_sampling_effort_benthic) <- c("code_lac", "camp_annee", "nb_id_campagne", "nb_id_prelev_poisson", "nb_id_point_prelev")
    sampling_effort_benthic <- rbind(sampling_effort_benthic,  sub_sampling_effort_benthic)
    }
    
    if(nrow(subset_data_pelagic) !=0 ){ sub_sampling_effort_pelagic  <- data.frame(as.list(c(unique(subset_data_pelagic$code_lac), unique(subset_data_pelagic$camp_annee), length(unique(subset_data_pelagic$id_campagne)),
                                                                                             length(unique(subset_data_pelagic$id_prelev_poisson)), length(unique(subset_data_pelagic$id_point_prelev))
    )))
    colnames(sub_sampling_effort_pelagic) <- c("code_lac", "camp_annee", "nb_id_campagne", "nb_id_prelev_poisson", "nb_id_point_prelev")
    sampling_effort_pelagic <- rbind(sampling_effort_pelagic, sub_sampling_effort_pelagic)
    }
  }
}

rm(sub_data, sub_sampling_effort_benthic, sub_sampling_effort_pelagic, subset_data, subset_data_benthic, subset_data_pelagic, txt_files_df, i, j, list_of_files)

write.table(sampling_effort_benthic, "outputs/lake_number_gillnets_benthic.txt", row.names = FALSE)
write.table(sampling_effort_pelagic, "outputs/lake_number_gillnets_pelagic.txt", row.names = FALSE)
