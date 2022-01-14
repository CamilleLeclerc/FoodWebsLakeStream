rm(list=ls()) #Removes all objects from the current workspace (R memory)


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
library(dismo)
library(lubridate)
library(magrittr)
library(plyr)
library(tidyverse)
library(zoo)

mypath <- rprojroot::find_package_root_file
source("./R/misc.R")


##----------------------------
## LOAD PACKAGES AND FUNCTIONS
##----------------------------
##get all text files of temperature data (okp model)
list_of_files <- list.files(path = "./data-raw/lake_temperature", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)


##read and merge with one colomn for lake_ID
daily_temp_okp <- list_of_files %>% rlang::set_names(.) %>% map_df(read_table2, .id = "Lake_ID")
head(daily_temp_okp)
class(daily_temp_okp)
daily_temp_okp = as.data.frame(daily_temp_okp)

##get read of useless symbols
prep_data <- daily_temp_okp
prep_data$Lake_ID <- str_sub(daily_temp_okp$Lake_ID, start = 29, end = 33)
head(prep_data)


##separate year, month and day information
prep_data$year <- year(prep_data$date)
prep_data$month <- month(prep_data$date)
prep_data$day <- day(prep_data$date)

head(prep_data)
prep_data <- prep_data %>% dplyr::select(Lake_ID, date, year, month, day, tepi)


##From Alain's script
prep_data %<>%
  group_by(Lake_ID) %>%
  arrange(desc(date)) %>%
  nest()

options(mc.cores = 15)
prep_data$moving_avg <- parallel::mclapply(prep_data$data, function(x) {
  rollapplyr(data = x$tepi, width = 365, FUN = mean, na.rm = TRUE, fill = NA, partial = 183)
})
prep_data %<>%
  unnest()
prep_data %<>%
  filter(date > "2006-12-31") %<>%
  filter(date < "2017-01-01")

# Yearly avg
yearly_avg_temp <- prep_data %>%
  mutate(year = year(date)) %>%
  group_by(Lake_ID, year) %>%
  summarise(value = mean(moving_avg, na.rm = TRUE), raw_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# To play:
#sample_daily_avg_temp <- prep_data %>%
#  filter(Lake_ID %in% sample(prep_data$Lake_ID, 100))

daily_avg_temp_lake <- prep_data ; length(unique(daily_avg_temp_lake$Lake_ID))
yearly_avg_temp_lake <- yearly_avg_temp ; length(unique(yearly_avg_temp_lake$Lake_ID))
mysave(daily_avg_temp_lake, yearly_avg_temp_lake,
       dir = mypath("outputs"), overwrite = TRUE)
