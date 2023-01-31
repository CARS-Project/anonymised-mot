path <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"

years <- 2005:2021

library(dplyr)
library(data.table)
library(foreign)
library(purrr)
library(future)
library(furrr)
library(progressr)
library(tidyr)
library(zoo)

mot <- list()

for(i in 1:length(years)){
  year <- years[i]
  message(year)
  mot_sub <- readRDS(file.path(path,paste0("test_result_",year,".Rds")))
  mot_sub$year <- year
  mot[[i]] <-  mot_sub
}

rm(mot_sub)
gc()
mot <- data.table::rbindlist(mot)
saveRDS(mot,paste0(path,"test_result_2005_2021.Rds"))
gc()
