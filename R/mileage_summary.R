library(data.table)
library(zoo)
library(tidyr)

path <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"
test_data <- readRDS(paste0(path,"test_data_2005_2021.Rds"))

test_data <- test_data[,c("vehicle_id","test_date","test_mileage","postcode_area")]
test_data$test_year <- lubridate::year(test_data$test_date)

# When multiple tests in the same year take the last one
# Most common case is test, fail, and retest
test_data_summary <- test_data[test_data[, .I[which.max(test_date)], by=.(vehicle_id, test_year)]$V1]
test_data_summary$test_date <- NULL
rm(test_data)
gc()

test_data_wide <- tidyr::pivot_wider(test_data_summary, 
                                     id_cols = "vehicle_id",
                                     names_from = "test_year", 
                                     values_from = c("test_mileage","postcode_area"))




test_data_wide <- test_data_wide[,c("vehicle_id",
                                    paste0("test_mileage_",2005:2021),
                                    paste0("postcode_area_",2005:2021))]

saveRDS(test_data_wide,paste0(path,"mileage_wide_2005_2021.Rds"))
