library(dplyr)
library(data.table)
path <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"

mot <- readRDS(paste0(path,"test_result_2005_2021.Rds"))
car_data <- mot[,c("vehicle_id","make","model", "colour", "fuel_type","cylinder_capacity","first_use_date")]

# Mostly a single characteristics has changes e.g colour or different spelling of model. A few have change fuel type
# Make a simple table using the most common value.

# Fist step group by everything, this will creat duplicates but massivle reduce the size of the dataset

car_data_summary = car_data[, .N,
         by=.(vehicle_id, make, model, colour, fuel_type, cylinder_capacity, first_use_date)]

test_data <- mot[,c("vehicle_id","test_id","test_date","test_class_id","test_type","test_result",
                    "test_mileage","postcode_area")]
test_data <- test_data[order(test_data$vehicle_id),]

saveRDS(test_data,paste0(path,"test_data_2005_2021.Rds"))

# Second Step 
dup_id <- car_data_summary$vehicle_id[duplicated(car_data_summary$vehicle_id)]
dup_id <- unique(dup_id)

is_dup <- car_data_summary$vehicle_id %in% dup_id

car_data_summary_single <- car_data_summary[!is_dup, ]
car_data_summary_dups <- car_data_summary[is_dup, ]
car_data_summary_dups <- car_data_summary_dups[order(car_data_summary_dups$N, decreasing = TRUE),]
car_data_summary_dups <- car_data_summary_dups[!duplicated(car_data_summary_dups$vehicle_id),]

car_data_summary_all <- rbind(car_data_summary_single, car_data_summary_dups)
car_data_summary_all$N <- NULL
car_data_summary_all <- car_data_summary_all[order(car_data_summary_all$first_use_date),]

saveRDS(car_data_summary_all,paste0(path,"car_data_2005_2021.Rds"))

rm(car_data, car_data_summary, car_data_summary_dups, car_data_summary_single, mot)

# Make a table of unique make/model for lookup
car_models <- car_data_summary_all[,c("make","model")]
car_models <- car_models[, .N, by=.(make, model)]
car_models <- car_models[order(car_models$N, decreasing = TRUE),]

saveRDS(car_models,paste0(path,"unique_make_model.Rds"))




