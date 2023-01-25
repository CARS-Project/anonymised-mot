# Clean 2017
# File is partially corupped and R can't unzip
path_in <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/raw"
path_out <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"



files <- list.files(file.path(path_in,"dft_test_result_2017"), pattern = ".csv", full.names = TRUE)

res <- list()
for(j in seq(1, length(files))){
  message("Reading ",files[j]," ",round(file.size(files[j])/1048576)," Mb")
  sub = read_delim(files[j], 
                    delim = "|",
                    escape_backslash = FALSE,
                    escape_double = FALSE,
                    col_types = cols(
                      test_id = col_integer(),
                      vehicle_id = col_integer(),
                      test_date = col_date(format = ""),
                      test_class_id = col_integer(),
                      test_type = col_factor(),
                      test_result = col_factor(),
                      test_mileage = col_integer(),
                      postcode_area = col_factor(),
                      make = col_factor(),
                      model = col_factor(),
                      colour = col_factor(),
                      fuel_type = col_factor(),
                      cylinder_capacity = col_integer(),
                      first_use_date = col_date(format = "")
                    ), lazy = FALSE)

    res[[j]] <- sub
}

res <- dplyr::bind_rows(res)

saveRDS(res, paste0(path_out,"dft_test_result_2017",yr,".Rds"))

# Item
message("Items ",yr)

files <- list.files(file.path(path_in,"dft_test_item_2017"), pattern = ".csv", full.names = TRUE)

res <- list()
for(j in seq(1, length(files))){
  sub <- read_delim(files[j], 
                    delim = "|", 
                  col_types = readr::cols(
                    test_id = col_integer(),
                    rfr_id = col_integer(),
                    rfr_type_code = col_factor(),
                    location_id = col_integer(),
                    dangerous_mark = col_factor()
                  ), 
                  lazy = FALSE)
  
  res[[j]] <- sub
}
res <- dplyr::bind_rows(res)

saveRDS(res, paste0(path_out,"test_item_",yr,".Rds"))
