# Clean Data
library(readr)
source("R/cleaning_functions.R")

path_in <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/raw"
path_out <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"
years <- 2019:2021

zips <- list.files(path_in)
zips <- zips[grepl(".zip",zips)|grepl(".txt.gz",zips)]
zips <- zips[!grepl("lookup",zips)]

for(yr in years){
  
  zip_yr <- zips[grepl(yr,zips)]
  if(length(zip_yr) != 2){stop("wrong number of zipped folders for ",yr)}
  zip_yr_item <- zip_yr[grepl("item",zip_yr)]
  zip_yr_result <- zip_yr[grepl("result",zip_yr)]
  
  
  if(yr %in% c(2005:2016)){
    #Gzipped
    if(yr %in% c(2015,2016)){
      
      message("Reading ",zip_yr_result," ",format(object.size(file.path(path_in,zip_yr_result)), units = "Mb"))
      con <- gzfile(file.path(path_in,zip_yr_result))
      data <- readLines(con)
      data <- strsplit(data,"|", fixed = TRUE)
      file <- clean_results(data)
      rm(data, con)
      
    } else {
      
      message("Reading ",zip_yr_result," ",format(object.size(file.path(path_in,zip_yr_result)), units = "Mb"))
      file = read_delim(file.path(path_in,zip_yr_result), 
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
                        ))
    }
    
    saveRDS(file, paste0(path_out,"test_result_",yr,".Rds"))
    
    message("Reading ",zip_yr_item," ",format(object.size(file.path(path_in,zip_yr_item)), units = "Mb"))
    
    file = read_delim(file.path(path_in,zip_yr_item), 
                      delim = "|",
                      escape_backslash = FALSE,
                      escape_double = FALSE,
                      col_types = readr::cols(
                        test_id = col_integer(),
                        rfr_id = col_integer(),
                        rfr_type_code = col_factor(),
                        location_id = col_integer(),
                        dangerous_mark = col_factor()
                      ))
    
    saveRDS(file, paste0(path_out,"test_item_",yr,".Rds"))
    
  } else {
    #Zipped
    
    message("Results ",yr)
    # Results
    dir.create(file.path(tempdir(),"mot"))
    zip::unzip(file.path(path_in,zip_yr_result),
          exdir = file.path(tempdir(),"mot"))
    
    files <- list.files(file.path(tempdir(),"mot"), pattern = ".csv", full.names = TRUE)
    if(length(files) == 0){
      files <- list.files(file.path(tempdir(),"mot"), pattern = ".csv", full.names = TRUE, recursive = TRUE)
    }
    
    if(yr == 2021){
      res <- list()
      for(j in seq(1, length(files))){
        sub <- read_csv(files[j], 
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
      file <- dplyr::bind_rows(res)
      unlink(file.path(tempdir(),"mot"), recursive = TRUE)
    } else {
      res <- list()
      for(j in seq(1, length(files))){
        sub <- readLines(files[j])
        if(j == 1){
          res[[j]] <- sub
        } else {
          res[[j]] <- sub[seq(2, length(sub))]
        }
        
      }
      unlink(file.path(tempdir(),"mot"), recursive = TRUE)
      data <- unlist(res)
      data <- strsplit(data,",", fixed = TRUE)
      file <- clean_results(data)
    }
    
    
    
    saveRDS(file, paste0(path_out,"test_result_",yr,".Rds"))
    
    # Item
    message("Items ",yr)
    dir.create(file.path(tempdir(),"mot"))
    zip::unzip(file.path(path_in,zip_yr_item),
               exdir = file.path(tempdir(),"mot"))
    
    files <- list.files(file.path(tempdir(),"mot"), pattern = ".csv", full.names = TRUE)
    if(length(files) == 0){
      files <- list.files(file.path(tempdir(),"mot"), pattern = ".csv", full.names = TRUE, recursive = TRUE)
    }
    
    res <- list()
    for(j in seq(1, length(files))){
      sub <- read_csv(files[j], 
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
    unlink(file.path(tempdir(),"mot"), recursive = TRUE)
  }
  
  
  
}


