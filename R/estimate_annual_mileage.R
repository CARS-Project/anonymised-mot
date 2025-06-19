library(data.table)
library(zoo)
library(tidyr)
library(dplyr)

path <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"
test_data_wide <- readRDS(paste0(path,"mileage_wide_2005_2023.Rds"))

# Check for year to year constancy

test_mileage <- test_data_wide[, c("vehicle_id",
                                   paste0("test_mileage_",2005:2023))]
test_postcode <- test_data_wide[, c("vehicle_id",
                                    paste0("postcode_area_",2005:2023))]
summary(duplicated(test_data_wide$vehicle_id))

car_data <- readRDS(paste0(path,"car_data_2005_2023.Rds"))
car_data <- car_data[,c("vehicle_id","first_use_date")]
car_data$zero_year <- lubridate::year(car_data$first_use_date)
car_data$first_use_date <- NULL

test_mileage <- dplyr::left_join(test_mileage, car_data, by = "vehicle_id")

#summary(test_mileage$zero_year)
test_mileage$zero_year[is.na(test_mileage$zero_year)] <- 2002 #Fallback assumption for 26,068 vehicles

# For old cars estimate a 2004 mileage.
# Assume linear interpolation from first used date to first test date
test_mileage$first_year <- names(test_mileage[2:20])[max.col(!is.na(test_mileage[2:20]), "first")]
test_mileage$last_year <- names(test_mileage[2:20])[max.col(!is.na(test_mileage[2:20]), "last")]
test_mileage <- as.data.table(test_mileage)
test_mileage <- test_mileage[, first_year_mileage := .SD[[.BY[[1]]]], by=first_year]
test_mileage <- test_mileage[, last_year_mileage := .SD[[.BY[[1]]]], by=last_year]

test_mileage$first_year <- as.integer(gsub("test_mileage_","",test_mileage$first_year))
test_mileage$last_year <- as.integer(gsub("test_mileage_","",test_mileage$last_year))

test_mileage$miles_per_year <- dplyr::if_else(test_mileage$zero_year <= 2004, 
                                              test_mileage$first_year_mileage / (test_mileage$first_year - test_mileage$zero_year),
                                              0)

test_mileage$test_mileage_2004 <- dplyr::if_else(test_mileage$zero_year <= 2004, 
                                                 as.integer(test_mileage$miles_per_year * (2004 - test_mileage$zero_year)),
                                                 NA_integer_)

# TODO: Handle rollovers of the odometer
test_mileage$max_mileage <- pmax(test_mileage$test_mileage_2005,
                                 test_mileage$test_mileage_2006, 
                                 test_mileage$test_mileage_2007,
                                 test_mileage$test_mileage_2008,
                                 test_mileage$test_mileage_2009,
                                 test_mileage$test_mileage_2010,
                                 test_mileage$test_mileage_2011,
                                 test_mileage$test_mileage_2012,
                                 test_mileage$test_mileage_2013,
                                 test_mileage$test_mileage_2014,
                                 test_mileage$test_mileage_2015,
                                 test_mileage$test_mileage_2016,
                                 test_mileage$test_mileage_2017,
                                 test_mileage$test_mileage_2018,
                                 test_mileage$test_mileage_2019,
                                 test_mileage$test_mileage_2020,
                                 test_mileage$test_mileage_2021,
                                 test_mileage$test_mileage_2022,
                                 test_mileage$test_mileage_2023,
                                      na.rm = TRUE)

test_mileage$rollover <- test_mileage$max_mileage != test_mileage$last_year_mileage
test_mileage$rollover[is.na(test_mileage$rollover)] <- FALSE

#test_mileage_rollover <- test_mileage[test_mileage$rollover,]



# foo = test_mileage[1:1000,]
# foo = foo[,c("vehicle_id","zero_year","first_year","first_year_mileage","miles_per_year",
#                         paste0("test_mileage_",2004:2021))]

# When car doesn't exist set mileage to 0
update_col <- function(orig, zy, yr){
  ifelse(zy == yr & is.na(orig), 0, orig)
}

test_mileage$test_mileage_2005 = update_col(test_mileage$test_mileage_2005, test_mileage$zero_year, 2005)
test_mileage$test_mileage_2006 = update_col(test_mileage$test_mileage_2006, test_mileage$zero_year, 2006)
test_mileage$test_mileage_2007 = update_col(test_mileage$test_mileage_2007, test_mileage$zero_year, 2007)
test_mileage$test_mileage_2008 = update_col(test_mileage$test_mileage_2008, test_mileage$zero_year, 2008)
test_mileage$test_mileage_2009 = update_col(test_mileage$test_mileage_2009, test_mileage$zero_year, 2009)
test_mileage$test_mileage_2010 = update_col(test_mileage$test_mileage_2010, test_mileage$zero_year, 2010)
test_mileage$test_mileage_2011 = update_col(test_mileage$test_mileage_2011, test_mileage$zero_year, 2011)
test_mileage$test_mileage_2012 = update_col(test_mileage$test_mileage_2012, test_mileage$zero_year, 2012)
test_mileage$test_mileage_2013 = update_col(test_mileage$test_mileage_2013, test_mileage$zero_year, 2013)
test_mileage$test_mileage_2014 = update_col(test_mileage$test_mileage_2014, test_mileage$zero_year, 2014)
test_mileage$test_mileage_2015 = update_col(test_mileage$test_mileage_2015, test_mileage$zero_year, 2015)
test_mileage$test_mileage_2016 = update_col(test_mileage$test_mileage_2016, test_mileage$zero_year, 2016)
test_mileage$test_mileage_2017 = update_col(test_mileage$test_mileage_2017, test_mileage$zero_year, 2017)
test_mileage$test_mileage_2018 = update_col(test_mileage$test_mileage_2018, test_mileage$zero_year, 2018)
test_mileage$test_mileage_2019 = update_col(test_mileage$test_mileage_2019, test_mileage$zero_year, 2019)
test_mileage$test_mileage_2020 = update_col(test_mileage$test_mileage_2020, test_mileage$zero_year, 2020)
test_mileage$test_mileage_2021 = update_col(test_mileage$test_mileage_2021, test_mileage$zero_year, 2021)
test_mileage$test_mileage_2022 = update_col(test_mileage$test_mileage_2022, test_mileage$zero_year, 2022)
test_mileage$test_mileage_2023 = update_col(test_mileage$test_mileage_2023, test_mileage$zero_year, 2023)


test_mileage <- test_mileage[,c("vehicle_id",paste0("test_mileage_",2004:2023))]
test_mileage <- pivot_longer(test_mileage, 
                             cols = test_mileage_2004:test_mileage_2023,
                             names_to = "year",
                             names_prefix = "test_mileage_",
                             values_to = "mileage",
                             values_drop_na = FALSE)
test_mileage$year <- as.integer(test_mileage$year)
test_mileage$mileage <- as.integer(test_mileage$mileage)


#clear up memory
saveRDS(test_postcode,paste0(path,"test_postcode_wide_2005_2023.Rds"))
rm(car_data, test_data_wide, test_postcode)
gc()

# Fill in the NAs with zoo::na.approx
test_mileage <- as.data.table(test_mileage)

test_mileage_aprox <- test_mileage[,approx := as.integer(zoo::na.approx(mileage, na.rm = FALSE)), 
                                   by = vehicle_id]

# foo <- test_mileage[1:1000,]
# bar = foo[foo$vehicle_id == 116,]
# bar = pivot_longer(bar, c("mileage","approx"))
# bar$name[bar$name == "approx"] <- "interpolated"
# bar$name[bar$name == "mileage"] <- "recorded"
# 
# library(ggplot2)
# ggplot(bar, aes(x = year, y = value, color = name)) +
#   geom_line() +
#   geom_point() +
#   ylab("Total Mileage") +
#   xlab("Year") +
#   ggtitle("Vehicle 116: APRILIA RS125 (motorbike)\nFirst Used 1995-12-31") +
#   ylim(0,50000)
# ggsave("plots/interpolation_example.png")

# test_mileage_aprox <- test_mileage %>%
#   group_by(vehicle_id) %>%
#   mutate(approx = as.integer(round(na.approx(mileage, na.rm = FALSE),0)))

test_mileage_aprox$mileage <- NULL

test_mileage_wide <- pivot_wider(test_mileage_aprox, 
                              names_from = "year", 
                              names_prefix = "mileage_", 
                              values_from = "approx")




rm(test_mileage_aprox, test_mileage)

test_mileage_wide$km_2006 = as.integer(round((test_mileage_wide$mileage_2006 - test_mileage_wide$mileage_2005) * 1.60934, 0))
test_mileage_wide$km_2007 = as.integer(round((test_mileage_wide$mileage_2007 - test_mileage_wide$mileage_2006) * 1.60934, 0))
test_mileage_wide$km_2008 = as.integer(round((test_mileage_wide$mileage_2008 - test_mileage_wide$mileage_2007) * 1.60934, 0))
test_mileage_wide$km_2009 = as.integer(round((test_mileage_wide$mileage_2009 - test_mileage_wide$mileage_2008) * 1.60934, 0))
test_mileage_wide$km_2010 = as.integer(round((test_mileage_wide$mileage_2010 - test_mileage_wide$mileage_2009) * 1.60934, 0))
test_mileage_wide$km_2011 = as.integer(round((test_mileage_wide$mileage_2011 - test_mileage_wide$mileage_2010) * 1.60934, 0))
test_mileage_wide$km_2012 = as.integer(round((test_mileage_wide$mileage_2012 - test_mileage_wide$mileage_2011) * 1.60934, 0))
test_mileage_wide$km_2013 = as.integer(round((test_mileage_wide$mileage_2013 - test_mileage_wide$mileage_2012) * 1.60934, 0))
test_mileage_wide$km_2014 = as.integer(round((test_mileage_wide$mileage_2014 - test_mileage_wide$mileage_2013) * 1.60934, 0))
test_mileage_wide$km_2015 = as.integer(round((test_mileage_wide$mileage_2015 - test_mileage_wide$mileage_2014) * 1.60934, 0))
test_mileage_wide$km_2016 = as.integer(round((test_mileage_wide$mileage_2016 - test_mileage_wide$mileage_2015) * 1.60934, 0))
test_mileage_wide$km_2017 = as.integer(round((test_mileage_wide$mileage_2017 - test_mileage_wide$mileage_2016) * 1.60934, 0))
test_mileage_wide$km_2018 = as.integer(round((test_mileage_wide$mileage_2018 - test_mileage_wide$mileage_2017) * 1.60934, 0))
test_mileage_wide$km_2019 = as.integer(round((test_mileage_wide$mileage_2019 - test_mileage_wide$mileage_2018) * 1.60934, 0))
test_mileage_wide$km_2020 = as.integer(round((test_mileage_wide$mileage_2020 - test_mileage_wide$mileage_2019) * 1.60934, 0))
test_mileage_wide$km_2021 = as.integer(round((test_mileage_wide$mileage_2021 - test_mileage_wide$mileage_2020) * 1.60934, 0))
test_mileage_wide$km_2022 = as.integer(round((test_mileage_wide$mileage_2022 - test_mileage_wide$mileage_2021) * 1.60934, 0))
test_mileage_wide$km_2023 = as.integer(round((test_mileage_wide$mileage_2023 - test_mileage_wide$mileage_2022) * 1.60934, 0))

saveRDS(test_mileage_wide,paste0(path,"test_mileage_wide_2005_2023_with_neg.Rds"))

# Some cars have -ve millage due to error so set to 0
summary(test_mileage_wide$km_2018)
foo <- test_mileage_wide[!is.na(test_mileage_wide$km_2018),]
foo <- foo[foo$km_2018 > 160000 |  foo$km_2018 < -160000, ]
# foo <- foo[!is.na(foo$km_2018),]

test_mileage_wide$km_2006[test_mileage_wide$km_2006 < 0] <- 0
test_mileage_wide$km_2007[test_mileage_wide$km_2007 < 0] <- 0
test_mileage_wide$km_2008[test_mileage_wide$km_2008 < 0] <- 0
test_mileage_wide$km_2009[test_mileage_wide$km_2009 < 0] <- 0
test_mileage_wide$km_2010[test_mileage_wide$km_2010 < 0] <- 0
test_mileage_wide$km_2011[test_mileage_wide$km_2011 < 0] <- 0
test_mileage_wide$km_2012[test_mileage_wide$km_2012 < 0] <- 0
test_mileage_wide$km_2013[test_mileage_wide$km_2013 < 0] <- 0
test_mileage_wide$km_2014[test_mileage_wide$km_2014 < 0] <- 0
test_mileage_wide$km_2015[test_mileage_wide$km_2015 < 0] <- 0
test_mileage_wide$km_2016[test_mileage_wide$km_2016 < 0] <- 0
test_mileage_wide$km_2017[test_mileage_wide$km_2017 < 0] <- 0
test_mileage_wide$km_2018[test_mileage_wide$km_2018 < 0] <- 0
test_mileage_wide$km_2019[test_mileage_wide$km_2019 < 0] <- 0
test_mileage_wide$km_2020[test_mileage_wide$km_2020 < 0] <- 0
test_mileage_wide$km_2021[test_mileage_wide$km_2021 < 0] <- 0
test_mileage_wide$km_2022[test_mileage_wide$km_2022 < 0] <- 0
test_mileage_wide$km_2023[test_mileage_wide$km_2023 < 0] <- 0

test_mileage_wide = test_mileage_wide[,c("vehicle_id",paste0("km_",2006:2023))]

test_mileage_wide[2:19] <- lapply(test_mileage_wide[2:19], as.integer)

saveRDS(test_mileage_wide,paste0(path,"annual_km_2006_2023.Rds"))
