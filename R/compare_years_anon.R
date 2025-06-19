library(dplyr)
library(data.table)
path <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"

mot <- readRDS(paste0(path,"test_result_2005_2023.Rds"))
car_data <- mot[,c("vehicle_id","make","model", "colour", "fuel_type","cylinder_capacity","first_use_date")]

#foo <- car_data[car_data$vehicle_id == 1263031090,]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mostfreqval <- function(x){
   head(names(sort(Rfast::Table(x), decreasing=TRUE)),1)
}

mostfreqvalfac <- function(x){
  if(inherits(x, "factor")){
    levels(x)[as.numeric(names(head(sort(Rfast::Table(x), decreasing=TRUE),1)))]
  } else {
    head(names(sort(Rfast::Table(x), decreasing=TRUE)),1)
  }
  
}

mostfreqvalfac2 <- function(x){
  levels(x)[as.numeric(names(head(sort(Rfast::Table(x), decreasing=TRUE),1)))]
}

mostfreqvalfac3 <- function(x){
  levels(x)[which.max(Rfast::Table(x))]
}

x = mot$fuel_type[1:10000]
y = as.character(x)
bench::mark(v1 = mostfreqval(y), 
            v2 = mostfreqvalfac(x),
            v3 = getmode(y),
            v4 = mostfreqvalfac3(x)
            )


# Mostly a single characteristics has changes e.g colour or different spelling of model. A few have change fuel type
# Make a simple table using the most common value.
# Takes days to run
# car_data_summary <- car_data %>%
#   group_by(vehicle_id) %>%
#   summarise(make = getmode(make),
#             model = getmode(model),
#             colour = getmode(colour),
#             fuel_type = getmode(fuel_type),
#             cylinder_capacity = getmode(cylinder_capacity),
#             first_use_date = getmode(first_use_date))

car_data$make = as.character(car_data$make)
car_data$model = as.character(car_data$model)
car_data$colour = as.character(car_data$colour)
car_data$fuel_type = as.character(car_data$fuel_type)

# retry with new function and data.table
car_data_summary <- car_data[, .(make = mostfreqval(make),
                                 model = mostfreqval(model),
                                 colour = mostfreqval(colour),
                                 fuel_type = mostfreqval(fuel_type),
                                 cylinder_capacity = mostfreqval(cylinder_capacity),
                                 first_use_date = mostfreqval(first_use_date)),
                              by = vehicle_id]

car_data_summary$first_use_date = as.Date(as.numeric(car_data_summary$first_use_date), origin = "1970-01-01")

saveRDS(car_data_summary,paste0(path,"car_data_2005_2023.Rds"))

test_data <- mot[,c("year","vehicle_id","test_id","test_date","test_class_id","test_type","test_result",
                    "test_mileage","postcode_area")]

saveRDS(test_data,paste0(path,"test_data_2005_2023.Rds"))


#rm(car_data, foo, bar, car_data_dup)
#test_data <- readRDS(paste0(path,"test_result_2005_2023.Rds"))
#nrow(test_data) / 1e6
summary(duplicated(test_data$test_id))

#foo = test_data$test_id[duplicated(test_data$test_id)]
#foo = test_data[test_data$test_id %in% foo,]

test_data <- as.data.table(test_data)
test_data <- test_data[order(test_data$vehicle_id),]
test_data <- test_data[,c("vehicle_id","test_date","test_mileage","postcode_area","year")]
#saveRDS(test_data,paste0(path,"test_data_2005_2023.Rds"))

test_data_wide <- tidyr::pivot_wider(test_data, id_cols = "vehicle_id",
                                     names_from = "year", values_from = c("test_mileage","postcode_area"))

saveRDS(test_data_wide,paste0(path,"test_data_wide_2005_2023.Rds"))

#test_data_wide <- readRDS(paste0(path,"test_data_wide_2005_2020.Rds"))
test_data_wide <- test_data_wide[,c("vehicle_id", paste0("test_mileage_",2005:2023), paste0("postcode_area_",2005:2023))]

# Check for year to year constancy

test_mileage <- test_data_wide[, c(paste0("test_mileage_",2005:2023),"vehicle_id")]
test_postcode <- test_data_wide[, c(paste0("postcode_area_",2005:2023),"vehicle_id")]
#summary(duplicated(test_data_wide$vehicle_id))

rm(test_data, test_data_wide)
test_mileage$first_year <- names(test_mileage[1:18])[max.col(!is.na(test_mileage[1:18]), "first")]
test_mileage$first_year <- gsub("test_mileage_","",test_mileage$first_year)
test_mileage$first_year <- as.numeric(test_mileage$first_year)

test_mileage$last_year <- names(test_mileage[1:18])[max.col(!is.na(test_mileage[1:18]), "last")]
test_mileage$last_year <- gsub("test_mileage_","",test_mileage$last_year)
test_mileage$last_year <- as.numeric(test_mileage$last_year)

test_mileage$total_years <- rowSums(!is.na(test_mileage[1:18]))
test_mileage$year_range <- test_mileage$last_year - test_mileage$first_year + 1
test_mileage$missing_years <- test_mileage$year_range - test_mileage$total_years
test_mileage$zero_year <- test_mileage$first_year - 3
#summary(test_mileage$missing_years)
#hist(test_mileage$missing_years)


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
test_mileage$test_mileage_2020 = update_col(test_mileage$test_mileage_2021, test_mileage$zero_year, 2021)
test_mileage$test_mileage_2020 = update_col(test_mileage$test_mileage_2022, test_mileage$zero_year, 2022)

stop()

test_mileage <- test_mileage[,c("vehicle_id",paste0("test_mileage_",2005:2020))]
test_mileage <- pivot_longer(test_mileage, 
                         cols = test_mileage_2005:test_mileage_2020,
                         names_to = "year",
                         names_prefix = "test_mileage_",
                         values_to = "mileage",
                         values_drop_na = FALSE)
test_mileage$year <- as.integer(test_mileage$year)

#clear up memory
test_mileage$mileage <- as.integer(test_mileage$mileage)
gc()

test_mileage_aprox <- test_mileage %>%
  group_by(vehicle_id) %>%
  mutate(approx = as.integer(round(na.approx(mileage, na.rm = FALSE),0)))

test_mileage_aprox$mileage <- NULL

test_data_wide <- pivot_wider(test_mileage_aprox, 
                            names_from = "year", 
                            names_prefix = "test_mileage_", 
                            values_from = "approx")

rm(test_mileage_aprox, test_mileage)

test_data_wide$km_2006 = as.integer(round((test_data_wide$test_mileage_2006 - test_data_wide$test_mileage_2005) * 1.60934, 0))
test_data_wide$km_2007 = as.integer(round((test_data_wide$test_mileage_2007 - test_data_wide$test_mileage_2006) * 1.60934, 0))
test_data_wide$km_2008 = as.integer(round((test_data_wide$test_mileage_2008 - test_data_wide$test_mileage_2007) * 1.60934, 0))
test_data_wide$km_2009 = as.integer(round((test_data_wide$test_mileage_2009 - test_data_wide$test_mileage_2008) * 1.60934, 0))
test_data_wide$km_2010 = as.integer(round((test_data_wide$test_mileage_2010 - test_data_wide$test_mileage_2009) * 1.60934, 0))
test_data_wide$km_2011 = as.integer(round((test_data_wide$test_mileage_2011 - test_data_wide$test_mileage_2010) * 1.60934, 0))
test_data_wide$km_2012 = as.integer(round((test_data_wide$test_mileage_2012 - test_data_wide$test_mileage_2011) * 1.60934, 0))
test_data_wide$km_2013 = as.integer(round((test_data_wide$test_mileage_2013 - test_data_wide$test_mileage_2012) * 1.60934, 0))
test_data_wide$km_2014 = as.integer(round((test_data_wide$test_mileage_2014 - test_data_wide$test_mileage_2013) * 1.60934, 0))
test_data_wide$km_2015 = as.integer(round((test_data_wide$test_mileage_2015 - test_data_wide$test_mileage_2014) * 1.60934, 0))
test_data_wide$km_2016 = as.integer(round((test_data_wide$test_mileage_2016 - test_data_wide$test_mileage_2015) * 1.60934, 0))
test_data_wide$km_2017 = as.integer(round((test_data_wide$test_mileage_2017 - test_data_wide$test_mileage_2016) * 1.60934, 0))
test_data_wide$km_2018 = as.integer(round((test_data_wide$test_mileage_2018 - test_data_wide$test_mileage_2017) * 1.60934, 0))
test_data_wide$km_2019 = as.integer(round((test_data_wide$test_mileage_2019 - test_data_wide$test_mileage_2018) * 1.60934, 0))
test_data_wide$km_2020 = as.integer(round((test_data_wide$test_mileage_2020 - test_data_wide$test_mileage_2019) * 1.60934, 0))


saveRDS(test_data_wide,paste0(path,"test_data_wide_2005_2020_with_neg.Rds"), compress = FALSE)

# Some cars have -ve millage due to error so set to 0
summary(test_data_wide$km_2018)
foo <- test_data_wide[test_data_wide$km_2018 > 160000 |  test_data_wide$km_2018 < -160000, ]
foo <- foo[!is.na(foo$km_2018),]

test_data_wide$km_2006[test_data_wide$km_2006 < 0] <- 0
test_data_wide$km_2007[test_data_wide$km_2007 < 0] <- 0
test_data_wide$km_2008[test_data_wide$km_2008 < 0] <- 0
test_data_wide$km_2009[test_data_wide$km_2009 < 0] <- 0
test_data_wide$km_2010[test_data_wide$km_2010 < 0] <- 0
test_data_wide$km_2011[test_data_wide$km_2011 < 0] <- 0
test_data_wide$km_2012[test_data_wide$km_2012 < 0] <- 0
test_data_wide$km_2013[test_data_wide$km_2013 < 0] <- 0
test_data_wide$km_2014[test_data_wide$km_2014 < 0] <- 0
test_data_wide$km_2015[test_data_wide$km_2015 < 0] <- 0
test_data_wide$km_2016[test_data_wide$km_2016 < 0] <- 0
test_data_wide$km_2017[test_data_wide$km_2017 < 0] <- 0
test_data_wide$km_2018[test_data_wide$km_2018 < 0] <- 0
test_data_wide$km_2019[test_data_wide$km_2019 < 0] <- 0
test_data_wide$km_2020[test_data_wide$km_2020 < 0] <- 0


test_data_wide <- left_join(test_data_wide, test_postcode, by = "vehicle_id")

test_data_wide <- test_data_wide[,c("vehicle_id",
                                   "km_2006",
                                   "km_2007",
                                   "km_2008",
                                   "km_2009",
                                   "km_2010",
                                   "km_2011",
                                   "km_2012",
                                   "km_2013",
                                   "km_2014",
                                   "km_2015",
                                   "km_2016",
                                   "km_2017",
                                   "km_2018",
                                   "km_2019",
                                   "km_2020",
                                   "postcode_area_2006",
                                   "postcode_area_2007",
                                   "postcode_area_2008",
                                   "postcode_area_2009",
                                   "postcode_area_2010",
                                   "postcode_area_2011",
                                   "postcode_area_2012",
                                   "postcode_area_2013",
                                   "postcode_area_2014",
                                   "postcode_area_2015",
                                   "postcode_area_2016",
                                   "postcode_area_2017",
                                   "postcode_area_2018",
                                   "postcode_area_2019",
                                   "postcode_area_2020"
                                   )]

saveRDS(test_data_wide, paste0(path,"vehicle_km_per_year.Rds"))

library(tidyr)

names(test_data_wide) <- gsub("postcode_area","postcodearea", names(test_data_wide))
vehicle_km_long <- pivot_longer(test_data_wide,
                                cols = km_2006:postcodearea_2020,
                                names_to = c(".value", "year"),
                                names_pattern = "(.+)_(.+)",
                                values_drop_na = TRUE)

saveRDS(vehicle_km_long, paste0(path,"vehicle_km_per_year_long.Rds"))

postcode_summary <- vehicle_km_long %>%
  group_by(postcodearea, year) %>%
  summarise(total_km = sum(km, na.rm = TRUE))

library(ggplot2)
library(ggrepel)

postcode_summary %>%
  filter(year > 2006) %>%
  filter(!is.na(postcodearea)) %>%
  mutate(label = if_else(year == max(year), as.character(postcodearea), NA_character_)) %>%
  ggplot(aes(x = year, y = total_km, group = postcodearea)) +
  geom_line(aes(colour = postcodearea)) +
  theme(legend.position = "none")  +
  scale_x_discrete(expand=c(0, 1)) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   max.overlaps = 10)

postcode_summary <- pivot_wider(postcode_summary, 
                                id_cols = "postcodearea", 
                                names_from = "year",
                                values_from = "total_km")


write.csv(postcode_summary, "../../CarbonCalculator/data/postocde_driving_summary.csv")
library(sf)

postcodes <- readRDS("../CarbonCalculator/data/bounds/postcode_areas.Rds")


postcodes <- left_join(postcodes, postcode_summary, by = c("PC_AREA" = "postcodearea"))

library(tmap)

tm_shape(postcodes) +
  tm_fill("2018", 
          n = 10, 
          style = "quantile") +
  tm_borders()



km_summary <- list()

for(i in 1:6){
  
  to <- i * 1e7
  from <- to - 1e7 + 1
  if(to > nrow(test_data_wide)){
    to <- nrow(test_data_wide)
  }
  message(Sys.time()," doing ",i," from ",from," to ",to)
  x <- test_data_wide[seq(from,to),]
  plan(multisession)
  km_summary[[i]] <- future_pmap_dfr(.l = x[,c("vehicle_id",
                                                      "test_mileage_2005",
                                                      "test_mileage_2006",
                                                      "test_mileage_2007",
                                                      "test_mileage_2008",
                                                      "test_mileage_2009",
                                                      "test_mileage_2010",
                                                      "test_mileage_2011",
                                                      "test_mileage_2012",
                                                      "test_mileage_2013",
                                                      "test_mileage_2014",
                                                      "test_mileage_2015",
                                                      "test_mileage_2016",
                                                      "test_mileage_2017",
                                                      "test_mileage_2018")],
                                            .f = summarise_history)
  plan(sequential)
  rm(x)
}

km_summary <- bind_rows(km_summary)

saveRDS(km_summary, paste0(path,"vehicle_km_per_year.Rds"))


# classif <- read.csv("D:/OneDrive - University of Leeds/Data/CREDS Data/car_classification.csv")
# classif <- classif[,c("GenModel","Classsification")]
# 
# car_data$make_model <- paste0(car_data$make," ",car_data$model)
# summary(car_data$make_model %in% classif$GenModel)


# car_data <- readRDS(paste0(path,"car_data_2005_2018.Rds"))
# car_data <- car_data[,c("vehicle_id","first_use_date")]

# test_data_wide <- left_join(test_data_wide, car_data, by = c("vehicle_id"))
# x = test_data_wide[1:1000000,]
# 
# summarise_history <- function(vehicle_id,
#                               test_mileage_2005,
#                               test_mileage_2006,
#                               test_mileage_2007,
#                               test_mileage_2008,
#                               test_mileage_2009,
#                               test_mileage_2010,
#                               test_mileage_2011,
#                               test_mileage_2012,
#                               test_mileage_2013,
#                               test_mileage_2014,
#                               test_mileage_2015,
#                               test_mileage_2016,
#                               test_mileage_2017,
#                               test_mileage_2018){
#   
#   res <- data.frame(vehicle_id,
#                     km_2006 = as.integer(round((test_mileage_2006 - test_mileage_2005) * 1.60934, 0)),
#                     km_2007 = as.integer(round((test_mileage_2007 - test_mileage_2006) * 1.60934, 0)),
#                     km_2008 = as.integer(round((test_mileage_2008 - test_mileage_2007) * 1.60934, 0)),
#                     km_2009 = as.integer(round((test_mileage_2009 - test_mileage_2008) * 1.60934, 0)),
#                     km_2010 = as.integer(round((test_mileage_2010 - test_mileage_2009) * 1.60934, 0)),
#                     km_2011 = as.integer(round((test_mileage_2011 - test_mileage_2010) * 1.60934, 0)),
#                     km_2012 = as.integer(round((test_mileage_2012 - test_mileage_2011) * 1.60934, 0)),
#                     km_2013 = as.integer(round((test_mileage_2013 - test_mileage_2012) * 1.60934, 0)),
#                     km_2014 = as.integer(round((test_mileage_2014 - test_mileage_2013) * 1.60934, 0)),
#                     km_2015 = as.integer(round((test_mileage_2015 - test_mileage_2014) * 1.60934, 0)),
#                     km_2016 = as.integer(round((test_mileage_2016 - test_mileage_2015) * 1.60934, 0)),
#                     km_2017 = as.integer(round((test_mileage_2017 - test_mileage_2016) * 1.60934, 0)),
#                     km_2018 = as.integer(round((test_mileage_2018 - test_mileage_2017) * 1.60934, 0)))
#   
#   
#   return(res)
#   
# }

# pb <- progress_estimated(length(file_list))
# system.time(res <- pmap_dfr(.l = x[,c("vehicle_id",
#                    "test_mileage_2005",
#                "test_mileage_2006",
#                "test_mileage_2007",
#                "test_mileage_2008",
#                "test_mileage_2009",
#                "test_mileage_2010",
#                "test_mileage_2011",
#                "test_mileage_2012",
#                "test_mileage_2013",
#                "test_mileage_2014",
#                "test_mileage_2015",
#                "test_mileage_2016",
#                "test_mileage_2017",
#                "test_mileage_2018")],
#       .f = summarise_history))
