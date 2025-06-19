

path <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"
test_km <- readRDS(paste0(path,"annual_km_2006_2023.Rds"))
test_pc <- readRDS(paste0(path,"test_postcode_wide_2005_2023.Rds"))

#identical(test_km$vehicle_id, test_pc$vehicle_id)

test_data_wide = cbind(test_km, test_pc[,2:ncol(test_pc)])
rm(test_km, test_pc)
library(tidyr)
library(dplyr)

names(test_data_wide) <- gsub("postcode_area","postcodearea", names(test_data_wide))
vehicle_km_long <- pivot_longer(test_data_wide,
                                cols = km_2006:postcodearea_2023,
                                names_to = c(".value", "year"),
                                names_pattern = "(.+)_(.+)",
                                values_drop_na = TRUE)
vehicle_km_long$year = as.integer(vehicle_km_long$year)

# Fill in Missing Postcodes
vehicle_km_lst = group_split(vehicle_km_long, vehicle_id, .keep = FALSE)

infill_postcode = function(x){
  if(nrow(x) > 1 & anyNA(x$postcodearea)){
    x = x[order(x$year),]
    for(i in seq(2,nrow(x))){
      if(is.na(x$postcodearea[i])){
        x$postcodearea[i] = x$postcodearea[i-1]
      }
    }
  }
  x
}
vehicle_km_lst = purrr::map(vehicle_km_lst, infill_postcode, .progress = TRUE)
vehicle_km_long = data.table::rbindlist(vehicle_km_lst)


rm(test_data_wide)

#saveRDS(vehicle_km_long, paste0(path,"vehicle_km_per_year_long.Rds"))

postcode_summary <- vehicle_km_long %>%
  group_by(postcodearea, year) %>%
  summarise(total_km = sum(km, na.rm = TRUE),
            total_vehicle = n(),
            km_per_vehicle = total_km / total_vehicle
            )

library(ggplot2)
library(ggrepel)

postcode_summary %>%
  filter(year > 2005) %>%
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

postcode_summary %>%
  filter(year > 2005) %>%
  filter(!is.na(postcodearea)) %>%
  mutate(label = if_else(year == max(year), as.character(postcodearea), NA_character_)) %>%
  ggplot(aes(x = year, y = km_per_vehicle, group = postcodearea)) +
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


postcodes <- readRDS("../../creds2/CarbonCalculator/data/bounds/postcode_areas.Rds")
postcodes <- sf::st_as_sf(postcodes)

postcodes <- left_join(postcodes, postcode_summary, by = c("PC_AREA" = "postcodearea"))

library(tmap)

tm_shape(postcodes) +
  tm_fill("2018", 
          n = 10, 
          style = "quantile") +
  tm_borders()

saveRDS(postcodes, paste0(path,"postcode_total_vkm_2005_2023.Rds"))

postcodes$diff = postcodes$`2023` - postcodes$`2018`
tm_shape(postcodes) +
  tm_fill("diff", 
          n = 10, 
          style = "quantile") +
  tm_borders()

