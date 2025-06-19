library(dplyr)

path = "D:/OneDrive - University of Leeds/Data/"

year <- 2021

dir.create("tmp")
unzip(paste0(path,"MOT anoymised/dft_test_item_",year,".zip"),
      exdir = "tmp")

files <- list.files("tmp", pattern = ".csv", full.names = TRUE, recursive = TRUE)

item <- list()
for(j in seq(1, length(files))){
  item[[j]] <- readr::read_csv(files[j])
}
unlink("tmp", recursive = TRUE)

item <- dplyr::bind_rows(item)



dir.create("tmp")
unzip(paste0(path,"MOT anoymised/dft_test_result_",year,".zip"),
      exdir = "tmp")

files <- list.files("tmp", pattern = ".csv", full.names = TRUE, recursive = TRUE)

result <- list()
for(j in seq(1, length(files))){
  result[[j]] <- readr::read_csv(files[j])
}
unlink("tmp", recursive = TRUE)

result <- dplyr::bind_rows(result)


# SUmmarise tests
result$vehicle_age <- round(as.numeric(lubridate::ymd("2022-01-01") - result$first_use_date) / 365)
summary(result$vehicle_age)
foo = result[result$vehicle_age > 100,]

result_summary <- result %>%
  group_by(vehicle_id) %>%
  summarise(n_tests = n(),
            n_pass = length(test_result[test_result == "P"]),
            n_fail = length(test_result[test_result == "F"]),
            n_pass_rec = length(test_result[test_result == "PRS"]),
            n_refused = length(test_result[test_result == "R"]),
            n_abandoned = length(test_result[test_result == "ABA"]),
            n_aborted = length(test_result[test_result == "ABR"]),
            n_aborted_ve = length(test_result[test_result == "ABRVE"]),
            vehicle_age = vehicle_age[1])

result_summary$total_nonpass <- rowSums(result_summary[,4:9])
summary(result_summary$total_nonpass > 0)

result_summary_age <- result_summary %>%
  group_by(vehicle_age) %>%
  summarise(n_tests = sum(n_tests),
            n_pass = sum(n_pass),
            n_fail = sum(n_fail),
            n_pass_rec = sum(n_pass_rec),
            n_refused = sum(n_refused),
            n_abandoned = sum(n_abandoned),
            n_aborted = sum(n_aborted),
            n_aborted_ve = sum(n_aborted_ve),
            total_nonpass = sum(total_nonpass),
            vehicle_age = vehicle_age[1])



res_nopass = result_summary[rowSums(result_summary[,4:9]) > 0,]
