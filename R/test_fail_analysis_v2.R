# Read in the data
library(data.table)

tests = readRDS("D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/test_result_2005_2021.Rds")
tests = tests[,c("year","test_id","vehicle_id","test_date","test_result","test_type","test_mileage","first_use_date")]
tests$vehicle_age <- floor(as.numeric(difftime(tests$test_date, tests$first_use_date, units = "days"))/365)
tests$first_use_date <- NULL

# Filter out ABR and ABRVE
tests = tests[!tests$test_result %in% c("ABA","ABR","ABRVE","R"),]

# Group and summarise
test_summary = tests[,
                     .(n_tests = .N,
                       n_pass = length(test_result[test_result == "P"]),
                       n_fail = length(test_result[test_result == "F"]),
                       n_pass_rec = length(test_result[test_result == "PRS"]),
                       vehicle_age = min(vehicle_age),
                       time_between_tests = as.numeric(max(test_date) - min(test_date))
                       ),
                     by = list(year, vehicle_id)]

saveRDS(test_summary,"data/vehicles_summarised_test_outcomes_by_year.Rds")


message("Hi")
type_summary = test_summary[,.(n_vehicles = .N),
                              by = list(n_tests,n_pass,n_fail,n_pass_rec)]
type_summary$percent <- round(type_summary$n_vehicles / sum(type_summary$n_vehicles) * 100,2)





# 
# path_in <- "D:/OneDrive - University of Leeds/Data/CARS/Anoymised MOT/clean/"
# files <- list.files(path_in, patter = "result", full.names = TRUE)
# res <- list()
# for(i in 1:length(files)){
#   message(i + 2004)
#   result <- readRDS(files[i])
#   result <- result[,c("test_id","vehicle_id","test_date","test_result","first_use_date")]
#   result$vehicle_age <- floor(as.numeric(difftime(result$test_date, result$first_use_date, units = "days"))/365)
#   result$test_year <- lubridate::year(result$test_date)
#   
#   
#   result_summary <- result %>%
#     group_by(vehicle_id) %>%
#     summarise(,
#               test_year = min(test_year))
#   
#   result_summary$total_nonpass <- rowSums(result_summary[,4:9])
#   
#   result_summary_age <- result_summary %>%
#     group_by(vehicle_age) %>%
#     summarise(n_tests = sum(n_tests),
#               n_pass = sum(n_pass),
#               n_fail = sum(n_fail),
#               n_pass_rec = sum(n_pass_rec),
#               n_refused = sum(n_refused),
#               n_abandoned = sum(n_abandoned),
#               n_aborted = sum(n_aborted),
#               n_aborted_ve = sum(n_aborted_ve),
#               total_nonpass = sum(total_nonpass),
#               vehicle_age = min(vehicle_age),
#               test_year = min(test_year))
#   
#   
#   
#   res[[i]] <- result_summary_age
# }
# res <- data.table::rbindlist(res)
# res <- as.data.frame(res)
# 
# saveRDS(res,"data/MOT_passrate.Rds")
# 
# library(ggplot2)
# 
# res$pass_rate <- (res$n_pass / res$n_tests) * 100
# res$test_year <- factor(res$test_year, levels = 2005:2021)
# 
# nb.cols <- 17
# mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
# 
# res_plot <- res[!is.na(res$vehicle_age),]
# res_plot <- res_plot[res_plot$vehicle_age > 0 & res_plot$vehicle_age < 31, ]
# 
# ggplot(res_plot, 
#        aes(x = vehicle_age, y = pass_rate, color = test_year, group = test_year)) +
#   geom_line(lwd = 1) +
#   scale_color_manual(values = mycolors) +
#   ylab("Pass rate %") +
#   xlab("Vehicle age when tested") +
#   guides(color=guide_legend(title="Year")) +
#   scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0,30), expand = c(0,0))
#   
# ggsave("plots/MOT_pass_rate_age.png")
# 
# nb.cols <- 17
# mycolors <- colorRampPalette(brewer.pal(8, "Sp"))(nb.cols)
# 
# 
# ggplot(res_plot, 
#        aes(x = vehicle_age, y = n_fail, color = test_year, group = test_year)) +
#   geom_line(lwd = 1) +
#   scale_color_manual(values = mycolors) +
#   ylab("Failed tests") +
#   xlab("Vehicle age when tested") +
#   guides(color=guide_legend(title="Year")) +
#   scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0,30), expand = c(0,0))
# 
# ggplot(res_plot, 
#        aes(x = vehicle_age, y = n_tests, color = test_year, group = test_year)) +
#   geom_line(lwd = 1) +
#   scale_color_manual(values = mycolors) +
#   ylab("All tests") +
#   xlab("Vehicle age when tested") +
#   guides(color=guide_legend(title="Year")) +
#   scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0,30), expand = c(0,0))
