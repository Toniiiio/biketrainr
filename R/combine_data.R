# library(magrittr)
# getwd()
# #"Hoherodskopf (1).RData" #
# file_names <- list.files(pattern = "Feldbergking.*.RData")
# file_names
# to_combine <- list()
# start_time <- rep(Sys.time(), length(file_names))
#
# nr <- 1
# for(nr in seq(file_names)){
#   load(file = file_names[nr])
#   to_combine[[nr]] <- track_raw
#   start_time[nr] <- to_combine[[nr]]$time %>% min
# }
#
# track_raw <- to_combine[order(start_time)] %>% do.call(rbind, .)
# track_raw$time[1] > Sys.Date()
#
# track_raw$time %<>%  as.POSIXct(tz = "CET")
# track_raw$heart_rate %<>% as.numeric()
#
# track_raw %>% head
#
#
