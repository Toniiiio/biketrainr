library(xts)
library(dygraphs)
library(ggplot2)
library(reshape2)

source("R/gen_energy_data.R")
source("R/load_from_strava.R")

# file_name <- "C:/Users/User11/Downloads/Morga.fit"
# file_name <- "C:/Users/User11/Downloads" %>%
#   {file.path(., list.files(.))} %>%
#   file.info() %>%
#   {rownames(.[which.max(.$atime), ])}


intervall_file_names <- "data" %>%
  {file.path(., list.files(.))} %>%
  .[grepl(., pattern = "4_4|Intervall|Wahoo")]
  # .[grepl(., pattern = ".fit")]

intervall_file_names

baseline = c(79, 111, 121, 129, 139, 150, 154)
watts <- c(0, 100, 130, 160, 190, 220, 230)

hrs_list <- list()
file_name <- intervall_file_names[1]
nr <- 3


# hit -> watt above ftp
hit_sec <- rep(NA, length(intervall_file_names))
total_duration_sec <- rep(NA, length(intervall_file_names))
ride_duration_sec <- rep(NA, length(intervall_file_names))

for(nr in seq(intervall_file_names)){

  file_name <- intervall_file_names[nr]
  records <- load_strava(file_name)

  # plot(records$power)
  idx <- 7
  hrs <- sapply(seq(watts), function(idx){
    cand <- which(abs(records$power - watts[idx]) < 5)
    cc <- cand[cand < idx*3*60]

    start <- ceiling(length(cc)/2)
    idxs <- cc[start:length(cc)]
    if(length(idxs) < 30) return(NA)
    mean(records$heart_rate[idxs], na.rm = TRUE)
  })

  date <- records$timestamp[1] %>% as.Date()
  print(date)
  hrs_list[[nr]] <- hrs
  names(hrs_list)[nr] <- date

  hit_sec[nr] <- which(records$power > 250) %>% length
  names(hit_sec)[nr] <- date


  has_power <- !is.null(records$power) | !is.null(records$cadence)
  if(has_power){
    keep <- records$cadence != 0 | records$power != 0
  }else{
    keep <- records$speed > 0
  }
  ride_duration_sec[nr] <- dim(records[keep, ])[1]
  names(ride_duration_sec)[nr] <- date

  total_duration_sec[nr] <- dim(records)[1]
  names(total_duration_sec)[nr] <- date

}

dates <- names(hrs_list) %>% as.numeric() %>%  as.Date()
ord <- dates %>% order

hr <- hrs_list[ord] %>% do.call(rbind, .)
colnames(hr) <- c(0, 100, 130, 160, 190, 220, 230)
hr <- data.frame(hr)
hr$time <- dates

qxts <- xts(hr[, 1:7], order.by = hr$time)
exclude <- apply(qxts, 1, is.na) %>% apply(2, all) %>% which
qxts <- qxts[-exclude, ]
qxts$tss <- c(100)

dygraph(qxts) %>%
  dyEvent("2021-09-07", "Ende 2,5Wochen Trainingslager", labelLoc = "bottom") %>%
  dyEvent("2021-09-18", "Frankfurt-Eschborn Rennen", labelLoc = "bottom") %>%
  dySeries("tss", fillGraph = TRUE, color = "grey")

qxts
# Next workout: 4*310 Watt
# Next workout: 3*(10 oder 13)*30/15 330/200 Watt


