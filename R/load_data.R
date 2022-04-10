dont_run <- function(plotKML, magrittr, leaflet, rbind, ceiling) {
  library(plotKML)
  library(magrittr)
  library(leaflet)

  file_name <- "Bernau_Deuting_1.gpx"
  xx <- readGPX("route_1.gpx", metadata = TRUE, bounds = TRUE,
                waypoints = TRUE, tracks = TRUE, routes = TRUE)
  xx
  track_raw <- xx$tracks[[1]]$`2021-06-01 18:19`

  file_name <- "Edelweissspitze (33).gpx"
  xx <- readGPX(file_name, metadata = TRUE, bounds = TRUE,
                waypoints = TRUE, tracks = TRUE, routes = TRUE)
  xx$tracks
  track_raw <- do.call(rbind, xx$tracks[[1]])



  ftp  <- 280 #watt


  clean_heart_rate <- function(track_raw){
    track_raw$heart_rate <- track_raw$extensions %>% as.numeric()
    track_raw$extensions <- NULL

    # false data: could exclude them but then there might be missing data
    errors <- track_raw$heart_rate < 50 | track_raw$heart_rate > 220
    track_raw <- track_raw[!errors, ]
  }

  track_raw <- clean_heart_rate(track_raw)



  heart_rate <- track_raw$heart_rate

  heart_rate


  watt <- 0:600
  hr <- seq(60, 200, length.out = 601) %>% ceiling
  carbs <- watt^2/600^2*100
  fat <- 100 - carbs


  # relation of heart rate - watt can be approximated in fitness studio



  used_hr <- 120
  used_fat <- function(track_raw, watt, hr, carbs, fat){
    matches <- sapply(track_raw$heart_rate, function(hrate) which(hrate == hr)[1])
    track_raw$watt_est <- watt[matches]

    # 1 Watt = 0.8598 Kilokalorien pro Stunde (3600s)
    # source: https://convertlive.com/de/u/konvertieren/watt/zu/kilokalorien-pro-stunde
    # track_raw$duration unit is "seconds"
    track_raw$kcal <- track_raw$watt_est*0.8598*track_raw$duration/3600

    #1kcal = 1/8g fat = 1/4g carbs
    track_raw$carbs <- carbs[matches]/100/4*track_raw$kcal
    track_raw$fat <- fat[matches]/100/8*track_raw$kcal
  }

  sum(track_raw$fat)
  sum(track_raw$carbs)


  plot(watt, fat, type = "l")
  lines(watt, carbs, type = "l")


  # UI
  # Current heart rate, Watt
  # burned carbs

  # Summary: Burned kcal - burned kcal of fat
}
