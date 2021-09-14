library(magrittr)
library(leaflet)
library(leaflet.extras)
library(dygraphs)

energy <- data.frame(
  watt = c(0, 100, 130, 160, 190, 220, 250, 280, 310),
  hr = c(40, 111, 121, 129, 139, 150, 162, 175, 185),
  kcal = c(76, 326, 423, 521, 619, 717, 814, 912, 1010),
  carbs = c(5, 80, 98, 120, 136, 177, 218, 253, 277),
  fat = c(15, 26, 26, 26, 26, 10, 5, 0, 0)
)


# y1 = a*x1 + b
# y2 = a*x2 + b
# y1 = ax1 + y2 - ax2
# (y1 - y2)/(x1 - x2)= a
# b = y1 - a*x1


# 0 -
# carb = 5 + 0.7*carb/watt*watt

energy$carbs[1:2]

start_idx <- 1
end_idx <- 5
calc_energy <- function(energy, start_idx){
  end_idx <- start_idx + 1
  a = (energy$carbs[start_idx] - energy$carbs[end_idx])/(energy$watt[start_idx] - energy$watt[end_idx])
  b = energy$carbs[start_idx] - a* energy$watt[start_idx]
  carbs = a*energy$watt[start_idx]:energy$watt[end_idx] + b

  a = (energy$hr[start_idx] - energy$hr[end_idx])/(energy$watt[start_idx] - energy$watt[end_idx])
  b = energy$hr[start_idx] - a* energy$watt[start_idx]
  hr = a*energy$watt[start_idx]:energy$watt[end_idx] + b

  data.frame(
    carbs = carbs,
    hr = hr,
    watt = energy$watt[start_idx]:energy$watt[end_idx]
  )
}

n <- nrow(energy) - 1
energies <- lapply(1:n, calc_energy, energy = energy)
nrg <- do.call(rbind, energies)
plot(nrg$carbs)
#
# calc_energy(energy[1:2, ])
# calc_energy(energy[2:3, ])
# calc_energy(energy[3:4, ])
# calc_energy(energy[4:5, ])
# calc_energy(energy[5:6, ])
# calc_energy(energy[6:7, ])
# calc_energy(energy[7:8, ])
# calc_energy(energy[8:9, ])
#
# energy$kcal_calc <- energy$fat*7 + energy$carbs*4
#
# energy$kcal_calc
# energy$kcal
# plot(energy$kcal_calc, energy$kcal)
#
# hr <- 120
# get_energy_with_hr <- function(track_raw){
#   idx <- sapply(track_raw$heart_rate, function(hr){
#     if(is.na(hr)) return(NA)
#     max(which(energy$hr < hr))
#   })
#   energies <- energy[idx, c("kcal", "carbs", "fat")] * track_raw$duration / 3600
# }
#
#
# energies$kcal %>% sum(na.rm = TRUE)
# energies$carbs %>% sum(na.rm = TRUE)
# energies$fat %>% sum(na.rm = TRUE)
#
#
#
# track_raw$time %>% range
#
# # Source: https://youtu.be/U59JjmX__DQ?t=374
# # HF: 50-72%
# # FTP: 55-75%
# # Carbs: ~ ~ 30-60g / Stunde
# hr_max <- 190
# hr_lit <- c(124, 146, 162)
#
# ylim <- range(track_raw$heart_rate, na.rm = TRUE) + c(-10, 10)
# # to do: second x-axis kilometer? + make it subsettable interactively
# track_raw$heart_rate %>% plot(x = track_raw$time, ylim = ylim, type = "l", main = "Heart rate over time")
# abline(h = hr_lit[1], col="blue")
# abline(h = hr_lit[2], col="blue")
# abline(h = hr_lit[3], col="blue")
#
# plot(x = track_raw$time, y = track_raw$elevation, type = "l", xlab = "Elevation in meters")
#
# ftp <- 280
# ftp_lit <- c(0.55*ftp, 0.75*ftp)
#
#
# lon <- track_raw$lon %>% range
# lat <- track_raw$lat %>% range
# m <- leaflet() %>% addTiles()
# m %>% fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
#   addPolylines(data = track_raw, lng = ~lon, lat = ~lat)
#
#
#
# library(xts)
# library(forecast) # for ma
#
#
# track_raw$speed %<>% as.numeric
# df <- data.frame(x = track_raw$time, y = track_raw$speed)
# qxts <- xts(track_raw[ ,c(3, 5, 6)], order.by = track_raw$time)
#
# dygraph(qxts,  main = "Speed over time")
#
#
# track_raw$heart_rate %>% which.max()
#
#
# plot(track_raw$heart_rate[7000:8600], type = "l")
#
# sm <- 25
# xx <- ma(track_raw$heart_rate[7000:8600], sm)
# plot(xx, type = "l")
#
# start <- ceiling(sm/2)
# nr <- start
# end <- length(xx) - start + 1
#
# intervall_end <- rep(FALSE, end - start + 1)
# for(nr in start:end){
#   intervall_end[nr] <- xx[nr] == max(xx[nr:(60 + nr)])
# }
# intervall_end
#
#
# plot(track_raw$heart_rate[7250:8600], type = "l")
#
#
#
# interval_end <- 7254
# interval_min_rest <- 7254 + 60
# track_raw[interval_end, ]$heart_rate
# track_raw[interval_min_rest, ]$heart_rate
#
#
# track_raw$heart_rate[7200:8600]
#
#
# track_raw[7000:8600, 1:2]$lat
#
# library(leaflet)
# m <- leaflet() %>% addTiles()
# m %>% fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
#   addPolylines(data = track_raw[7000:8600, ], lng = ~lon, lat = ~lat)
