library(plyr)
ma <- function(x, n = 5){
  ma <- stats::filter(x, rep(1 / n, n), sides = 2)
  ma[1:2] <- ma[3]
  n <- length(ma)
  ma[(n - 1):n] <- ma[n-2]
  return(c(ma))
}


used_energy <- function(records){
  avg_power <- records$power %>% mean

  time <- records$timestamp %>%
    range %>%
    {difftime(time1 = .[2], time2 = .[1], units = "hours")} %>%
    as.numeric
  time <- nrow(records)/3600
  time
  nrg[1, ]$carbs <- 0

  nrg$hr <- NULL
  idx <- which(nrg$watt == round(avg_power))[1]
  carbs <- nrg[idx, ]$carbs
  fat <- nrg[idx, ]$fat

  records$watt <- records$power

  records_ma <- records
  records_ma$watt <- round(ma(records$power))

  comb <- join(records, nrg, by = "watt")
  comb_ma <- join(records_ma, nrg, by = "watt")

  carbs_from_avg <- time*carbs
  carbs_from_avg
  carbs_from_single <- sum(comb_ma$carbs)/3600
  carbs_from_single
  carbs_from_ma <- sum(comb$carbs)/3600
  carbs_from_ma


  carbs_range <- c(min(floor(c(carbs_from_ma, carbs_from_ma))), max(ceiling(c(carbs_from_ma, carbs_from_ma))))


  fat_from_avg <- time*fat
  fat_from_avg
  fat_from_single <- sum(comb_ma$fat)/3600
  fat_from_single
  fat_from_ma <- sum(comb$fat)/3600
  fat_from_ma

  fat_range <- c(min(floor(c(fat_from_ma, fat_from_ma))), max(ceiling(c(fat_from_ma, fat_from_ma))))



  kcal <- carbs_from_ma*4 + fat_from_ma*7.2
  kcal


  list(
    list(
      carbs_from_ma = carbs_from_ma,
      carbs_from_avg = carbs_from_avg,
      carbs_from_single = carbs_from_single,
      carbs_range = carbs_range
    ),
    list(
      fat_from_ma = fat_from_ma,
      fat_from_avg = fat_from_avg,
      fat_from_single = fat_from_single,
      fat_range = fat_range
    ),
    kcal = kcal
  )
}

# file_name <- "data/03_04_2022.fit"
# #"data/",
# records <- load_strava(paste0(file_name))
# energy <- used_energy(records)
# energy
