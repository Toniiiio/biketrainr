#remotes::install_github("grimbough/FITfileR")
library(FITfileR)
library(magrittr)
library(tidyverse)

# file_name <- "4_4min_Wahoo_Kickr_Core.fit"
# file_name <- "C:/Users/User11/Downloads" %>%
#   {file.path(., list.files(.))} %>%
#   file.info() %>%
#   {rownames(.[which.max(.$atime), ])}
# file_name


# keep <- strava_records %>%
#   sapply(nrow) %>%
#   which.max

load_strava <- function(file_name){
  strava <- readFitFile(file_name)
  strava_records <- records(strava)

  # take the one with the most records
  # but the variables could be split across records, because
  # not all observations have all variables, e.g. heart_rate

  # all records share the timestamp. so join the records via
  # the timestamp variable.

  strava_data <- strava_records[[1]]
  nr <- 2
  for(nr in seq(strava_records)[-1]){
    strava_data <- merge(strava_data, strava_records[[nr]], all = TRUE, by = "timestamp", no.dups = TRUE)
  }

  strava_data %>% head
  amt_na <- apply(strava_data, 2, function(col) sum(is.na(col))) / dim(strava_data)[1]
  keep <- which(amt_na < 0.99)
  strava_data <- strava_data[, keep]

  strava_data <- zoo::na.locf(strava_data)
  names(strava_data) <- gsub(pattern = "[.].*", replacement = "", x = names(strava_data))
  strava_data %>% head
  strava_data

  # keep <- strava_records %>%
  #   lapply(dim) %>%
  #   lapply(prod) %>%
  #   which.max
  #   # sapply(nrow) %>%
  #   # which.max
  #
  # strava_data <- strava_records[[keep]]
  # strava_data
}

# records <- load_strava(file_name)
# head(records)


check_power <- function(records){
  has_power <- !is.null(records$power)
  if(has_power){

    max_power <- 800
    clean_avg <- records$power[records$power <= max_power] %>% mean
    records$power[records$power > max_power] <- clean_avg
    return(records)
  }

  w <- sapply(records$heart_rate, function(hr) which.min(abs(hr - nrg$hr)))

  plot(nrg$watt[w], records$power)
}

# records <- check_power(records)


# efficiency <- records$power / records$heart_rate
# plot(efficiency, type = "l")
# efficiency %>% mean


used_carbs <- function(records){
  avg_power <- records$power %>% mean
  time <- records$timestamp %>%
    range %>%
    {difftime(time1 = .[2], time2 = .[1], units = "hours")} %>%
    as.numeric

  idx <- which(nrg$watt == round(avg_power))[1]
  carbs <- nrg[idx, ]$carbs
  time*carbs

  records$power
}

# used_carbs(records)

