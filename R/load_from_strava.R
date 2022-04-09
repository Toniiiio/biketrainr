library(remotes)
#remotes::install_github("grimbough/FITfileR")
# usethis::create_github_token()
library(FITfileR)
library(magrittr)
library(tidyverse)

source("R/gen_energy_data.R")

file_name <- "01_04_2022_Evening_Ride.fit"
file_name <- "31_03_2022_Lunch.fit"
# file_name <- "C:/Users/User11/Downloads" %>%
file_name <- "data" %>%
  {file.path(., list.files(.))} %>%
  file.info() %>%
  {rownames(.[which.max(.$atime), ])}
file_name


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
  names(strava_data) <- gsub(pattern = "[.].*", replacement = "", x = names(strava_data))

  # wahoo sometimes produces watt = 65535, see e.g. 2.4.22. Set as NA and replace by previous value, as other NAs
  # have to be replaced anyway. The issue of 65535 could be if wahoo kickr does not get power from outlet, then 65535 are set.
  strava_data$power[which(strava_data$power == 65535)] <- NA
  strava_data <- zoo::na.locf(strava_data, na.rm = FALSE)
  strava_data$power %>% summary

  nas <- is.na(strava_data)
  rmv <- which((rowSums(nas) / ncol(nas)) > 0.75) # remove incomplete lines - mostly first one.
  strava_data <- strava_data[-rmv, ]

  strava_data %>% head
  strava_data$power %>% summary

  return(strava_data)

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

# file_name
# records <- load_strava(paste0("data/", file_name))
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


# file_name <- "data/03_04_2022.fit"
# #"data/",
# records <- load_strava(paste0(file_name))
