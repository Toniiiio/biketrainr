source("R/gen_energy_data.R")
source("R/load_from_strava.R")

file_name <- "C:/Users/User11/Downloads/Morga.fit"
file_name <- "C:/Users/User11/Downloads" %>%
  {file.path(., list.files(.))} %>%
  file.info() %>%
  {rownames(.[which.max(.$atime), ])}
file_name

records <- load_strava(file_name)

baseline = c(79, 111, 121, 129, 139, 150, 154)
watts <- c(0, 100, 130, 160, 190, 220, 230)

plot(records$power.y)
hrs <- sapply(seq(watts), function(idx){
  cand <- which(abs(records$power.x - watts[idx]) < 5)
  cc <- cand[cand < idx*3*60]

  start <- ceiling(length(cc)/2)
  idxs <- cc[start:length(cc)]

  mean(records$heart_rate.x[idxs], na.rm = TRUE)
})

hrs
hrs - baseline

