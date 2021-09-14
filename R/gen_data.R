library(FITfileR)
library(magrittr)

source("R/gen_energy_data.R")
source("R/load_from_strava.R")

file_name <- "C:/Users/User11/Downloads/Morga.fit"
file_name <- "C:/Users/User11/Downloads" %>%
  {file.path(., list.files(.))} %>%
  file.info() %>%
  {rownames(.[which.max(.$atime), ])}
file_name

records <- load_strava(file_name)
head(records)
#records <- check_power(records)


# efficiency <- records$power / records$heart_rate
# plot(efficiency, type = "l")
# efficiency %>% mean
#
#
# used_carbs(records)
