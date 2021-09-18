
file_name <- "C:/Users/User11/Downloads/GA1_89_1km_1574_Hm.fit"
records <- load_strava(file_name)
head(records)

n <- dim(records)[1]
outside <- sum(records$heart_rate > 146)

# measurement is incomplete does not account for short sprints or SST
GA2_plus_ratio <- outside/n*100
GA2_plus_ratio
