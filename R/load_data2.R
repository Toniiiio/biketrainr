library(xml2)
library(magrittr)
library(rvest)
library(geosphere)

nr <- 1
for(nr in 1:4){
  print(nr)
  file_name <- paste0("Feldbergking (", nr,").gpx")

  qq <- readLines(file_name)
  qqq <- qq %>% gsub(pattern = "hr", replacement = "heart")
  writeLines(qqq, con = file_name)

  xx <- xml2::read_html(file_name)
  nodes <- rvest::html_nodes(xx, "trkpt")

  xpathes <- c("ele", "time", "extensions/trackpointextension/speed", "extensions/trackpointextension/heart")
  names(xpathes) <- c("elevation", "time", "speed", "heart_rate")

  node <- nodes[1]
  values <- lapply(nodes, function(node){
    df <- data.frame(lat = html_attr(node, name = "lat"), lon = html_attr(node, name = "lon"))
    df2 <- sapply(xpathes, function(xpath) html_nodes(node, xpath = xpath) %>% html_text() %>% toString) %>% t %>% data.frame
    cbind(df, df2)
  })

  track_raw <- do.call(rbind, values) %>% as.data.frame()
  #track_raw


  options(digits.secs = 3)
  track_raw$time <- strptime(track_raw$time, "%Y-%m-%dT%H:%M:%OSZ")
  track_raw$time %>% summary
  track_raw$duration <- c(0, diff(track_raw$time))

  track_raw$lat %<>% as.numeric()
  track_raw$lon %<>% as.numeric()


  n <- dim(track_raw)[1]
  dists <- distHaversine(p1 = track_raw[1:(n - 1), c("lon", "lat")], p2 = track_raw[2:n, c("lon", "lat")])
  track_raw$dists <- c(0, dists)
  track_raw$speed2 <- track_raw$dists / track_raw$duration * 3600 / 1000


  output_name <- file_name %>% gsub(pattern = "gpx", replacement = "RData")
  save(track_raw, file = output_name)
}

plot(track_raw$speed, track_raw$speed2, ylim = c(0, 40))

track_raw$lon
track_raw %>% head


