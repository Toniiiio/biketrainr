#https://github.com/danvk/dygraphs/blob/e9155b2c442bf69396d5a456c885f938d38d35da/src/plugins/legend.js#L193
library(dygraphs)
library(magrittr)
library(datasets)
library(shiny)
library(shinyjs)
library(xts)
library(leaflet)

hr_max <- 190
hr_lit <- c(124, 146, 162)

track_raw <- records
track_raw$lat <- track_raw$position_lat
track_raw$lon <- track_raw$position_long
track_raw$time <- track_raw$timestamp
track_raw$time %<>%  as.POSIXct(tz = "CET")
track_raw$heart_rate %<>% as.numeric()

track <- track_raw
head(track)
tail(track)

track$distance %<>% round
track$enhanced_altitude

idx <- which("heart_rate" == names(track) | "enhanced_altitude" == names(track)| "speed" == names(track))
qxts <- xts(track[, idx], order.by = track$time) #track$time


has_na <- is.na(track_raw) %>% sum
if(has_na) stop("NA in data")

# track$distance2 <- as.Date(track$distance*365)
# track$distance2 %>% range
# qxts <- xts(as.numeric(track$heart_rate), order.by = track$distance2)
# dygraph(qxts, main = "Predicted Deaths/Month") %>%
#   dyAxis("y", label = "Heart rate", valueRange = c(50, 210)) %>%
#   dyAxis("x", label = "Heart rate", valueRange = c("1974-12-31", "1965-07-04"))
# nhtemp
# plot(track$speed)

ui <- shinyUI(fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      div(strong("From: "), textOutput("from", inline = TRUE)),
      div(strong("To: "), textOutput("to", inline = TRUE)),
      div(strong("Date: "), textOutput("clicked", inline = TRUE)),
      br(),
      uiOutput("create_seq"),
      br(),
      helpText("Click and drag to zoom in (double click to zoom back out).")
    ),
    mainPanel(
      div(id = "msvr", dygraphOutput("dygraph")),
      leafletOutput('trackmap')
    )
  )
))


server <- shinyServer(function(input, output, session) {

  n <- track_raw %>% nrow
  global <- reactiveValues(track = track_raw, keep_time = rep(TRUE, n), heart_range = 1:n,
                           map_updated_at = Sys.time() + 3,
                           dy_updated_at = Sys.time() + 3, sub_seq = NULL, just_map_updated = FALSE,
                           seq_updated_at = Sys.time() + 3)

  # onevent("mousemove", "msvr", function(event) print(event$offsetX))

  observeEvent(input$trackmap_bounds, {
    # print("UPDATETRy")
    # print(global$map_updated_at)
    # print(Sys.time())
    if(Sys.time() > global$map_updated_at + 1){
      # print("UPDATE")
      global$map_updated_at <- Sys.time()
    }
  })

  observeEvent(input$trackmap_bounds, {
    req(!is.null(input$trackmap_bounds))
    global$trackmap_bounds <- input$trackmap_bounds
  })

  # it has to have an update from graph window without having had a previous update from the map.
  # one way would be to enforce there is a time window (e.g. x seconds) in which another update is forbidden.
  # x has to be large enough to avoid having circular updates (leaflet and dygraph triggering each other), but
  # small enough to allow user to make fast updates. --> Trying with 1 second.
  observeEvent(input$dygraph_date_window, {
    global$dy_updated_at <- Sys.time()
  })


  observeEvent(global$dy_updated_at, {
    dy_triggered <- global$dy_updated_at + 1 >= Sys.time()
    if(dy_triggered){
      print("da")
      # print(input$dygraph_date_window)
      from = req(input$dygraph_date_window[[1]])
      to = req(input$dygraph_date_window[[2]])
      from <- strptime(from, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
      to <- strptime(to, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600

      start <- which.min(abs(track_raw$time - from))
      end <- which.min(abs(track_raw$time - to))
      global$heart_range <- start:end

      global$sub_track <- track_raw[global$heart_range, ]
      lats <- global$sub_track$position_lat %>% range
      longs <- global$sub_track$position_long %>% range
      leafletProxy("trackmap", session) %>%
        fitBounds(lng1 = longs[1], lng2 = longs[2], lat1 = lats[1], lat2 = lats[2])
      # global$dy_updated_at <- FALSE
    }else{
      print("njet")
    }

  })

  observeEvent(c(global$dy_updated_at, global$heart_range), {
    # req(global$dy_updated_at == TRUE)
    # print(global$dy_updated_at)

    from = req(input$dygraph_date_window[[1]])
    to = req(input$dygraph_date_window[[2]])
    from <- strptime(from, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
    to <- strptime(to, "%Y-%m-%dT%H:%M:%OSZ") + 2*3600

    start <- which.min(abs(track_raw$time - from))
    end <- which.min(abs(track_raw$time - to))
    global$heart_range <- start:end

    is_subset <- length(track_raw$timestamp) != length(start:end)
    if(is_subset){
      global$sub_track <- track[global$heart_range, ]
    }else{
      global$sub_track <- NULL
    }
  })

  target_icon <- icons(
    iconUrl = "https://cdn.icon-icons.com/icons2/817/PNG/512/thefreeforty_target_icon-icons.com_66342.png",
    iconWidth = 20, iconHeight = 20
  )

  cycle_icon <- icons(
    iconUrl = "https://w7.pngwing.com/pngs/873/319/png-transparent-bicycle-safety-cycling-cyclist-icon-text-bicycle-logo.png",
    iconWidth = 20, iconHeight = 20
  )

  observeEvent(c(global$dy_updated_at, input$choose_seq), {

    global$keep_time

    if(!is.null(global$sub_seq)){
      # print(global$sub_seq[[input$choose_seq]] %>% length)
      global$sub_track <- track_raw[global$sub_seq[[input$choose_seq]], ]

      # print("kkllk")
      # print(input$choose_seq)
      # print(global$sub_seq)
      # print(global$sub_seq[[input$choose_seq]])

      # global$dy_track <- track_raw[1:length(track_raw$timestamp) %in% global$sub_seq[[input$choose_seq]], ] might need this
    }
    just_sec_updated <- global$seq_updated_at + 1 >= Sys.time()

    leafletProxy("trackmap", session) %>%
      removeShape("init_poly") %>%
      removeShape("add_poly_base") %>%
      removeShape("add_poly_sub") %>%
      addPolylines(data = track, lng = ~lon, lat = ~lat, layerId = "add_poly_base")

    if(!is.null(global$sub_seq)){
      leafletProxy("trackmap", session) %>%
        addPolylines(data = global$sub_track, lng = ~lon, lat = ~lat, color = "green", layerId = "add_poly_sub")
    }

  })

  output$trackmap = renderLeaflet({
    # global$trigger_map_update
    isolate({
      # print("global$dy_updated_at223")
      lon <- track$lon %>% range(na.rm = TRUE)
      lat <- track$lat %>% range(na.rm = TRUE)
      m <- leaflet() %>% addTiles()

      finish <- c(tail(track$lon, 1), tail(track$lat, 1))
      m %>% fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
        addPolylines(data = track, lng = ~lon, lat = ~lat, layerId = "init_poly") %>%
        addMarkers(lng = finish[1], lat = finish[2], icon = target_icon)

    })
  })

  observeEvent(input$mouse, {
    req(input$mouse)
    tr <- track[input$mouse$x, ]
    leafletProxy("trackmap", session) %>%
      removeMarker(layerId = "current_pos") %>%
      addMarkers(lng = tr$position_long, lat = tr$position_lat, layerId = "current_pos", icon = cycle_icon)

  })

  output$create_seq <- renderUI({

    ww <- global$keep_time %>% which
    end_first_seq <- which(diff(ww) != 1)

    if(length(end_first_seq)){
      start_sec_seq <- end_first_seq + 1
      first_seq <- ww[1:end_first_seq]
      second_seq <- ww[start_sec_seq:length(ww)]

      global$sub_seq <- list(
        Hinweg = first_seq,
        Rueckweg = second_seq,
        Beide = ww
      )

      radioButtons("choose_seq", "Choose sequences", c("Hinweg", "Rueckweg", "Beide"), selected = "Beide")
    }else{
      global$sub_seq <- NULL
    }
  })

  observe({
    input$choose_seq
    isolate({global$seq_updated_at <- Sys.time()})
  })

  observeEvent(c(global$trigger_dygraph_update, global$map_updated_at), {
    # print(global$map_updated_at)
    map_is_init <- !is.null(global$trackmap_bounds)
    req(map_is_init)

    global$trigger_dygraph_update
    # if update was less than a second ago - now is smaller/equal to then + 1
    manual_update <- global$map_updated_at +1 >= Sys.time()
    # print(global$map_updated_at)
    # print(Sys.time())
    # print(global$map_updated_at + 1 >= Sys.time())
    # print("manual_update")
    # print(manual_update)
    #   # req(manual_update == TRUE)
    isolate({
      # print("dss")
      bounds <- global$trackmap_bounds
      # print(bounds)
      global$keep_time <- track_raw$lat < bounds$north & track_raw$lat > bounds$south &
        track_raw$lon < bounds$east & track_raw$lon > bounds$west
      print(global$keep_time %>% sum)
    })
  })

  observe({
    input$choose_seq
    input$dygraph_date_window

    global$just_map_updated <- global$map_updated_at +1 >= Sys.time()
    # print("yeah")
    # print(global$map_updated_at + 1)
    # print(Sys.time())
    # print(global$just_map_updated)
    req(global$seq_updated_at)

    just_sec_updated <- global$seq_updated_at + 1 >= Sys.time()
    print("just_sec_updated")
    dy_triggered <- global$dy_updated_at + 1 >= Sys.time()
    print(dy_triggered)
    if(global$just_map_updated & !dy_triggered){
      print("need dygraph update")
      global$trigger_dygraph_update <- rnorm(1)
    }
  })

  observe({
    global$dy_track <- track_raw[global$keep_time, ]
  })


  output$dygraph <- renderDygraph({

    global$trigger_dygraph_update
    print("keep time")
    isolate({
      track <- global$dy_track
      # print(str(track))
      track$speed %<>% as.numeric
      # df <- data.frame(x = track$time, y = track$speed) # toberemoved

      dygraph(qxts, main = "Heart rate over time") %>%
        dyAxis("y", label = "Heart rate", valueRange = c(50, 210), valueFormatter = "function(v, opts, seriesName, dygraph, row) {
            Shiny.onInputChange('mouse', {'x': row, 'val': v})
          	return v;
          }") %>%
        dyAxis("y2", label = "altitude", independentTicks = TRUE) %>%
        dySeries("enhanced_altitude", axis = ('y2'), fillGraph = TRUE, color = "grey", strokeWidth = 1, strokePattern = "dashed") %>%
        dyLimit(hr_lit[1], color = "blue") %>%
        dyLimit(hr_lit[2], color = "blue") %>%
        dyLimit(hr_lit[3], color = "blue") %>%
        dyRangeSelector()
    })

  })

  output$from <- renderText({
    date_time <- strptime(req(input$dygraph_date_window[[1]]), "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
    strftime(date_time, "%H:%M:%OS")
  })

  output$to <- renderText({
    date_time <- strptime(req(input$dygraph_date_window[[2]]), "%Y-%m-%dT%H:%M:%OSZ") + 2*3600
    strftime(date_time, "%H:%M:%OS")
  })

  output$clicked <- renderText({
    strftime(req(input$dygraph_date_window[[1]]))
  })

  output$point <- renderText({
    paste0('X = ', strftime(req(input$dygraph_click$x_closest_point)),
           '; Y = ', req(input$dygraph_click$y_closest_point))
  })
})


shinyApp(ui, server)

