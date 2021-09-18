library(shiny)
library(ggplot2)

source("R/hit_lit_ratio.R")

names(hit_sec) %>% as.numeric %>% as.Date()
hit_sec

week_nr <- format(as.Date("2021-01-01"), "%U") %>% as.numeric

date <- names(hit_sec) %>% as.numeric %>% as.Date
week <- data.frame(
  date,
  hit_sec,
  ride_duration_sec,
  total_duration_sec
)
week
week$week_nr <- format(as.Date(week$date), "%U") %>% as.numeric
week %<>% group_by(week_nr) %>% mutate(week_load_total = sum(ride_duration_sec)/3600)
week %<>% group_by(week_nr) %>% mutate(week_load_hit = sum(hit_sec)/3600)
week %<>% group_by(week_nr) %>% mutate(week_load_lit = week_load_total - week_load_hit)
week

week %<>% mutate(hit_ratio = week_load_hit/week_load_total*100)
week

week_aggr <- week %>% select(week_nr, week_load_hit, week_load_lit, hit_ratio) %>% unique
week_aggr_long <- melt(week_aggr, id.vars = c("week_nr", "hit_ratio"))

# display text only for hit
week_aggr_long %<>%
  mutate(hit_ratio = ifelse(variable == "week_load_hit", paste0(round(hit_ratio, 0), "% HIT"), paste0(100 - round(hit_ratio, 0), "% LIT")))


ui <- fluidPage(
  fluidRow(
    column(6,
           plotOutput("plot1", click = "plot1_click")
    ),
    column(5,
           uiOutput("text")
    )
  )
)

server <- function(input, output) {
  global <- reactiveValues(toHighlight = rep(FALSE, length(letters$word)),
                           selectedBar = NULL)

  observeEvent(eventExpr = input$plot1_click, {
    week <- round(input$plot1_click$x)
    print(week_nr)
    print(input$plot1_click$y)

    y <- input$plot1_click$y
    week_sel <- week_aggr %>% filter(week_nr == week)
    week_sel
    is_lit <- y > 0 & y <= week_sel$week_load_lit
    is_hit <- y > week_sel$week_load_lit & y <= week_sel$week_load_hit + week_sel$week_load_lit
    print(is_hit)
    print(is_lit)
  })

  output$plot1 <- renderPlot({
    ggplot(week_aggr_long, aes(fill = variable, y = value, x = week_nr), label = hit_ratio) +
      geom_bar(position = "stack", stat = "identity") +
      geom_text(aes(label = hit_ratio), size = 3, position = position_stack(vjust = 0.5))
  })

  output$text <- renderUI({
    req(global$selectedBar)
    textInput(inputId = "label", label = "selected text:", value = global$selectedBar)
  })
}
shinyApp(ui, server)


# volume, HIT, HIT Ratio, Consistency
# LIT really LIT?
# low TSS before HIT?
# https://v1.nitrocdn.com/IAEZpQbHvRqkjIDrVgmeAoKybhOCdsVs/assets/static/optimized/rev-5423573/wp-content/uploads/2018/03/seiler-hierarchy-of-endurance-training-needs.jpg

# 25 < Volume > 7
