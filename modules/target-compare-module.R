MAX_PLOTS <- 6

compareTargetsModuleUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("Plot"), width = "100%", height = 1600)
}

compareTargetsModule <- function(input, output, session, mainData, dates) {
  
  tdates <- reactive({ tail(sort(dates()), MAX_PLOTS) })
  
  nplots <- reactive({ length(tdates()) })
  
  output$Plot <- renderPlot({
    req(mainData())
    req(tdates())
    par(mfrow = c(6,2))
    for(i in seq_len(length(tdates()))) {
      date <- tdates()[i]
      dt <- mainData()[mainData()[["Date"]] == date, ]
      plot_target_with_shots(dt)
      title(date)
    }
  })
  
  
  
}