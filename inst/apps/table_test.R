library(shiny)
library(shinyTable)

a <- "Test"
dt_ <- data.frame(val = as.numeric(11:22), value = 1:12, vals = 31:42, mth = month.abb, sel = c(T, rep(F, 11)), stringsAsFactors = F)

runApp(list(ui = fluidPage(fluidRow(column(6, htable("hdtable", colHeaders = "provided", setColTypes = TRUE)), column(3, 
  uiOutput("outSel")))), server = function(input, output, session) {
  cache <- reactiveValues()
  
  output$hdtable <- renderHtable({
    if (is.null(input$hdtable)) {
      cache[["dt"]] <- dt_
    } else {
      cache[["dt"]] <- input$hdtable
    }
    return(cache[["dt"]])
  })
  
  output$outSel <- renderUI({
    if (!is.null(cache[["dt"]])) {
      dt1 <- cache[["dt"]]
      
      picked <- dt1[dt1$sel == T, "mth"]
      if (length(picked) >= 0) {
        buttn <- 1
        valsTv <- sapply(picked, function(i) {
          if (is.null(input[[paste0(i, "-", "a", buttn)]])) {
          list(value = 0)
          } else {
          list(value = as.numeric(input[[paste0(i, "-", "a", buttn)]]))
          }
        }, simplify = F)
        
        tagList(lapply(picked, function(i) {
          sliderInput(inputId = paste0(i, "-", "a", buttn), label = h6(paste0(i, "")), min = -100, max = 100, 
          step = 1, value = as.numeric(valsTv[[i]]$value), post = "%", ticks = FALSE, animate = FALSE)
        }), dt1)
      }
    }
  })
})) 
