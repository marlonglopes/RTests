
shiny <- function(){

  if (interactive()) {
  library(shiny)
  library(shinythemes)

  ui <- fluidPage(
    shinytheme("cerulean"),
    timevisOutput("timeline"),
    div("Selected items:", textOutput("selected", inline = TRUE)),
    div("Visible window:", textOutput("window", inline = TRUE)),
    tableOutput("table"),
    actionButton("btn", "Show current time and allow items to be editable")
  )


  check_for_update <- function() {
    loadData()
  }
  get_data <- function() {
    data = loadData()
    data[ data == "NA" ] = NA
    data[] <- lapply(data, factor)
    data
  }
 
  server <- function(input, output, session) {
 
   data <- reactivePoll(10000, session, checkFunc = check_for_update, valueFunc = get_data)
 
    #output$timeline <- renderTimevis(
    #  timevis(
    #    data.frame(start = Sys.Date(), content = "Today"),
    #    options = list(showCurrentTime = FALSE, orientation = "top")
    #  )
    #)
    
    #observeEvent(input$btn, {
    #  setOptions("timeline", list(editable = TRUE, showCurrentTime = TRUE))
    #})

   output$timeline <- renderTimevis(
      timevis(
        data(), getSelected = TRUE, getData = TRUE, getWindow = TRUE,
        options = list(editable = TRUE, multiselect = TRUE, align = "center", showCurrentTime = TRUE)
      )
    )

    output$selected <- renderText(
      paste(input$appts_selected, collapse = " ")
    )

    output$window <- renderText(
      paste(input$appts_window[1], "to", input$appts_window[2])
    )

    output$table <- renderTable(
      input$timeline_data
    )


  }
 
  shinyApp(ui, server)
 
  }
  
}


shiny()