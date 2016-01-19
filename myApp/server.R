library(shiny)
library(stringi)
suppressMessages(library(quanteda))
library(magrittr)
suppressMessages(library(data.table))
library(wordcloud)
library(Matrix)
library(googlesheets)

source('storage.R')
fields <- c("textbox", "n")

shinyServer(function(input, output){
  # Enable/Disable logging ability based on mandatory input being present.
  observe({
    shinyjs::toggleState(id = "log", condition = !is.null(input$textbox) && !is.null(input$correct_prediction) 
                         && length(input$correct_prediction) == 1)
  })
  cor_pred <- reactive({
    validate(
      need(length(input$correct_prediction) <=1, "Only one box may be selected at a time!")
    )
  })
  
  output$cor_pred <- renderText({
    cor_pred()
  })
  # Prediction algorithm and scoring function results are obtained here.
  predictions <- reactive({
    sbAlgo(input$textbox, rows = 15, nResults = input$n)
  })
  
  scores <- reactive({
    scoreNgrams(input$textbox)
    
  })
  
  # Output of all prediction and scoring to the predictions tab.
  output$prediction1 <- renderText({
    predictions()[1]
  })
  
  output$prediction2 <- renderText({
    predictions()[2]
  })
  
  output$prediction3 <- renderText({
    predictions()[3]
  })
  
  output$score1 <- renderText({
    scores()$score[1]
  })
  
  output$score2 <- renderText({
    scores()$score[2]
  })
  
  output$score3 <- renderText({
    scores()$score[3]
  })
  
  
  # Use sbAlgo to grab significantly more results than used for prediction tab.
  # I think there is a way to clean this up and only call the predictions and scores 
  # function one time and use for both the wc and the predictions tab.
  #
  # If I have time I will try to rewrite this
  wc.word <- reactive({
    sbAlgo(input$textbox, rows = 15, nResults = 30)
  })
  wc.score <- reactive({
    if(nrow(scoreNgrams(input$textbox, rows = 15)) == 0){
      data.frame(score = rep(1, 3))
    }else{
      scoreNgrams(input$textbox, rows = 15)
    }
    
  })
  
  # Uses output from wc.word and wc.score reactives to form the word cloud. Displays
  # in the "wordCloud" tab.
  output$wordcloud <- renderPlot({
    words <- wc.word()
    score <- wc.score()$score
    wordcloud(words, score, scale = c(5, 2), colors = brewer.pal(8, "Dark2"),
              max.words = 30, min.freq = 0, rot.per = 0, fixed.asp = FALSE)
  })
  
  # Creates a list of data to be feed to the googlesheet for logging.  This can
  # be pretty easily appended to add more logging data as needed.
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data <- c(data, predictions()[1], predictions()[2], predictions()[3], input$correct_prediction, format(Sys.time(), "%Y%m%d-%H%M%OS"))
    data
  })

  # When the Predict button is clicked, log the form data to the google docs spreadsheet
  # NOTE: This logging is not perfect.  It does not have SQL-like protections against
  # simulataneous input.
  #
  # If this app is developed further, some SQL-like logging will have to be written.
  #
  # Button clicking is disabled or hidden as needed to try and prevent things like double
  # clicking and generally trying to control user input and prevent overloading the app
  # and crashing
  # Probably a much better way to do this, but it works for now.
  observeEvent(input$log, {
    
    shinyjs::disable("log")
    shinyjs::show("logMsg")
    on.exit({
      shinyjs::enable("log")
      shinyjs::hide("logMsg")
    })
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
    })
  })
  
  # Create a reactive response_data object
  response_data <- reactive({
    input$log
    loadData()
  })
  # Creates the table showing past results in the "View Results" tab.
  output$resultsTable <- renderDataTable({
    response_data() %>%
      select(-time)%>%
      do(tail(., n = 5))
  }, options = list(searching = FALSE, lengthChange = FALSE, ordering = FALSE, paging = FALSE,
                    info = FALSE, autoWidth = TRUE))
  
  # Calculate the accuracy of results based on user logging
  output$accuracy <- renderText({
    calc_accuracy <- sum(response_data()$correct == "Yes")/nrow(response_data()) * 100
    paste0("This app has predicted ", round(calc_accuracy, 1), "% of the ", nrow(response_data()),  " sentences entered correctly.")
  })
    
})