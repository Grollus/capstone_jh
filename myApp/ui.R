library(shiny)
library(googlesheets)
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  theme = 'bootstrap.css',
  
  titlePanel("Word Prediction App", windowTitle = "A Naive Approach to Text Prediction"),
  h5("a simple approach to text prediction"),
  
  br(),
  sidebarPanel(id = "form",
    textInput("textbox", label = tags$small("Enter Text:"), value = NULL, width = "100%"),
    helpText(tags$small("Simply enter part of a sentence or a phrase. Our prediction will then appear, no clicking necessary.
                        Our best guess is highlighted in red.")),
    numericInput("n", tags$small("How many results should I display?"), value = 3,min = 1, max = 3),
    helpText(tags$small("To keep things working speedily, a maximum of three predictions are returned")),
    checkboxGroupInput("correct_prediction",tags$small("Was the correct word amongst the top predictions?"),
                   choices = c("Yes", "No"), selected = NULL, width = '100%'),
    fluidRow(
      tags$small(textOutput("cor_pred"))
    ),
    helpText(tags$small("To help improve accuracy, please tell us whether the app correctly predicted your sentence.
             Once you indict Yes/No, click 'Log Results'.")),
    helpText(tags$small(strong("Note:"), "Clicking 'Log Results' resets all input.  To view past input, click
             the 'View Results' tab.")), 
    actionButton('log', "Log Results")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(id = "pred", "Predictions",
              br(),
              fluidRow(
                column(4,
                       textOutput("prediction1"),
                       tags$style("#prediction1{color: red;
                                  font-size: 25px}"))
                ,
                column(4,
                       conditionalPanel(
                         condition = "input.n > 1",
                         textOutput("prediction2"),
                         tags$style("#prediction2{color: black;
                                    font-size: 25px}"))
                       ),
                column(4,
                      conditionalPanel(
                        condition = "input.n > 2",
                        textOutput("prediction3"),
                        tags$style("#prediction3{color: black;
                                   font-size: 25px}"))
                      ) 
              ),
              fluidRow(
                column(4,
                       textOutput("score1"))
                ,
                column(4,
                       conditionalPanel(
                         condition = "input.n > 1",
                         textOutput("score2")
                       ))
                ,
                column(4,
                       conditionalPanel(
                         condition = "input.n > 2",
                         textOutput("score3")
                       ))
              ),
              fluidRow(
                shinyjs::hidden(span(id = "logMsg", "Logging data..."))
              )),
      tabPanel(id = "wc", "WordCloud",
               plotOutput(outputId = 'wordcloud')),
      tabPanel(id = "results", "View Results",
               dataTableOutput("resultsTable"),
               br(),
               textOutput("accuracy")),
      tabPanel(id = "instructions", "How Does This Work?",
               p("How does this magic piece of software work you might ask? The work 
                 started long ago with the deconstruction of", a(href = "http://www.corpora.heliohost.org/", "this"),
                 "dataset.  It consists of approximately 4.2 million lines of text from
                 blogs, news articles and tweets."),
               p("To start, this data was cleaned of all punctuation, most special
                 characters, most numbers and all separators/whitespace.  Several other cleaning
                 procedures were tested and the final model is a balance that results in
                 a very performant model with quick, easy creation of data."),
               p("Next, we break the data down into tokens/ngrams.  
                 For instance, if we break 'Once upon a time' into its bigram components,
                 we get the following-- 'once_upon, upon_a, a_time'.  We feed 75% of the cleaned 
                 data into a 'tokenizer' function that creates bi, tri, quad and quintgrams
                 from the dataset.  These tokens are then aggregated to determine the 
                 frequency with which each ngram occurs."),
               p("This frequency table is the key to our model, known as 'Stupid-Backoff'.
                 Stupid-Backoff is a naive algorithm that does not 'learn' from users
                 speech patterns.  When you enter a phrase, the app cleans and tokenizes 
                 the input text.  Then it searches through the frequency tables we created
                 looking for matches to the input phrase. If you
                 enter 'I have a dream that one day', it will look for all entries that
                 match 'dream that one day' in the quintgram table, 'that one day' in the quadgram table and so on.  Any matches will
                 be added to a table with their 'next' word and its relative frequency given the preceding text."),
               p("A score is then calculated for each possible 'next' word in the following manner-- ",
                 withMathJax(h3("$$\\text{Quintgram Match: } score = relative freq$$")),
                 withMathJax(h3("$$\\text{Quadgram Match: } score = relative freq * 0.4$$")),
                 withMathJax(h3("$$\\text{Trigram Match: } score = relative freq * 0.4^2$$")),
                 withMathJax(h3("$$\\text{Bigram Match: } score = relative freq * 0.4^3$$")),
                 "The highest ngram matched is the score used. The result is that matches in higher ngrams receive 
                 higher scores and are more likely to be predicted.  Intuitively, this
                 means that longer strings of matching text give us more confidence in our predicted
                 next word."),
               p("Ultimately, the words with the highest score are returned.  Scores 
                 should not be interpreted as probabilities, merely as a way to rank words
                 relative to other returned results."),
               p("The word cloud is merely eye candy.  A fun way to visualize the results of the model.
                 It calls the same scoring function used in the prediction tab, but gathers 
                 more results.  The plot is scaled by score."))
      
      
    )
  )
))
  