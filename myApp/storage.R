library(googlesheets)
library(dplyr)

table <- "text_prediction_app"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  table %>%
    gs_title %>%
    gs_read_csv
}

