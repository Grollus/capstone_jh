# Loading necessary data
bi <- readRDS("bi.rds")
tri <- readRDS("tri.rds")
quad <- readRDS("quad.rds")
quint <- readRDS("quint.rds")

profanity <- read.csv("profanity.txt")

field_mandatory <- "textbox"
# Load prediction functions
source("sb_model.R")