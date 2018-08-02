############################################
## Michael Hulin
## August 2018
## Coursera Data Science Capstone Project
## Word Prediction Application
## Uses 4 ngram models and a backoff algorithm to determine the most likely next word
## Shiny app: https://pavefe.shinyapps.io/Word_predictor/
## Presentation: 
############################################
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(stringr)

# Load data and prediction helper functions
source("dt_prediction.R")  

shinyServer(function(input, output) {
  
  
  # Reactive statement for prediction function when user input changes ####
  prediction <- reactive({
    # Get input
    inputText <- input$Input
    
    #nPredictions <- input$slider
    
    # Predict the next word
    Get_next_word(inputText)
   
  })
  
    # Output data table ####
  output$table <- renderTable(prediction(), na = "Nothing Predicted")
    
})