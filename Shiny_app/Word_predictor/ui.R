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
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coursera Word Predictor"),
  
  # Sidebar with a slider input for number of words returned
  sidebarLayout(
    sidebarPanel(
      textInput("Input",
                label = ("Enter some words:"),
                value = "just in")),
      
      
    # Show a word cloud plot
    mainPanel(h4("Table of Predicted Words"), 
              # Output in a table
              tableOutput('table')
              
    ) # MAINPANEL
  ) # Sidebar Layout
)  #fLUID pAGE
) # ShinyUI
  