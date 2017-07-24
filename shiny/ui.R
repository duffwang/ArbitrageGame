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
  h5('Author: jeff.wang@cutlergrouplp.com'),
  hr(),
  
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(10,
           DT::dataTableOutput('dt.stock'),
           DT::dataTableOutput('dt.market')
          
           
    ),
    column(2, 
           sliderInput("trade.speed",
                       "Market Speed:",
                       min = 0,
                       max = 5,
                       value = 0),
           actionButton("new.market", "New Market"),
           actionButton("find.arb", "Show Arbitrages"))
  )
))
