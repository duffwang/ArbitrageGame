#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(scales)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  generateStock <- function() {
    num.stock <- round(runif(1) * 50+10, 2)
    num.int <<- round(runif(1),1)
    num.div <<- round(runif(1)*2,1)
    if (runif(1) < 0.3) {
      num.div <<- 0
    }
    data.table(BidQuantity = 100, StockBid = num.stock, StockOffer = num.stock+0.01, OfferQuantity = 100, interest = num.int, dividends = num.div)
  }
  
  chr.exp = 'JUL21 17'
  
  generateNewMarket <- function(num.strike, num.stock, num.int, num.div) {
    mBpC <- round(sapply(num.strike, function(x) exp((num.stock - x)/(2.7+runif(1)*0.6))), 2)
    mBpC <- round((mBpC * 100 - as.integer(mBpC*100) %% 5) / 100, 2) #round to nickels
   
    mBpP <- round(sapply(num.strike, function(x) exp((x - num.stock)/(2.7+runif(1)*0.6))), 2)
    mBpP <- round((mBpP * 100 - as.integer(mBpP*100) %% 5) / 100, 2) #round to nickels

    mBpP[num.strike >= num.stock] = mBpC[num.strike >= num.stock] - 
      (num.stock - num.strike[num.strike >= num.stock] + num.int - num.div)
    mBpC[num.strike < num.stock] = mBpP[num.strike < num.stock] + 
      (num.stock - num.strike[num.strike < num.stock] + num.int - num.div)
    
    #No negative prices
    mBpC[mBpC < 0] = 0
    mBpP[mBpP < 0] = 0
    
    mApC = mBpC + 0.05 * (length(num.strike) + 1 - seq_along(num.strike))
    mApP = mBpP + 0.05 * (seq_along(num.strike))
    
    #Fix any floating point errors
    mBpP <- round(mBpP, 2)
    mApC <- round(mApC, 2)
    mBpC <- round(mBpC, 2)
    mApP <- round(mApP, 2)
    
    mBsC = round(rnorm(n = length(num.strike), mean = 100, sd = 20) / (1 + abs(num.stock - num.strike)))
    mAsC = round(rnorm(n = length(num.strike), mean = 100, sd = 20) / (1 + abs(num.stock - num.strike)))
    mBsP = round(rnorm(n = length(num.strike), mean = 100, sd = 20) / (1 + abs(num.stock - num.strike)))
    mAsP = round(rnorm(n = length(num.strike), mean = 100, sd = 20) / (1 + abs(num.stock - num.strike)))
    
    data.table(mBsC, mBpC, mApC, mAsC, id = paste(chr.exp, num.strike), mBsP, mBpP, mApP, mAsP, arb = NA, Correct = NA_character_)
    
  }
  
  refreshMarket <- function() {
    output$dt.market <- DT::renderDataTable(dt.market, server=FALSE, selection = list(target = 'row'), rownames= FALSE)
  }
  refreshStock <- function() {
    output$dt.stock <- DT::renderDataTable(dt.stock, server=FALSE, rownames= FALSE)
  }
 
  
  marketMovement <- function(dt.market) {
    n = nrow(dt.market)
    dt.update.code <- data.table(bid.call = runif(n), ask.call = runif(n), bid.put = runif(n), ask.put = runif(n))
    dt.update.code[dt.update.code < 0.4] = 1 #no change
    dt.update.code[dt.update.code < 0.6] = 2 #decrease quantity
    dt.update.code[dt.update.code < 0.8] = 3 #increase quantity
    dt.update.code[dt.update.code < 0.9] = 4 #price down
    dt.update.code[dt.update.code < 1.0] = 5 #price up
    
    #change quantities
    dt.market$mBsC[dt.update.code$bid.call == 2] = dt.market$mBsC[dt.update.code$bid.call == 2] -  round(runif(length(dt.market$mBsC[dt.update.code$bid.call == 2])) * 30)
    dt.market$mBsC[dt.update.code$bid.call == 3] = dt.market$mBsC[dt.update.code$bid.call == 3] +  round(runif(length(dt.market$mBsC[dt.update.code$bid.call == 3])) * 30)
    dt.market$mAsC[dt.update.code$ask.call == 2] = dt.market$mAsC[dt.update.code$ask.call == 2] -  round(runif(length(dt.market$mBsC[dt.update.code$ask.call == 2])) * 30)
    dt.market$mAsC[dt.update.code$ask.call == 3] = dt.market$mAsC[dt.update.code$ask.call == 3] +  round(runif(length(dt.market$mBsC[dt.update.code$ask.call == 3])) * 30)
    dt.market$mBsP[dt.update.code$bid.put == 2] = dt.market$mBsP[dt.update.code$bid.put == 2] -  round(runif(length(dt.market$mBsC[dt.update.code$bid.put == 2])) * 30)
    dt.market$mBsP[dt.update.code$bid.put == 3] = dt.market$mBsP[dt.update.code$bid.put == 3] +  round(runif(length(dt.market$mBsC[dt.update.code$bid.put == 3])) * 30)
    dt.market$mAsP[dt.update.code$ask.put == 2] = dt.market$mAsP[dt.update.code$ask.put == 2] -  round(runif(length(dt.market$mBsC[dt.update.code$ask.put == 2])) * 30)
    dt.market$mAsP[dt.update.code$ask.put == 3] = dt.market$mAsP[dt.update.code$ask.put == 3] +  round(runif(length(dt.market$mBsC[dt.update.code$ask.put == 3])) * 30)
    
    #change prices
    if (sum(dt.update.code$bid.call == 4) > 0)
      dt.market$mBpC[dt.update.code$bid.call == 4] = pmax(0.05, dt.market$mBpC[dt.update.code$bid.call == 4] -  .05 * round(runif(length(dt.market$mBpC[dt.update.code$bid.call == 4])) * 6))
    if (sum(dt.update.code$ask.call == 5) > 0)
      dt.market$mApC[dt.update.code$ask.call == 5] = pmax(0.05, dt.market$mApC[dt.update.code$ask.call == 5] + .05 * round(runif(length(dt.market$mApC[dt.update.code$ask.call == 5])) * 6))
    if (sum(dt.update.code$bid.put == 4) > 0)
      dt.market$mBpP[dt.update.code$bid.put == 4] = pmax(0.05, dt.market$mBpP[dt.update.code$bid.put == 4] -  .05 * round(runif(length(dt.market$mBpP[dt.update.code$bid.put == 4])) * 6))
    if (sum(dt.update.code$ask.put == 5) > 0)  
      dt.market$mApP[dt.update.code$ask.put == 5] = pmax(0.05, dt.market$mApP[dt.update.code$ask.put == 5] +  .05 * round(runif(length(dt.market$mApP[dt.update.code$ask.put == 5])) * 6))
    #change prices but also keep spreads 
    if (sum(dt.update.code$bid.call == 5) > 0)
      dt.market$mBpC[dt.update.code$bid.call == 5] = pmin(dt.market$mApC[dt.update.code$bid.call == 5] - .05, pmax(0, dt.market$mBpC[dt.update.code$bid.call == 5] +  .05 * round(runif(length(dt.market$mBpC[dt.update.code$bid.call == 5])) * 6)))
    if (sum(dt.update.code$ask.call == 4) > 0)
      dt.market$mApC[dt.update.code$ask.call == 4] = pmax(dt.market$mBpC[dt.update.code$ask.call == 4] + .05, dt.market$mApC[dt.update.code$ask.call == 4] -  .05 * round(runif(length(dt.market$mApC[dt.update.code$ask.call == 4])) * 6))
    if (sum(dt.update.code$bid.put == 5) > 0)
      dt.market$mBpP[dt.update.code$bid.put == 5] = pmin(dt.market$mApP[dt.update.code$bid.put == 5] - .05,pmax(0, dt.market$mBpP[dt.update.code$bid.put == 5] +  .05 * round(runif(length(dt.market$mBpP[dt.update.code$bid.put == 5])) * 6)))
    if (sum(dt.update.code$ask.put == 4) > 0)
      dt.market$mApP[dt.update.code$ask.put == 4] = pmax(dt.market$mBpP[dt.update.code$ask.put == 4] + .05, dt.market$mApP[dt.update.code$ask.put == 4] -  .05 * round(runif(length(dt.market$mApP[dt.update.code$ask.put == 4])) * 6))
    
    #refresh quantities
    dt.market$mBsC[dt.update.code$bid.call >= 4 | dt.market$mBsC <= 0] = round(rnorm(n = length(dt.market$mBsC[dt.update.code$bid.call >= 4 | dt.market$mBsC <= 0]), 
                                                                               mean = 30, sd = 5))
    dt.market$mAsC[dt.update.code$ask.call >= 4 | dt.market$mAsC <= 0] = round(rnorm(n = length(dt.market$mAsC[dt.update.code$ask.call >= 4 | dt.market$mAsC <= 0]), 
                                                                                     mean = 30, sd = 5))
    dt.market$mBsP[dt.update.code$bid.put >= 4 | dt.market$mBsP <= 0] = round(rnorm(n = length(dt.market$mBsP[dt.update.code$bid.put >= 4 | dt.market$mBsP <= 0]), 
                                                                                     mean = 30, sd = 5))
    dt.market$mAsP[dt.update.code$ask.put >= 4 | dt.market$mAsP <= 0] = round(rnorm(n = length(dt.market$mAsP[dt.update.code$ask.put >= 4 | dt.market$mAsP <= 0]), 
                                                                                     mean = 30, sd = 5))
    
    #ensure spreads are okay 
    
    dt.market$arb = NA
    dt.market$Correct = NA_character_
    
    
    dt.market
  }
  
  findArbitrage <- function(dt.market, num.strike, num.stock, num.int, num.div) {
    dt.market[,arb := ifelse(mApC - mBpP < num.stock - num.strike + num.int - num.div, 'Reversal', 
                             ifelse(mBpC - mApP > num.stock - num.strike + num.int - num.div, 'Conversion', 'None'))]
    isolate({
      dt.market$Correct = NA_character_
      dt.market[input$dt.market_rows_selected, Correct := ifelse(arb != 'None', 'Correct!', 'Wrong.')]
      dt.market[is.na(Correct) & arb != 'None', Correct := 'Missed.']
    })
    dt.market
  }

  marketTick <- reactiveTimer(1000)
 
  observe({
    marketTick()
    
       i = 0
      while (i < input$trade.speed) {
        dt.market <<- marketMovement(dt.market)
        i = i+1
      }
      if (i > 0) { refreshMarket() }
  })
  
  observe({
    input$new.market
    
    dt.stock <<- generateStock()
    dt.market <<- generateNewMarket(seq(from = round(dt.stock$StockBid) - 10, to = round(dt.stock$StockBid) + 10, by = 2.5)
                                   , dt.stock$StockBid, num.int, num.div)
    refreshMarket()
    refreshStock()
    
  })
  
  observe({
    input$find.arb
    
    if (input$find.arb > 0) {
      dt.market <<- findArbitrage(dt.market, seq(from = round(dt.stock$StockBid) - 10, to = round(dt.stock$StockBid) + 10, by = 2.5), dt.stock$StockBid, num.int, num.div)
      refreshMarket()
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
})
