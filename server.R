retreiveColors <- function(data){
  colors <- c(rep(NA, length(data)))
  for(i in 1:length(stocks)){
    for(j in 1:length(data)){
      if(data[j] == stocks[i]){
        colors[j] = arrayColors[i]
      }
    }
  }
  return (colors)
}



retreiveSecondaryColors <- function(data){
  colors <- c(rep(NA, length(data)))
  for(i in 1:length(stocks)){
    for(j in 1:length(data)){
      if(data[j] == stocks[i]){
        colors[j] = complementaryColors[i]
      }
    }
  }
  return (colors)
}

server <- function(input, output) {
    output$simpleCCReturnsOut <- renderText({"Simple and Continuous Compound Return"})
    output$correlationTextOut <- renderText({"Correlation of all stocks (data)"})
    output$simpleCCReturnsPlot <- renderPlot(plotSimpleCCReturns(input$stocksCheckBox,  input$stocksCheckBox, retreiveColors(input$stocksCheckBox)))
    output$betaPlotOut <- renderPlot({(betaPlot(input$stocksCheckBox, input$indexRadioBtn, retreiveColors(input$stocksCheckBox))) })
    output$corrDataPlotOut <- renderPlot(plotCorrelationData(input$stocksCheckBox))
    output$stockIndexPlotOut <- renderPlot(plotStockIndex(input$stocksCheckBox, input$indexRadioBtn, retreiveColors(input$stocksCheckBox)))
    output$diagnosticPlotOut <- renderPlot(showDiagnosticPlots(input$stocksCheckBox, retreiveColors(input$stocksCheckBox), retreiveSecondaryColors(input$stocksCheckBox)))
    output$correlationPairsPlotOut <- renderPlot(plotCorrelationPairs(input$stocksCheckBox))
    output$forecastErrorText <- renderText({"Error! The number of months of testing and training exceeds the limit of 120!"})
    

    observeEvent(list(input$forecastNValue, input$forecastMValue), {
      if(input$forecastNValue + input$forecastMValue > 120 && !is.null(input$forecastRadioBtn)){
        shinyjs::hide(id = "forecastBtn")
        shinyjs::disable(id = "forecastRadioBtn")
        shinyjs::hide(id = "forecastPlot")
        shinyjs::show(id = "forecastErrorText")
      }
      else{
        shinyjs::hide(id = "forecastErrorText")
        shinyjs::show(id = "forecastBtn")
        shinyjs::enable(id = "forecastRadioBtn")
        shinyjs::show(id = "forecastPlot")
      }
    })
    
    observeEvent(input$forecastBtn, {
      output$forecastPlot <- renderPlot(arimaForecast(input$forecastRadioBtn, input$forecastNValue, input$forecastMValue))
      })
    
    output$showPortfolio <- renderPlot(userPortfolioOptimization(input$portfolioStocksCheckBox, input$portfolioBudget))
    
    
}

  
