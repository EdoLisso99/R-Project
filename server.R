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
    # output$prova2 <- renderText({input$stocksCheckBox})
    # output$samplingPeriodRadioBtnOut <- renderText({paste("Sampling period Selected:", paste(input$samplingPeriodRadioBtn))})
    output$simpleCCReturnsPlot <- renderPlot(plotSimpleCCReturns(input$stocksCheckBox,  input$stocksCheckBox, retreiveColors(input$stocksCheckBox)))
    output$betaPlotOut <- renderPlot({(betaPlot(input$stocksCheckBox, input$indexRadioBtn, retreiveColors(input$stocksCheckBox))) })
    output$corrDataPlotOut <- renderPlot(plotCorrelationData(input$stocksCheckBox))
    output$stockIndexPlotOut <- renderPlot(plotStockIndex(input$stocksCheckBox, input$indexRadioBtn, retreiveColors(input$stocksCheckBox)))
    output$diagnosticPlotOut <- renderPlot(showDiagnosticPlots(input$stocksCheckBox, retreiveColors(input$stocksCheckBox), retreiveSecondaryColors(input$stocksCheckBox)))
    output$correlationPairsPlotOut <- renderPlot(plotCorrelationPairs(input$stocksCheckBox))
    
    #Controllo stock singolo
    #renderText({paste(stampaProva(length(input$stocksCheckBox)))})
  }
  
