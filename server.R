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

changeWindowTime <- function(start, end){
  return(paste("Start:", start, "End:", end))
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
  # output$prova <- renderText({input$indexRadioBtn})
  # output$prova2 <- renderText({input$stocksCheckBox})
  # output$dataRangeOut <- renderText({paste("Data Range Selected:", paste(input$dataRangeInput[1], "-", input$dataRangeInput[2]))})
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


