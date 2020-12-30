rm(list=ls())
graphics.off()
dev.off()
cat("\014")
library(shiny)
library(forecast)
library(quantmod)
library(dygraphs)
library(PerformanceAnalytics)
library(lubridate)
library(tseries)
library(corrplot)



#Declare variables
stocks <- c("MNST", "PEP", "RYAAY", "ALGT", "AMD", "NVDA")
sampling <- c("daily", "weekly", "monthly", "quaterly", "yearly")
marketIndex <- c("^GSPC", "^DJI", "^IXIC")
shortColNames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
start_date <- as.Date("2010-09-30")
end_date <- as.Date("2020-11-01")
arrayColors <- c("goldenrod", "deeppink","darkturquoise","darkorange",
                 "royalblue1", "slateblue2")
complementaryColors <- c("red","cyan3","orangered","springgreen3","gold",
                         "yellow3")
#Import stocks and index data from Yahoo! Finance
getSymbols(Symbols = stocks[1:5], from=start_date, to=end_date, src='yahoo')
getSymbols(Symbols = c(stocks[6], marketIndex[1:length(marketIndex)]),
           from=start_date, to=end_date, src='yahoo')

#Monthly 
MNSTMonth <- to.monthly(MNST)
PEPMonth <- to.monthly(PEP)
RYAAYMonth <- to.monthly(RYAAY)
ALGTMonth <- to.monthly(ALGT)
AMDMonth <- to.monthly(AMD)
NVDAMonth <- to.monthly(NVDA)
GSPCMonth <- to.monthly(GSPC)
DJIMonth <- to.monthly(DJI)
IXICMonth <- to.monthly(IXIC)

#Change column names
colnames(MNSTMonth) <- shortColNames
colnames(PEPMonth) <- shortColNames
colnames(RYAAYMonth) <- shortColNames
colnames(ALGTMonth) <- shortColNames
colnames(AMDMonth) <- shortColNames
colnames(NVDAMonth) <- shortColNames
colnames(GSPCMonth) <- shortColNames
colnames(DJIMonth) <- shortColNames
colnames(IXICMonth) <- shortColNames

#Pick Adjusted column
MNSTMonthAdj <- MNSTMonth$Adjusted
PEPMonthAdj <- PEPMonth$Adjusted
RYAAYMonthAdj <- RYAAYMonth$Adjusted
ALGTMonthAdj <- ALGTMonth$Adjusted
AMDMonthAdj <- AMDMonth$Adjusted
NVDAMonthAdj <- NVDAMonth$Adjusted
GSPCMonthAdj <- GSPCMonth$Adjusted
DJIMonthAdj <- DJIMonth$Adjusted
IXICMonthAdj <- IXICMonth$Adjusted

#Rename column names
colnames(MNSTMonthAdj) <- stocks[1]
colnames(PEPMonthAdj) <- stocks[2]
colnames(RYAAYMonthAdj) <- stocks[3]
colnames(ALGTMonthAdj) <- stocks[4]
colnames(AMDMonthAdj) <- stocks[5]
colnames(NVDAMonthAdj) <- stocks[6]
colnames(GSPCMonthAdj) <- marketIndex[1]
colnames(DJIMonthAdj) <- marketIndex[2]
colnames(IXICMonthAdj) <- marketIndex[3]

MNST.r <- periodReturn( x=MNST, period="yearly", subset=paste0(start_date+1,"/",end_date-1) )
PEP.r <- periodReturn( x=PEP, period="yearly", subset=paste0(start_date+1,"/",end_date-1) )
RYAAY.r <- periodReturn( x=RYAAY, period="yearly", subset=paste0(start_date+1,"/",end_date-1) )
ALGT.r <- periodReturn( x=ALGT, period="yearly", subset=paste0(start_date+1,"/",end_date-1) )
AMD.r <- periodReturn( x=AMD, period="yearly", subset=paste0(start_date+1,"/",end_date-1) )
NVDA.r <- periodReturn( x=NVDA, period="yearly", subset=paste0(start_date+1,"/",end_date-1) )

#Merge only Adjusted column of all stocks and index in one variable
mergedStocksAdj <- merge(MNST$MNST.Adjusted, PEP$PEP.Adjusted,
                         RYAAY$RYAAY.Adjusted, ALGT$ALGT.Adjusted,
                         AMD$AMD.Adjusted, NVDA$NVDA.Adjusted)

mergedStocksAdjMonth <- merge(MNSTMonthAdj,PEPMonthAdj, RYAAYMonthAdj,
                              ALGTMonthAdj, AMDMonthAdj, NVDAMonthAdj)
mergedIndexAdjMonth <- merge(GSPCMonthAdj, DJIMonthAdj, IXICMonthAdj)

simpleStocksReturnsYear <- merge(MNST.r, PEP.r, RYAAY.r, ALGT.r, AMD.r, NVDA.r)

colnames(simpleStocksReturnsYear) <- colnames(mergedStocksAdj) <- stocks

#Compute simple and continuous compound return, omitting NA values
simpleStocksReturns <- na.omit(CalculateReturns(mergedStocksAdjMonth,
                                                method = "simple"))
ccStocksReturns <- na.omit(CalculateReturns(mergedStocksAdjMonth,
                                            method = "compound"))
simpleIndexReturns <- na.omit(CalculateReturns(mergedIndexAdjMonth,
                                               method = "simple"))
ccIndexReturns <- na.omit(CalculateReturns(mergedIndexAdjMonth,
                                           method = "compound"))

colnames(ccIndexReturns) <- colnames(mergedIndexAdjMonth)<- 
  colnames(simpleIndexReturns)<- c("S&P 500", "DOW JONES", "NASDAQ")

#Plot all the data in a single graph
plotSimpleCCReturns <- function(simple, cc, lineColor){
  simple <- simpleStocksReturns[,simple]
  ccReturns <- ccStocksReturns[,cc]
  layout(matrix(c(1,2,3,3), ncol=1, byrow=TRUE), heights=c(8, 7, 1))
  simple <- plot(simple, main=paste("Simple returns of all stocks"),
                 col = lineColor)
  cc <- plot(ccReturns, main=paste("Continuous compound returns of all stocks"),
             col = lineColor)
  print(simple)
  print(cc)
  par(mai=rep(0,4))
  plot.new()
  legend(x="center", ncol=3, fill = lineColor, legend=colnames(ccReturns), title="Stocks")
}

#Plot the CC monthly return in only one graph
plotCCReturns <- function(ccMonth){
  dygraph(ccMonth) %>%
    dyOptions(stackedGraph = FALSE, drawAxesAtZero = TRUE, axisLineColor = "red",
              axisTickSize = 5) %>%
    dyRangeSelector(height = 40)%>%
    dyLegend(show = "always", hideOnMouseOut = TRUE)#%>%
  # dyCSS("./www/dygraph.css")
}

#Covariance of index
printCovariance <- function(stockData){
  covarianze <- cov(cbind(stockData[,1:dim(stockData)[2]]))
  print("Covarianze:")
  print(covarianze)
}

#Correlation of index
plotCorrelationData <- function(stockData){
  stockData <- ccStocksReturns[,stockData]
  correlazioni <- cor(cbind(stockData[,1:dim(stockData)[2]]))
  # print("Correlazioni:")
  # print(correlazioni)
  par(mfrow=c(1,1))
  corrplot(correlazioni, method="number", col=c("red", "tomato", "blue",
                                                "royalblue3", "darkgreen", "limegreen"))
}

plotStockIndex <- function(stockData, indexData, lineColor){
  stockData <- ccStocksReturns[,stockData]
  indexData <- ccIndexReturns[, indexData]
  allData <- merge(stockData, indexData)
  names <- c(colnames(stockData)[1], colnames(indexData)[1])
  layout(matrix(c(1,2), ncol=1, byrow=TRUE), heights=c(8, 2))
  graph <- plot(allData, main = "Stock and index comparison", col = c(lineColor, "black"))
  print(graph)
  par(mai=rep(0,4))
  plot.new()
  legend(x="center", ncol=3, c(colnames(stockData), legend=colnames(indexData)[1]),
         fill = c(lineColor, "black"), title = "Data")
  # dygraph(allData)%>%
  #   dyRangeSelector(height = 40)
}

# Beta function to calculate beta value
beta_function <- function(stock, market_index){
  beta <- cov(stock, market_index)/var(market_index)
  return(beta)
}

betaPlot <- function(stock, marketIndex, lineColor){
  stock <- as.zoo(ccStocksReturns[,stock])
  marketIndex <- as.zoo(ccIndexReturns[,marketIndex])
  stock_betas <- NULL # time series to save beta's values
  length_period = (interval(start_date, end_date) %/% months(1)) # move time windows for beta value
  delta_t <- 60
  for (i in (delta_t+1):length_period){
    # Beta value
    beta_val <- beta_function(stock[(i-delta_t):(i)], marketIndex[(i-delta_t):(i)])
    # create a time series of one beta values for each stock
    beta_xts <- as.xts(beta_val, order.by = index(stock[i]))
    # Create a time series of beta for each stock 
    if(is.null(stock_betas)){
      stock_betas <- beta_xts
    }else{
      stock_betas <- rbind(stock_betas, beta_xts)
    }
  }
  print(paste("Media:",mean(na.omit(stock_betas))))
  stock_betas <- as.xts(c(rep(NA,delta_t), as.numeric(stock_betas)), order.by = index(stock))
  stock_betas_window <- window(stock_betas,
                               start=(start_date+floor(round(delta_t*30.41, 0))), end=end_date)
  par(mfrow=c(1,1))
  grafico <- plot(stock_betas_window, main = paste(colnames(stock)[1], "Betas"),
       col = lineColor, xlab = "Date", ylab = "Beta Value")
  print(grafico)
  # grafico <- (dygraph(stock_betas_window)%>%
  #               dyRangeSelector(height = 40)%>%
  #               dySeries("V1", label = paste(colnames(stock)[1], "BETA")) %>%
  #               dyLegend(show = "always", hideOnMouseOut = TRUE))
  # 
  # print(grafico)
}


getAllStats <- function(data){
  allStats <- matrix(NA, nrow=10, ncol=6)
  for(i in 1:dim(data)[2]){
    allStats[1,i] = mean(data[,i])
    allStats[2,i] = var(data[,i])[1]
    allStats[3,i] = sd(data[,i])
    allStats[4,i] = skewness(data[,i])
    allStats[5,i] = kurtosis(data[,i])
    for(j in 1:5){
      allStats[5+j,i] <- c(quantile(data[,i])[j])
    }
  }
  
  rownames(allStats) <- c("Mean", "Variance", "Standard Deviation", "Skewness",
                          "Kurtosis", "0%", "25%", "50%", "75%", "100%")
  colnames(allStats) <- stocks
  return(allStats)
}

#Compute all descriptive statistics into one matrix
#Mean,Variance,Standard deviation, Skewness, Kurtosis and Quantile
printUnivariateStatistics <- function(stockData){
  print(getAllStats(stockData)[1:5,])
}

#Plot all information
showDiagnosticPlots <- function(stockData, primaryLineColors, secondaryLineColors){
  stockData <- ccStocksReturns[,stockData]
  allStats <- getAllStats(stockData)
  
  for(i in 1:dim(stockData)[2]){
    layout(matrix(c(1,2,3,4), ncol=2, nrow = 2), heights = c(8,7))
    par(mai=rep(0.47,4))
  
    #Histogram
    hist(stockData[,i], xlab = "Compound Returns", ylab = "Distribution",
         main=paste("Distribution of", colnames(stockData)[i], "CC Returns"),
         col = primaryLineColors[i])
    for(j in (dim(allStats)[1]-5):dim(allStats)[1]){
      abline(v=allStats[(dim(allStats)[1]-5):dim(allStats)[1],i],
             col=secondaryLineColors[i],
             lwd=c(1,1.5,2,1.5,1)[j-5])
    }
    
    #Density
    plot(density(stockData[, i]), type = "l", col = primaryLineColors[i],
         main = paste("Smoothed Density of", colnames(stockData)[i], "CC Returns" ))
    
    #QQ Plots
    qqnorm(stockData[,i], main=paste("Quantiles of", colnames(stockData)[i]),
           col=primaryLineColors[i], xlab = "Theorical quantiles",
           ylab = "Sample quantiles")
    qqline(stockData[,i], col = primaryLineColors[i])
    
    #BoxPlots
    boxplot(as.numeric(stockData[,i]), main = paste(colnames(stockData)[i], "CC Return"),
            col = primaryLineColors[i], outline = TRUE)
    abline( h=0.0, lty=2, lwd=2, col=secondaryLineColors[i] )
    # chart.Boxplot(ccStocksReturns[,i] , outlier.symbol = "O", ylab = stocks[i],
    #               main = paste(stocks[i], " CC Return"), xlab = "Returns")
  }  
}

#Plot correlation of all stocks
plotCorrelationPairs <- function(stockData){
  stockData <- ccStocksReturns[,stockData]
  stocksCorrelation <- matrix(NA, nrow=dim(stockData)[1], ncol=dim(stockData)[2])
  colnames(stocksCorrelation) <- colnames(stockData)
  for(i in 1:dim(stockData)[2]){
    stocksCorrelation[,i] <- as.numeric(stockData[,i])
  }
  pairs(cbind(stocksCorrelation[,1:dim(stockData)[2]]),  
        col="dodgerblue2", main="Correlation of ALL STOCKS", )
} 

#Forecasting
arimaForecast <- function(stock, trainingSet, testSet){
  stock <- as.zoo(stock)
  returnsTrain <- as.zoo(stock[1:trainingSet])  # Train dataset 90%
  returnsTest <- as.zoo(stock[(trainingSet+1):(trainingSet+testSet)])   # Test dataset 10%
  fit <- arima(returnsTrain, order = c(13,0,15))
  arma_forecast <- forecast(fit, h = testSet, level = c(95,80))
  par(mfrow=c(1,1))
  plot(arma_forecast, main = paste("ARMA forecasts for", colnames(stock),  "returns"))
  legend("topleft", c("Real","Predict"), col=c("black", "deepskyblue3"), lty=1, lwd = 3 ,title = "Data")
  lines(returnsTest)
  print("Accuracy:")
  print(accuracy(arma_forecast, returnsTest) )
  print(accuracy(arma_forecast, returnsTest)[2])
}


# for(i in 1:length(stocks)){
#   arimaForecast(ccStocksReturns[,i], 80, 30)
# }

# Portfolio Optimization 
userPortfolioOptimization <- function(stockReturns, budget, dateEnd, stockAdj){
  # Markowitz optimal portfolio con impostazioni di default
  Mop <- portfolio.optim( x=stockReturns)
  # costruiamo la frontiera efficiente e poniamo Mop su essa
  rs <- seq(0.0,1.0,length.out=250)
  risk <- numeric(length(rs))+NA
  for( i in 1:length(rs) ) {
    p <- NULL
    try( p <- portfolio.optim( x=stockReturns, pm=rs[i], shorts = FALSE) )
    if( is.null(p) ) {
      risk[i] <- NA
    } else {
      risk[i] <- p$ps
    }
  }
  par(mfrow=c(1,1))
  par(mar = c(5, 5, 2, 2))
  plot( risk, rs, pch=20, col="blue", xlab="Risk (sigma)", ylab="Return (mean)"
        , main = "Risk-Mean plane", )
  points( Mop$ps, Mop$pm, pch=17, col="red" )
  shares <- spesa <- c(rep(NA, (dim(stockReturns)[2])))
  investimento <- 0
  for(i in 1:(dim(stockReturns)[2])){
    shares[i] <- round(floor((budget * Mop$pw[i])/as.numeric(stockAdj[dateEnd, i])),3)
    spesa[i] <- round(shares[i] * as.numeric(stockAdj[dateEnd, i]), 3)
    investimento <- round((investimento + spesa[i]),3)
    print(paste(colnames(stockReturns)[i], ":", shares[i], "quote a:",
                as.numeric(stockAdj[dateEnd, i]), "$ con una spesa di:", spesa[i]))
    print(paste("Il peso di", colnames(stockReturns)[i], "nel portafoglio è di:",
                round(Mop$pw[i], 3)))
    print("------------------------------------------------------------------")
  }
  print(paste("Il ritorno del portafoglio (ideale) è del:", round((Mop$pm*100),3), 
              "%, con il", round((Mop$ps*100),3), "% di rischio"))
  print(paste("Il ritorno del portafoglio (reale) è del:", round((Mop$pm*99),3), 
              "%, con il", round((Mop$ps*100),3), "% di rischio"))
  
}

source("./server.R")
source("./ui.R")
shinyApp(ui = ui, server = server)
