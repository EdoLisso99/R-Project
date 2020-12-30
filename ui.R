title <- "Progetto Business Intelligence Lissoni Edoardo"

ui <- fluidPage(
  tags$html(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tags$body(
      tags$div(class="title",
        titlePanel(title),
      ),
      tags$div(class="navbar",
               navbarPage( 
                 title = NULL,
                 tabPanel(title="Descriptive Analytics",
                          sidebarLayout( # divides in two section the web page
                            sidebarPanel("",
                                         
                                         checkboxGroupInput(inputId ="stocksCheckBox",
                                                            label="Select stocks:", 
                                                            choices=c("MONSTER BEVERAGE" = stocks[1],
                                                                      "PEPSI" = stocks[2],
                                                                      "RYANAIR" = stocks[3],
                                                                      "ALLEGIANT TRAVEL CO." = stocks[4],
                                                                      "AMD" = stocks[5],
                                                                      "NVIDIA" = stocks[6])),
                                         
                                         radioButtons(inputId ="indexRadioBtn",
                                                      label="Select Market Index:", 
                                                      choices=c("S&P 500",
                                                                "DOW JONES",
                                                                "NASDAQ"),
                                                      selected = "NASDAQ"),
                                         
                                         dateRangeInput(inputId = "dataRangeInput", 
                                                        label = "Date Range Input", 
                                                        start = start_date+1, 
                                                        end = end_date-1),
                                         
                                         radioButtons(inputId ="samplingPeriodRadioBtn",
                                                            label="Select sampling:", 
                                                            choices=sampling,
                                                            selected = sampling[3]),
                                         
                                         ), # typically used for input from user
                            mainPanel("",
                                      textOutput(outputId = "prova"),
                                      textOutput(outputId = "prova2"),
                                      conditionalPanel(condition = "input.stocksCheckBox.length > 0",
                                        plotOutput(outputId = "simpleCCReturnsPlot"),
                                        conditionalPanel(condition = "input.stocksCheckBox.length == 1",
                                                         plotOutput(outputId = "betaPlotOut"),
                                                         plotOutput(outputId = "stockIndexPlotOut"),
                                                         plotOutput(outputId = "diagnosticPlotOut"),               
                                        ),
                                        conditionalPanel(condition = "input.stocksCheckBox.length > 1",
                                                         plotOutput(outputId = "corrDataPlotOut"),
                                                         plotOutput(outputId = "correlationPairsPlotOut"),
                                        ),
                                      ),
                            ),
                          ),
                 ),
                 tabPanel(title="Predictive Analytics"),
                 tabPanel(title="Portfolio Management"),
                 id = "subTitle", selected = FALSE,
                 position = "static-top",
                 header = NULL,
                 footer = NULL, inverse = FALSE,
                 collapsible = FALSE, fluid = FALSE,
                 theme = NULL, windowTitle = title
               ),
      ),
    ),
  ),
)
#icon("database", lib = "font-awesome") # From font-awesome library

