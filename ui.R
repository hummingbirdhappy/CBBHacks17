require(shiny)
require(shinythemes)
require(ggplot2)
require(pROC)
require(xts)
require(PerformanceAnalytics)
require(quantmod)
require(dygraphs)
require(httr)
require(jsonlite)
require(rjson)


shinyUI(
  navbarPage("Stretch",
    theme = shinytheme("yeti"),
      tabPanel(actionButton("home", "Home")),
        verbatimTextOutput("Welcome to Stretch"), 
            fluidRow(
              column(4,wellPanel(
                actionButton("spend", "Spend")
              )),
              column(4,wellPanel(
                actionButton("invest", "Invest")
              )),
              column(4,wellPanel(
                actionButton("give", "Give")
              ))
            ),
      tabPanel(actionButton("invest", "Invest")),
      conditionalPanel(
        condition = "input.invest",
            column(3, offset = -1,wellPanel( sliderInput("budget","What's your budget?",min=0,max=1000,value=100),
                  sliderInput("start_year","Pick an initial year:", min=2007,max=2017,value=2009),
                  selectInput("ticker1","Pick a ticker:",c('GOOG','JPM','AMZN','MS','GM','YHOO','VGSIX', 'VUSTX', 'VGTSX', 'VFISX', 'VTSMX', 'VFITX', 'VEIEX', 'VIPSX'),selected = 'VFITX',multiple=FALSE),
                  selectInput("ticker2","Pick a ticker:",c('GOOG','JPM','AMZN','MS','GM','YHOO','VGSIX', 'VUSTX', 'VGTSX', 'VFISX', 'VTSMX', 'VFITX', 'VEIEX', 'VIPSX'),selected = 'VUSTX',multiple=FALSE),
                  numericInput("itemid1","Type item ID to check for availability",175052,min=0,max=1000000,step=1,width=NULL),
                  selectInput("prodid1","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179'),selected = '33381'),
                  selectInput("prodid2","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179'),selected = '63297'),
                  selectInput("prodid3","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179'),selected = '31179'),
                  selectInput("prodid4","Select the ID for Bean product:",c('33381','116998','78819','117636','63297','31179'),selected = '78819')
            )),
            mainPanel(
              verbatimTextOutput("resultdesc"),
              h3(htmlOutput("result")),
              verbatimTextOutput("pricesdesc"),
              h3(htmlOutput("ar")),
              verbatimTextOutput("divdesc"),
              h3(htmlOutput("div")), #,
              tabsetPanel(
                tabPanel("Monthly Returns",dygraphOutput("prices")),
                tabPanel("Price and Volume",imageOutput("chart1"),imageOutput("chart2")))
            )
              ),
      tabPanel(actionButton("spend", "Spend")),
        conditionalPanel(
          condition = "input.spend",
              conditionalPanel(htmlOutput("inv1"),
                         conditionalPanel("input.budget >= output.bean1",
                                          htmlOutput("pic1"),h3(htmlOutput("bean1"))),
                         conditionalPanel("input.budget >= output.bean2",
                                          htmlOutput("pic2"),h3(htmlOutput("bean2"))),
                         conditionalPanel("input.budget >= output.bean3",
                                          htmlOutput("pic3"),h3(htmlOutput("bean3"))),
                         conditionalPanel("input.budget >= output.bean4",
                                          htmlOutput("pic4"),h3(htmlOutput("bean4")))
              )
          )
    )                 
  )