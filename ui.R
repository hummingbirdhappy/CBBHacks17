require(shiny)
require(ggplot2)
require(pROC)
require(xts)
require(PerformanceAnalytics)
require(quantmod)
require(dygraphs)
require(httr)
require(jsonlite)
require(rjson)


shinyUI(navbarPage( #logo image file goes here 
  headerPanel("Stretch"),
           titlePanel("Welcome to Stretch"),
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
           ) #end of fluidrow
  ,
#)
#  fluidPage(
#conditionalPanel() open this for the other parts of the main page

         mainPanel(
          tabsetPanel(
            tabPanel("Spend",htmlOutput("bean"))
          
        
        )  
        ),

         #sidebarLayout(
         sidebarPanel(
           sliderInput("budget","What's your budget?",min=0,max=1000,value=100),
           sliderInput("start_year","Pick an initial year", min=2007,max=2017,value=2009),
           selectInput("ticker1","Pick a ticker:",c('GOOG','JPM','AMZN','MS','GM','YHOO','VGSIX', 'VUSTX', 'VGTSX', 'VFISX', 'VTSMX', 'VFITX', 'VEIEX', 'VIPSX'),selected = 'VFITX',multiple=FALSE),
           selectInput("ticker2","Pick a ticker:",c('GOOG','JPM','AMZN','MS','GM','YHOO','VGSIX', 'VUSTX', 'VGTSX', 'VFISX', 'VTSMX', 'VFITX', 'VEIEX', 'VIPSX'),selected = 'VUSTX',multiple=FALSE),
           selectInput("ID","Select the ID for Bean product:",c('212880','286124'),selected = '212880')
         ),
         mainPanel(
           tabsetPanel(
           tabPanel("quantity",htmlOutput("result")),
           tabPanel("Dynamic Graph",dygraphOutput("prices")),
           tabPanel("ROI",htmlOutput("ar")),
           #htmlOutput("bean"),
           tabPanel("Price Chart",imageOutput("chart")),
           tabPanel("Dividends",htmlOutput("div"))
        )
        #)
    )
#tabPanel("Give")
))