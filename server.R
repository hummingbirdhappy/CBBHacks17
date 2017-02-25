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


shinyServer(function(input,output) {
  
  decide <-reactiveValues(pick = NULL)
  
    observeEvent(input$spend,{
      decide$pick <-spend
    })
    observeEvent(input$invest,{
      decide$pick <-invest
    })
    observeEvent(input$give,{
      decide$pick <-give
    })
    if (is.null(decide$pick)) 
      renderText("This is the Main Page")
    if (decide$pick==spend)
      renderText("Stuff you can buy")
    if (decide$pick==invest)
      output$result<-renderText({input$budget/quo(input$ticker1)})
      output$prices<-renderDygraph({msr(input$ticker1,input$start_year)})
      output$ar<-renderText({(input$budget)*(1+ar(input$ticker1,input$start_year))})
      #output$bean<-renderText(prod(input$ID))
      output$chart<-renderPlot(pv(input$ticker1,input$ticker2))
      output$div<-renderText ({input$budget*divid(input$ticker1)*4})
    if (decide$pick==give)
      renderText("Give!")
  
      
  msr <- function(ticker1,start_year) {
    
    symbol <- getSymbols(ticker1, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
    
    # Tranform it to monthly returns using the periodReturn function from quantmod
    prdata <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""), 
                           type = 'log')
    
    # Let's rename the column of returns to something intuitive because the column name is what
    # will eventually be displayed on the time series graph.
    colnames(prdata) <- as.character(ticker1)
    # We want to be able to work with the xts objects that result from this function 
    # so let's explicitly put them to the global environment with an easy to use 
    # name, the stock ticker.
    assign(ticker1, prdata, .GlobalEnv)
    
    dygraph(prdata, main = ticker1) %>%
    dyAxis("y", label = "%") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  }
  
  pv<-function(ticker1,ticker2) {
    symbol1 <- getSymbols(ticker1,src='yahoo',auto.assign = FALSE, warnings=FALSE)
    symbol2<- getSymbols(ticker2,src='yahoo',auto.assign = FALSE, warnings=FALSE)
    chartSeries(c(symbol1,symbol2),type="candlesticks",TA='addVo()',theme=chartTheme("black",up.col="red",dn.col="blue",multi.col = "red"),line.type="1")
  }
  
  quo<-function(ticker1){
    a<-getQuote(ticker1,what=yahooQF("Previous Close"))
    a$`P. Close`
  }
  
  divid<-function(ticker1){
    d<-getDividends(ticker1,src='yahoo',from="2000-01-01",to="2017-02-02")
    e<-matrix(data=d)
    tail(e,1)
  }
  
  ar<-function(ticker1,start_year){
    symbol <- getSymbols(ticker1, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
    
    prdata <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""), 
                           type = 'log')
    colnames(prdata) <- as.character(ticker1)
    
    Return.annualized(prdata,scale=12,geometric=TRUE)
    
  }
  
  prod<-function(itemId){
    query <- "https://test.api.llbean.com/v1/inventory/item/212880?inventoryLocationId=60%2C40"
    raw <- GET(url=query, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    substr(neat,1,100)
    neat1<-fromJSON(neat)
    neat1$properties$items$itemId
  }
  
#   output$result<-renderText({input$budget/quo(input$ticker1)})
#   output$prices<-renderDygraph({msr(input$ticker1,input$start_year)})
#   output$ar<-renderText({(input$budget)*(1+ar(input$ticker1,input$start_year))})
# #  output$bean<-renderText(prod(input$ID))
#   output$chart<-renderPlot(pv(input$ticker1,input$ticker2))
#   output$div<-renderText ({input$budget*divid(input$ticker1)*4})
}
)

