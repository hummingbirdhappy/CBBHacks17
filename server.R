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


shinyServer(function(input,output) {
  decide <-reactive({
    decide <-reactiveValues(pick = NULL)
    
    observeEvent(input$spend,{
      decide$pick <-spend
    }) 
    observeEvent(input$invest,{
      decide$pick <-invest
      #show ui of invest
    })
    observeEvent(input$give,{
      decide$pick <-give
    })
    observeEvent(input$reset,{
      decide$pick <-NULL
    })
    if (decide$pick==invest)
      output$result
      output$resultdesc
      output$prices
      output$pricesdesc
      output$ar
      output$chart1
      output$chart2
      output$divdesc
    if (decide$pick==spend)
      output$inv1
      output$bean1
      output$bean2
      output$bean3
      output$bean4
      output$pic1
      output$pic2
      output$pic3
      output$pic4
  
      
  }
  )
  
  msr <- function(ticker1,ticker2,start_year) {
    
    symbol <- getSymbols(ticker1, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
    
    # Tranform it to monthly returns using the periodReturn function from quantmod
    prdata1 <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""), 
                            type = 'log')
    
    # Let's rename the column of returns to something intuitive because the column name is what
    # will eventually be displayed on the time series graph.
    colnames(prdata1) <- as.character(ticker1)
    # We want to be able to work with the xts objects that result from this function 
    # so let's explicitly put them to the global environment with an easy to use 
    # name, the stock ticker.
    assign(ticker1, prdata1, .GlobalEnv)
    symbol <- getSymbols(ticker2, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
    
    # Tranform it to monthly returns using the periodReturn function from quantmod
    prdata2 <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""), 
                            type = 'log')
    
    # Let's rename the column of returns to something intuitive because the column name is what
    # will eventually be displayed on the time series graph.
    colnames(prdata2) <- as.character(ticker2)
    # We want to be able to work with the xts objects that result from this function 
    # so let's explicitly put them to the global environment with an easy to use 
    # name, the stock ticker.
    assign(ticker2, prdata2, .GlobalEnv)
    
    mergedreturns<-merge.xts(prdata1,prdata2)
    
    dygraph(mergedreturns, main = c(ticker1,ticker2)) %>%
      dyAxis("y", label = "%") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  }
  
  
  pv1<-function(ticker1) {
    symbol1 <- getSymbols(ticker1,src='yahoo',auto.assign = FALSE, warnings=FALSE)
    chartSeries(symbol1,type="candlesticks",clev=0,name = colnames(symbol1),TA='addVo()',theme=chartTheme("black",up.col="blue",dn.col="red",multi.col = "red"),line.type="1")
  }
  
  pv2<-function(ticker2) {
    symbol2<- getSymbols(ticker2,src='yahoo',auto.assign = FALSE, warnings=FALSE)
    chartSeries(symbol2,type="candlesticks",clev=0,name =colnames(symbol2),TA='addVo()',theme=chartTheme("black",up.col="blue",dn.col="red",multi.col = "red"),line.type="1")
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
  
  beaninv1<-function(itemid1){
    u<-"https://test.api.llbean.com/v1/inventory/item/"
    p1<-itemid1
    e<-"?inventoryLocationId=60%2C40"
    product<-paste(u,p1,e,sep="")
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    substr(neat,1,100)
    neat1<-fromJSON(neat)
    neat1$properties$items$availability
  }
  
  prodprice1<-function(prodid1) {
    
    u<-"https://api.llbean.com/v1/products/"
    p1<-prodid1
    e<-"?expand=images,items,prices"
    
    product<-paste(u,p1,e,sep="")
    
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
  }
  prodprice2<-function(prodid2) {
    
    u<-"https://api.llbean.com/v1/products/"
    p2<-prodid2
    e<-"?expand=images,items,prices"
    
    product<-paste(u,p2,e,sep="")
    
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
  }
  prodprice3<-function(prodid3) {
    
    u<-"https://api.llbean.com/v1/products/"
    p3<-prodid3
    e<-"?expand=images,items,prices"
    
    product<-paste(u,p3,e,sep="")
    
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
  }
  prodprice4<-function(prodid4) {
    
    u<-"https://api.llbean.com/v1/products/"
    p4<-prodid4
    e<-"?expand=images,items,prices"
    
    product<-paste(u,p4,e,sep="")
    
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    neat1$items[[nzchar(neat1$items)]]$prices[[1]]$price
  }
  
  
  prodimg1<-function(prodid1) {

    u<-"https://api.llbean.com/v1/products/"
    p1<-prodid1
    e<-"?expand=images,items,prices"

    product<-paste(u,p1,e,sep="")

    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    baseimage<-neat1$images[[1]]$path
    width<-"?wid=200"
    paste(baseimage,width)
  }
  prodimg2<-function(prodid2) {
    u<-"https://api.llbean.com/v1/products/"
    p2<-prodid2
    e<-"?expand=images,items,prices"
    
    product<-paste(u,p2,e,sep="")
    
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    baseimage<-neat1$images[[1]]$path
    width<-"?wid=200"
    paste(baseimage,width)
  }
  prodimg3<-function(prodid3) {
    u<-"https://api.llbean.com/v1/products/"
    p3<-prodid3
    e<-"?expand=images,items,prices"
    
    product<-paste(u,p3,e,sep="")
    
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    baseimage<-neat1$images[[1]]$path
    width<-"?wid=200"
    paste(baseimage,width)
  }
  prodimg4<-function(prodid4) {
    u<-"https://api.llbean.com/v1/products/"
    p4<-prodid4
    e<-"?expand=images,items,prices"
    
    product<-paste(u,p4,e,sep="")
    
    raw <- GET(url=product, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
    names(raw)
    raw$status_code
    content(raw)
    head(raw$content)
    neat<-rawToChar(raw$content)
    neat1<-fromJSON(neat)
    baseimage<-neat1$images[[1]]$path
    width<-"?wid=200"
    paste(baseimage,width)
  }
  

  output$result<-renderText({input$budget/quo(input$ticker1)})
  output$resultdesc<-renderText("The number of shares you can buy:")
  output$prices<-renderDygraph({msr(input$ticker1,input$ticker2,input$start_year)})
  output$pricesdesc<-renderText("Future value of Dollars invested in Exchange Traded Funds:")
  output$ar<-renderText({(input$budget)*(1+ar(input$ticker1,input$start_year))})
  output$chart1<-renderPlot(pv1(input$ticker1))
  output$chart2<-renderPlot(pv2(input$ticker2))
  output$div<-renderText({input$budget*divid(input$ticker1)*4})
  output$divdesc<-renderText("Dollars you will receive as annual dividends:")

  ####
  output$inv1<-renderText({beaninv1(input$itemid1)})
  output$bean1<-renderText({prodprice1(input$prodid1)})
  output$bean2<-renderText({prodprice2(input$prodid2)})
  output$bean3<-renderText({prodprice3(input$prodid3)})
  output$bean4<-renderText({prodprice4(input$prodid4)})
  output$pic1<-renderText({c('<img src="',prodimg1(input$prodid1),'">')})
  output$pic2<-renderText({c('<img src="',prodimg2(input$prodid2),'">')})
  output$pic3<-renderText({c('<img src="',prodimg3(input$prodid3),'">')})
  output$pic4<-renderText({c('<img src="',prodimg4(input$prodid4),'">')})
}
)