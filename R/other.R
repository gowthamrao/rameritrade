

#' Title
#'
#' @param ticker 
#' @param strikes 
#' @param inclQuote 
#' @param startDate 
#' @param endDate 
#' @param accessToken 
#'
#' @return
#' @export
#'
#' @examples
option_chain = function(ticker = 'SPY',strikes=10,inclQuote=TRUE,
                        startDate = Sys.Date(),endDate = Sys.Date() + months(12),accessToken=NULL) {
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Create URL
  optionURL = base::paste0('https://api.tdameritrade.com/v1/marketdata/chains?symbol=',ticker,
                           '&strikeCount=',strikes,
                           '&includeQuotes=',inclQuote,
                           '&fromDate=',as.Date(startDate),
                           '&toDate=',endDate)
  options =  httr::GET(optionURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(options)
  
  ### Parse Data
  jsonOptions <- httr::content(options, as = "text",encoding = 'UTF-8')
  jsonOptions <- jsonlite::fromJSON(jsonOptions)
  
  underlying = data.frame(jsonOptions$underlying)
  puts =  dplyr::bind_rows(lapply(jsonOptions$putExpDateMap,dplyr::bind_rows)) %>%
    dplyr::mutate(expireDate = Sys.Date() + lubridate::days(daysToExpiration))
  calls = dplyr::bind_rows(lapply(jsonOptions$callExpDateMap,dplyr::bind_rows)) %>%
    dplyr::mutate(expireDate = Sys.Date() + lubridate::days(daysToExpiration))
  fullChain = dplyr::bind_rows(puts,calls)
  
  returnVal = list(underlying=underlying,fullChain=fullChain)
  
  return(returnVal)
}



#' Title
#'
#' @param accountNumber 
#' @param startDate 
#' @param endDate 
#' @param transType 
#' @param accessToken 
#'
#' @return
#' @export
#'
#' @examples
transact_search = function(accountNumber,startDate=Sys.Date()-months(1),endDate=Sys.Date(),
                           transType='All',accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  transactURL = paste0('https://api.tdameritrade.com/v1/accounts/',accountNumber,
                       '/transactions?startDate=',as.Date(startDate),
                       '&endDate=',as.Date(endDate),'&type=',transType)
  searchTransact = httr::GET(transactURL,ram_headers(accessToken),encode='json')
  
  ### Confirm status code of 200
  ram_status(searchTransact)
  
  jsonTransact = httr::content(searchTransact, as = "text",encoding = 'UTF-8')
  jsonTransact <- jsonlite::fromJSON(jsonTransact)
  
  return(jsonTransact)
}



#' Title
#'
#' @param marketDate 
#' @param marketType 
#'
#' @return
#' @export
#'
#' @examples
market_hours = function(marketDate = Sys.Date(),marketType = c('EQUITY','OPTION','BOND','FUTURE','FOREX'),accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Create URL for market
  if(missing(marketType)){marketType='EQUITY'}
  marketURL = paste0('https://api.tdameritrade.com/v1/marketdata/',marketType,'/hours')
  
  ### Make Get Request using token
  marketHours = httr::GET(marketURL,ram_headers(accessToken),body=list(date = marketDate),encode='json')
  
  ### Confirm status code of 200
  ram_status(marketHours)
  
  return(httr::content(marketHours))
}


#' Title
#'
#' @param ticker 
#' @param accessToken 
#'
#' @return
#' @export
#'
#' @examples
symbol_detail = function(ticker='AAPL',accessToken=NULL) {
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  tickerURL = paste0('https://api.tdameritrade.com/v1/instruments?symbol=',ticker,'&projection=fundamental')
  
  ### Make Get Request using token
  tickerDet = httr::GET(tickerURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(tickerDet)
  
  ### Get Content
  tickCont = httr::content(tickerDet)
  if(length(tickCont)==0){stop('Ticker not valid')}
  
  Fund = data.frame(tickCont[[1]]$fundamental)
  Tick = tickCont[[1]]
  Tick$fundamental=NULL
  TickOut = merge(data.frame(Tick),Fund)
  
  return(TickOut)
}
      
