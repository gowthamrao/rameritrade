#' Get Options Chain
#'
#' Return a list containing two data frames. The first is the underlying 
#' data for the symbol. The second item in the list is a data from that
#' contains the options chain for the specified ticker
#'
#' @param ticker underlying ticker for the options chain
#' @param strikes the number of strikes above and below the current strike
#' @param inclQuote include pricing details (will be delayed if account is set for delayed quotes)
#' @param startDate the start date for expiration (should be greater than or equal to today). format yyyy-mm-dd
#' @param endDate the end date for expiration. format yyyy-mm-dd
#' @param accessToken A valid Access Token must be set using auth_new_accessToken. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function
#'
#' @return a list of 2 data frames - underlying and options chain
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### Pull all option contracts expiring over the next 6 months 
#' ### with 5 strikes above and below the at-the-money price
#' option_chain(ticker='SPY',strikes=5,endDate = Sys.Date() + months(6))
#' 
#' }
option_chain = function(ticker,strikes=10,inclQuote=TRUE,startDate = Sys.Date(),
                        endDate = Sys.Date() + months(12),accessToken=NULL) {
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Set value to NULL to pass check()
  daysToExpiration <- NULL
  
  ### Create URL
  optionURL = base::paste0('https://api.tdameritrade.com/v1/marketdata/chains?symbol=',ticker,
                           '&strikeCount=',strikes,
                           '&includeQuotes=',inclQuote,
                           '&fromDate=',startDate,
                           '&toDate=',endDate)
  options =  httr::GET(optionURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(options)
  
  ### Parse Data
  jsonOptions <- httr::content(options, as = "text",encoding = 'UTF-8')
  jsonOptions <- jsonlite::fromJSON(jsonOptions)
  
  underlying = data.frame(jsonOptions$underlying) %>% dplyr::as_tibble()
  puts =  dplyr::bind_rows(lapply(jsonOptions$putExpDateMap,dplyr::bind_rows)) %>%
    dplyr::mutate(expireDate = Sys.Date() + lubridate::days(daysToExpiration))
  calls = dplyr::bind_rows(lapply(jsonOptions$callExpDateMap,dplyr::bind_rows)) %>%
    dplyr::mutate(expireDate = Sys.Date() + lubridate::days(daysToExpiration))
  fullChain = dplyr::bind_rows(puts,calls) %>% dplyr::as_tibble()
  
  returnVal = list(underlying=underlying,fullChain=fullChain)
  
  return(returnVal)
}



#' Search for all Transaction types
#' 
#' Can pull trades as well as transfers, dividend reinvestment, interest, etc.
#' Any activity assoicated with the account.
#'
#' @param accountNumber The account number associated with the Access Token
#' @param startDate Transactions after a certain date. Will not pull back transactions older than 1 year. format yyyy-mm-dd
#' @param endDate Filter transactions that occurred before a certain date. format yyyy-mm-dd
#' @param transType Filter for a specific Transaction type. No entry will return all types.
#' For example: TRADE, CASH_IN_OR_CASH_OUT, CHECKING, DIVIDEND, INTEREST, OTHER
#' @param accessToken A valid Access Token must be set using auth_new_accessToken. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function 
#'
#' @return a jsonlite data frame of transactions
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### Transactions for the last 5 days
#' transact_search(accountNumber=987654321, startDate = Sys.Date()-days(5))
#' 
#' }
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



#' Get Market Hours
#'
#' Returns a list output for a specified day and market that details the trading window for that day
#'
#' @param marketDate The market date to pull details for
#' @param marketType The asset class for hour: 'EQUITY','OPTION','BOND','FUTURE','FOREX'
#' @param accessToken A valid Access Token must be set using auth_new_accessToken. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function
#'
#' @return List output of times and if the specified date is a trading day
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### Market hours for the current date
#' market_hours()
#' 
#' }
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


#' Get ticker details
#' 
#' Get identifiers and fundamental data for a specific ticker
#'
#' @param ticker a valid ticker or symbol
#' @param accessToken A valid Access Token must be set using auth_new_accessToken. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function
#'
#' @return data frame of ticker details
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### details for Apple
#' symbol_detail('AAPL')
#' 
#' }
symbol_detail = function(ticker,accessToken=NULL) {
  
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
  TickOut = merge(data.frame(Tick),Fund) %>% dplyr::as_tibble()
  
  return(TickOut)
}
      
