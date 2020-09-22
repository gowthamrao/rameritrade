#' Get Quotes for specified tickers in List form
#'
#' Quotes may be delayed depending on agreement with TD Ameritrade. If the
#' account is set up for real-time quotes then this will return real-time.
#' Otherwise the quotes will be delayed.
#'
#' @param tickers One or more tickers
#' @inheritParams act_data_list
#'
#' @return a list of data with quote details for each valid ticker submitted
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ### Use a valid access token and a vector of one or more tickers
#' quoteList = price_quote_list(c('GOOG','TSLA'),accessToken)
#'
#' }
price_quote_list = function(tickers = c('AAPL','MSFT'),accessToken=NULL) {
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Create URL
  quoteURL = base::paste0('https://api.tdameritrade.com/v1/marketdata/quotes?symbol=',paste0(tickers, collapse = '%2C'))
  quotes =  httr::GET(quoteURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(quotes)
  
  return(httr::content(quotes))
}

#' Get Quotes for specified tickers in data frame form
#'
#' Quotes may be delayed depending on agreement with TD Ameritrade. If the
#' account is set up for real-time quotes then this will return real-time.
#' Otherwise the quotes will be delayed.
#'
#' @param tickers One or more tickers
#' @inheritParams act_data_list
#'
#' @return a data frame with quote details for each valid ticker submitted
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ### Use a valid access token and a vector of one or more tickers
#' quoteList = price_quote_df(c('GOOG','TSLA'),accessToken)
#'
#' }
price_quote_df = function(tickers = c('AAPL','MSFT'),accessToken=NULL) {
  
  ### Get access token from options if one is not passed
  # accessToken = ram_accessToken(accessToken)
  
  ### Get list of quotes
  quoteList = price_quote_list(tickers,accessToken)
  
  ### Return data frame from list
  return(dplyr::bind_rows(lapply(quoteList,data.frame)))
  
}




#' Get price history for a single security
#'
#' Pulls price history for a single security based on the parameters that
#' include a date range and frequency of the interval. Depending on the
#' frequency interval, data can only be pulled back to a certain date. For
#' example, at a one minute interval, data can only be pulled for 30-35 days
#'
#' @param ticker a single ticker
#' @param startDate the Starting point of the data
#' @param endDate the Ending point of the data
#' @param freq the frequency of the interval. Can be daily, 1min, 5min, 10min,
#'   15min, or 30min
#' @inheritParams act_data_list
#'
#' @return a tibble of price data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ### Use a valid access token and a vector of one or more tickers
#' tickHist5min = price_history_single(c('GOOG','TSLA'),freq='5min')
#'
#' }
price_history_single = function(ticker='AAPL',startDate=Sys.Date()-months(1),endDate=Sys.Date(),
                                freq=c('daily','1min','5min','10min','15min','30min'),
                                accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Set Variable to NULL to pass check()
  date_time <- volume <- NULL
  
  ### Adjust dates to support conversion to numeric time
  startDate=as.Date(startDate)+lubridate::days(1)
  endDate=as.Date(endDate)+lubridate::days(1)
  
  ### Set Variables for URL
  if(missing(freq)){freq='daily'}
  startDateMS = as.character(as.numeric(lubridate::as_datetime(startDate, tz='America/New_York'))*1000)
  endDateMS = as.character(as.numeric(lubridate::as_datetime(endDate, tz='America/New_York'))*1000)
  
  ### Set URL specific parameters
  if(freq=='daily'){
    PriceURL = paste0('https://api.tdameritrade.com/v1/marketdata/',ticker,'/pricehistory','?periodType=month&frequencyType=daily',
                      '&startDate=',startDateMS,'&endDate=',endDateMS)
  }else{
    PriceURL = paste0('https://api.tdameritrade.com/v1/marketdata/',ticker,'/pricehistory','?periodType=day&frequency=',
                      gsub('min','',freq),'&startDate=',startDateMS,'&endDate=',endDateMS)
  }
  
  
  ### Send request
  tickRequest = httr::GET(PriceURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(tickRequest)
  
  ### Extract pricing data from request
  tickHist <- httr::content(tickRequest, as = "text")
  tickHist <- jsonlite::fromJSON(tickHist)
  tickHist <- tickHist[["candles"]]
  
  ### If no data was pulled, exit the request
  if(class(tickHist)=='list') return()
  tickHist$ticker = ticker
  tickHist$date_time = lubridate::as_datetime(tickHist$datetime/1000, tz='America/New_York')
  tickHist$date = as.Date(tickHist$date_time)
  tickHist = dplyr::select(tickHist,ticker,date,date_time,open:volume)
  
  ### Return pricing data as a tibble
  return(dplyr::as_tibble(tickHist))
}  

#' Get price history for a multiple securities
#'
#' Pulls price history for a list of security based on the parameters that
#' include a date range and frequency of the interval. Depending on the
#' frequency interval, data can only be pulled back to a certain date. For
#' example, at a one minute interval, data can only be pulled for 30-35 days.
#' PLEASE NOTE: Large data requests will take time to pull back because of the
#' looping nature. TD Does not allow bulk ticker request, so this is simply
#' running each ticker individually. For faster and better historical data
#' pulls, try Tiingo or FMP Cloud
#'
#' @param tickers a vector of tickers - no more than 15 will be pulled. for
#'   bigger requests, split up the request or use Tiingo, FMP Cloud, or other
#'   free data providers
#' @inheritParams price_history_single
#'
#' @return a tibble of price data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ### Use a valid access token and a vector of one or more tickers
#' tickHistday = price_hisotry_mult(c('GOOG','TSLA'),freq='5min')
#'
#' }
price_hisotry_mult = function(tickers=c('AAPL','MSFT'),startDate=Sys.Date()-months(1),endDate=Sys.Date(),
                              freq=c('daily','1min','5min','10min','15min','30min'),
                              accessToken=NULL){
  
  ### Get access token from options if one is not passed
  # accessToken = ram_accessToken(accessToken)
  if(length(tickers)>15) {
    tickers = tickers[1:15]
    warning('More than 15 tickers submitted. Only the first 15 tickers were pulled from the list of tickers.')
  }
  
  if(missing(freq)){freq='daily'}
  
  ### Loop through all tickers and 
  allTickers = dplyr::bind_rows(lapply(tickers,function(x) 
                      price_history_single(ticker = x,startDate,endDate,freq,accessToken=accessToken)))
  return(allTickers)
}
