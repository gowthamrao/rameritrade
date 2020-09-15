#' Get Quotes for specified tickers
#' 
#' Quotes may be delayed depending on agreement with TD Ameritrade. If the account
#' is set up for real-time quotes then this will return real-time. Otherwise the 
#' quotes will be delayed.
#'
#' @param accessToken A valid Access Token from td_auth_getaccess
#' @param tickers One or more tickers
#'
#' @return a list of data with quote details for each valid ticker submitted
#' @import httr
#' @export
#' 
#' @example 
#' \dontrun{
#' 
#' ### Use a valid access token and a vector of one or more tickers
#' quoteList = td_prc_quote('accessToken',c('GOOG','TSLA'))
#' 
#' }
td_prc_quote = function(accessToken,tickers = c('AAPL','MSFT')){
  
  quoteURL = base::paste0('https://api.tdameritrade.com/v1/marketdata/quotes?symbol=',base::paste0(tickers, collapse = '%2C'))
  quotes =  httr::GET(quoteURL,httr::add_headers('Authorization' = base::paste("Bearer", accessToken)))
  
  return(httr::content(quotes))
}
