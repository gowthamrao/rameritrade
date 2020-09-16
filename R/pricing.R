#' Get Quotes for specified tickers
#' 
#' Quotes may be delayed depending on agreement with TD Ameritrade. If the account
#' is set up for real-time quotes then this will return real-time. Otherwise the 
#' quotes will be delayed.
#'
#' @param tickers One or more tickers
#' @param accessToken A valid Access Token must be set using auth_get_access. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function
#'
#' @return a list of data with quote details for each valid ticker submitted
#' @import httr
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' ### Use a valid access token and a vector of one or more tickers
#' quoteList = td_prc_quote('accessToken',c('GOOG','TSLA'))
#' 
#' }
prc_quote_list = function(tickers = c('AAPL','MSFT'),accessToken=NULL){
  
  ### Get access token from options if one is not passed
  if (is.null(accessToken)) {
    accessToken = auth_get_accessToken()
  }
  
  ### Create URL
  quoteURL = base::paste0('https://api.tdameritrade.com/v1/marketdata/quotes?symbol=',base::paste0(tickers, collapse = '%2C'))
  quotes =  httr::GET(quoteURL,httr::add_headers('Authorization' = paste("Bearer", accessToken)))
  
  ### Print Message and send result as either the content when successful or the POST when 
  if(quotes$status_code==200){
    Result = httr::content(quotes)
  }else{
    warning('Request Failed. Confirm Access Token is valid.')
    Result = quotes
  }
  
  return(Result)
}
