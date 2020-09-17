


### Get TD Order Details
#' Title
#'
#' @param orderId 
#' @param accountNumber 
#' @param accessToken 
#'
#' @return
#' @export
#'
#' @examples
order_detail = function(orderId,accountNumber,accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Get Order Details
  orderURL = paste0('https://api.tdameritrade.com/v1/accounts/',accountNumber,'/orders/',orderId)
  orderDetails = httr::GET(orderURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(orderDetails)
  
  return(content(orderDetails))

}

### Cancel Ordeer
#' Title
#'
#' @param orderId 
#' @param accountNumber 
#' @param accessToken 
#'
#' @return
#' @export
#'
#' @examples
order_cancel =  function(orderId,accountNumber,accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Get Order Details
  orderURL = paste0('https://api.tdameritrade.com/v1/accounts/',accountNumber,'/orders/',orderId)
  orderCancel = httr::DELETE(orderURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(orderCancel)
  
  print('Order Cancelled')
  return(paste0(orderCancel$status_code,'- ',orderCancel$url))
  
}


### Get TD Order into a DF
#' Title
#'
#' @param accountNumber 
#' @param startDate 
#' @param endDate 
#' @param maxResults 
#' @param orderStatus 
#' @param accessToken 
#'
#' @return
#' @export
#'
#' @examples
order_search = function(accountNumber,startDate=Sys.Date()-months(1),endDate=Sys.Date(),
                        maxResults=50,orderStatus='',accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  searchURL = paste0('https://api.tdameritrade.com/v1/orders?accountId=',accountNumber,
                     '&maxResults=',maxResults,'&status=',orderStatus,
                     '&fromEnteredTime=',startDate,'&toEnteredTime=',endDate)
  searchOrders = httr::GET(searchURL,ram_headers(accessToken),encode='json')
 
  
  ### Confirm status code of 200
  ram_status(searchOrders)
  
  ### Bind variables to quiet warning
  accountId <- orderId <- instrument.symbol <- instruction <- total_qty <- duration <- orderType <- instrument.cusip <- enteredTime <- NULL
  
  jsonOrder <- httr::content(searchOrders, as = "text",encoding = 'UTF-8')
  jsonOrder <- jsonlite::fromJSON(jsonOrder)
  
  
  OrdrExecFinal=NULL
  OrderEnterFinal=NULL
    ### Run a loop for each order within the account
    UnqOrdrs = content(searchOrders)
    for(ords in 1:length(UnqOrdrs)) {
      
      ### Get the high level order details
      OrdrDet = UnqOrdrs[[ords]]
      OrdrDet$orderLegCollection=NULL
      OrdrDet$orderActivityCollection=NULL
      OrdrDet = data.frame(OrdrDet) %>% dplyr::rename(total_qty=quantity)
      
      ### Get the Entry details and merge with order details
      OrdrEnter = UnqOrdrs[[ords]]
      OrdrEnter = dplyr::bind_rows(lapply(OrdrEnter$orderLegCollection,data.frame))
      OrdrEnter = merge(OrdrEnter,OrdrDet)
      OrderEnterFinal = dplyr::bind_rows(OrderEnterFinal,OrdrEnter)
      
      ### Get execution details when available
      OrdrExec = UnqOrdrs[[ords]]
      OrdrExec = dplyr::bind_rows(lapply(OrdrExec$orderActivityCollection,data.frame))
      OrdrEntDet = dplyr::select(OrdrEnter,accountId,orderId,instrument.symbol,instruction,total_qty,duration,orderType,instrument.cusip,
                                 enteredTime)
      OrdrExecAll = merge(OrdrEntDet,OrdrExec)
      OrdrExecFinal = dplyr::bind_rows(OrdrExecFinal,OrdrExecAll)
    }
  
    
    orderOutput = list(enteredOrders = dplyr::as_tibble(OrderEnterFinal),
                       executedOrders = dplyr::as_tibble(OrdrExecFinal),
                       allOrderJSON = dplyr::as_tibble(jsonOrder))
    
    return(orderOutput)
}  




### This order function does not capture all the capabilites of the TD order entry
## It is designed to be a simple single entry order function. For more complex
# stratetgies, please consult the order guide on TDs website
### To do - set for limit orders, other type of instruments


#' Place Order for a specific account
#' 
#' Place trades through the TD Ameritrade API using a range of parameters.
#' A valid account and access token must be passed. An access token will 
#' be passed by default when auth_new_accessToken is executed successfully
#' and the token has not expired, which occurs after 30 minutes.
#' This function is built to allow a single trade submission. More complex 
#' trades can be executed through the API, but a custom function or submission
#' will need to be constructed. See details below. Only equities and options
#' can be traded at this time. 
#' 
#' To build more custom trading strategies, reference the TD Ameritrade
#' API Instructions (https://developer.tdameritrade.com/account-access/apis)
#' or referencing the order sample guide 
#' (https://developer.tdameritrade.com/content/place-order-samples). A full
#' list of the input parameters and details can be found at the links above.
#' Please note that in rare cases, the documentation may not be accurate in the
#' API section, so the Order Sample guide is a better refernce.
#'   
#'
#' @param accountNumber a valid TD Ameritrade brokerage account number linked to the Access Token
#' @param ticker a valid Equity/ETF or option. Use symbol_detail to confirm
#' @param quantity the number of shares to be bought or sold
#' @param instruction Equity instructions include buy, sell, buy_to_cover, sell_short
#' Option instructions include buy_to_open
#' @param orderType MARKET, LIMIT (requiring limitPrice), STOP (requiring stopPrice),
#' STOP_LIMIT, TRAILING_STOP (requiring stopPriceBasis, stopPriceType, stopPriceOffset)
#' @param limitPrice the limit price for a limit or stop_limit order
#' @param stopPrice the stop price for a STOP or STOP_LIMIT order
#' @param assetType Equity or Option. No other asset types are available at this time
#' @param session Normal for normal market hours, AM or PM for extended market hours
#' @param duration how long will the trade stay open without a fill: 
#' DAY, GOOD_UNTIL_CANCEL, FILL_OR_KILL 
#' @param stopPriceBasis the basis for a STOP, STOP_LIMIT, or TRAILING_STOP which include
#' LAST, BID, ASK
#' @param stopPriceType the link to the stopPriceBasis. VALUE for dollar difference or
#' PERCENT for a percentage offset from the price basis
#' @param stopPriceOffset the offset used for the stopPriceType, 10 and PERCENT is a 10%
#' offset from the current price basis. 5 and value is a $5 offset from the current price basis
#' @param accessToken A valid Access Token must be set using auth_new_accessToken. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function. The Access Token must be linked to the accountNumber
#'
#' @return the trade id, account id, and other basic details
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' accountNumber = 1234567890
#' 
#' ### Standard market buy order
#' order_place(accountNumber = accountNumber,ticker='AAPL',quantity = 1,instruction='buy')
#' 
#' ### Stop limit order
#' order_place(accountNumber = accountNumber,ticker='AAPL',quantity = 1,instruction='sell',duration='good_till_cancel',
#'             orderType = 'stop_limit',limitPrice=98,stopPrice=100)
#'             
#' ### Trailing Stop Order
#' order_place(accountNumber = accountNumber,ticker='AAPL',quantity = 1,instruction='sell',
#'             orderType = 'trailing_stop',stopPriceBasis = 'BID',stopPriceType = 'percent',stopPriceOffset = 10)
#'             
#' ### Option Order with a market price
#' order_place(accountNumber = accountNumber,ticker='SPY_092120P334',quantity = 1,
#'             instruction='buy_to_open',duration='Day',assetType = 'OPTION')
#' 
#' }
#' 
#' 
order_place = function(accountNumber,ticker,quantity,instruction,
                       orderType='MARKET',limitPrice=NULL,stopPrice=NULL,
                       assetType=c('EQUITY','OPTION'),session='NORMAL',duration='DAY',
                       stopPriceBasis=NULL,stopPriceType=NULL,stopPriceOffset=NULL,
                       accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Check symbol and asset type
  if (missing(assetType)) assetType ='EQUITY'
  
  ### Set URL specific to account
  orderURL = paste0('https://api.tdameritrade.com/v1/accounts/',accountNumber,'/orders')
  
  ### Put order details in a list
  orderList = list(orderType=orderType,
                   complexOrderStrategyType='NONE',
                   session=session,
                   duration=duration,
                   price=limitPrice,
                   stopPrice=stopPrice,
                   orderStrategyType='SINGLE',
                   stopPriceLinkBasis=toupper(stopPriceBasis),
                   stopPriceLinkType=toupper(stopPriceType),
                   stopPriceOffset=stopPriceOffset,
                   orderLegCollection = list(list(
                     instruction=instruction,
                     quantity=quantity,
                     instrument= list(
                       symbol=ticker,
                       assetType=assetType
                     )
                   ))
  )
  
  
  postOrder = httr::POST(orderURL,ram_headers(accessToken),body=orderList,encode='json')
  
  ### Confirm status code of 201
  ram_status(postOrder)
  
  orderDet = postOrder$headers
  orderOutput = data.frame(
    accountNumber=gsub('/orders/.*','',gsub('.*accounts/','',orderDet$location)),
    orderId=gsub('.*orders/','',orderDet$location),
    status_code=postOrder$status_code,
    date = orderDet$date,
    location = orderDet$location
  )
  
  return(orderOutput)
}







