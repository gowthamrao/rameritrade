## order_place, order_search, order_cancel, order_status


### Get TD Order Details
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
      OrdrExecDet = data.frame(OrdrExec$orderActivityCollection[[1]][1:4])
      OrdrExec = dplyr::bind_rows(lapply(OrdrExec$orderActivityCollection[[1]]$executionLegs,data.frame))
      OrdrEntDet = dplyr::select(OrdrEnter,accountId,orderId,instrument.symbol,instruction,total_qty,duration,orderType,instrument.cusip,
                                 enteredTime)
      OrdrExecAll = merge(OrdrEntDet,merge(OrdrExecDet,OrdrExec))
      OrdrExecFinal = dplyr::bind_rows(OrdrExecFinal,OrdrExecAll)
    }
  
    
    orderOutput = list(enteredOrders = dplyr::as_tibble(OrderEnterFinal),
                       executedOrders = dplyr::as_tibble(OrdrExecFinal),
                       allOrderJSON = dplyr::as_tibble(jsonOrder))
    
    return(orderOutput)
}  





### To do - set for limit orders, other type of instruments
order_place = function(accountNumber,buySell = c('buy','sell'),quantity,ticker,accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Set URL specific to account
  orderURL = paste0('https://api.tdameritrade.com/v1/accounts/',accountNumber,'/orders')
  
  ### Put order details in a list
  orderList = list(orderType='MARKET',
                   session='NORMAL',
                   duration='DAY',
                   orderStrategyType='SINGLE',
                   orderLegCollection = list(list(
                     instruction=buySell,
                     quantity=quantity,
                     instrument= list(
                       symbol=ticker,
                       assetType='EQUITY'
                     )
                   ))
  )
  
  postOrder = POST(orderURL,ram_headers(accessToken),body=orderList,encode='json')

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







