#' Get a list of data associated with TD Brokerage Accounts linked to the Access Token
#' 
#' The output will be a list of requested details for TD Ameritrade accounts 
#' linked to the access token. Use act_data_df for a cleaner output in a dataframe.
#' Request can be for current positions, current balances, or current day orders.
#'
#' @param dataType 'balances' for current cash balances, 'positions' for
#' current account positions, 'orders' for orders entered on the current day
#' @param accessToken A valid Access Token must be set using auth_new_accessToken. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function
#'
#' @return a list of requested account details
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' ### A valid refresh token can be fed into the function below for a new Access Token
#' positions = act_data_list('positions',accessToken)
#' 
#' }
act_data_list = function(dataType=c('balances','positions','orders'),accessToken=NULL){
  
  ### Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  ### Check Data Type
  if(missing(dataType)){dataType='balances'}
  dataTypeURL = switch(dataType,'balances'='','positions'='?fields=positions','orders'='?fields=orders')
  
  ### Create URL specific to TD Brokerage Account
  actURL = paste0('https://api.tdameritrade.com/v1/accounts/',dataTypeURL)
  
  ### Get position data using a valid accessToken
  accountData <- httr::GET(actURL,ram_headers(accessToken))
  
  ### Confirm status code of 200
  ram_status(accountData)
  
  ### Return Position Data
  return(httr::content(accountData))
  
}

#' Get a dataframe of data associated with TD Brokerage Accounts linked to the Access Token
#' 
#' The output will be a dataframe of requested details for TD Ameritrade accounts 
#' linked to the access token. Use act_data_list for an output in list form.
#' Request can be for current positions, current balances, or current day orders.
#'
#' @param dataType 'balances' for current cash balances, 'positions' for
#' current account positions, 'orders' for orders entered on the current day. Default is to 'balances'
#' @param accessToken A valid Access Token must be set using auth_new_accessToken. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function
#'
#' @return a dataframe of requested account details
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' ### A valid refresh token can be fed into the function below for a new Access Token
#' positions = act_data_df('positions')
#' 
#' }
act_data_df = function(dataType=c('balances','positions','orders'),accessToken=NULL){
  
  ### Get access token from options if one is not passed
  # accessToken = ram_accessToken(accessToken) # This gets run below
  
  ### Set values to Null to pass check()
  quantity <- accountId <- orderId <- instrument.symbol <- instruction <- NULL
  total_qty <- duration <- orderType <- instrument.cusip <- enteredTime <- NULL
  
  ### Check Data Type
  if(missing(dataType)){dataType='balances'}
  
  ### Get Account Data
  actData = act_data_list(dataType,accessToken)
  
  ### Parse data depending on what dataType is
  if (dataType=='orders') {
    
    ### Orders need to be parsed into entries and executions
    OrdrExecFinal=NULL
    OrderEnterFinal=NULL
    
    ### Run a loop for each account associated with the access token
    for (acts in 1:length(actData)) {
    
      ### Run a loop for each order within the account
      UnqOrdrs = actData[[acts]]$securitiesAccount$orderStrategies
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
    }
    
    actOutput = list(orderEntry = OrderEnterFinal, orderExecution = OrdrExecFinal)
    
  } else if (dataType=='positions'){
    
    ### Pull out account and position details
    actOutput =  dplyr::bind_rows(lapply(actData, function(x) {
                      merge(x=data.frame(x$securitiesAccount)[,c(2,1,3:5)],
                            ### y contains the order details
                            y=dplyr::bind_rows(lapply(x[[1]]$positions,data.frame)))
                    }))
  } else {
    
    actOutput = dplyr::bind_rows(lapply(actData, function(x) {
                  merge(x=data.frame(x$securitiesAccount)[,c(2,1,3:5)],
                        ### y contains the current cash balances
                        y=data.frame(x[[1]]$currentBalances))
                  }))
  }
  
  return(actOutput)
  
}
