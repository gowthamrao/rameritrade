#' Get account balances positions, or orders returned as a list
#'
#' Retrieves a list of account data for the accounts linked to the Access Token
#'
#' The output will be a list of requested details for TD Ameritrade accounts
#' linked to the access token. Use \code{\link{act_data_df}} for a cleaner
#' output in a data frame. Request can be for current positions, current
#' balances, or current day orders. For historical orders, see
#' \code{\link{order_search}}.
#'
#' @param dataType 'balances' for current cash balances, 'positions' for current
#'   account positions, 'orders' for orders entered on the current day. Default
#'   is set to 'balances'.
#' @param accessToken A valid Access Token must be set using
#'   \code{\link{auth_new_accessToken}}. The most recent Access Token will be
#'   used by default unless one is manually passed into the function.
#'
#' @return a list of requested account details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get stored refresh token
#' refreshToken = readRDS('/secure/location/')
#'
#' # Generate a new access token
#' accessToken = auth_new_accessToken(refreshToken, 'consumerKey')
#'
#' # Passing the accessToken is optional. The default will return balances
#' balances = act_data_list()
#' positions = act_data_list('positions',accessToken)
#' orders = act_data_list('orders')
#'
#' }
act_data_list = function(dataType=c('balances','positions','orders'),accessToken=NULL) {
  
  # Get access token from options if one is not passed
  accessToken = ram_accessToken(accessToken)
  
  # Check Data Type, default to balances, stop if not one of the three options passed
  if (missing(dataType)) dataType='balances'
  if (!(dataType %in% c('balances','positions','orders'))) {
    stop('dataType must be "balances", "positons", or "orders"', call. = FALSE)
  }
  
  # Set URL end based on user input
  dataTypeURL = switch(dataType,
                       'balances'='',
                       'positions'='?fields=positions',
                       'orders'='?fields=orders')
  
  # Create URL specific to TD Brokerage Account and dataType
  actURL = paste0('https://api.tdameritrade.com/v1/accounts/',dataTypeURL)
  
  # Get account data using a valid accessToken
  accountData <- httr::GET(actURL,ram_headers(accessToken))
  
  # Confirm status code of 200
  ram_status(accountData)
  
  # Return Account Data
  httr::content(accountData)
  
}

#' Get account balances, positions, or orders returned as a data frame
#'
#' Retrieves a data frame of data for the accounts linked to the Access Token
#'
#' The output will be a data frame of requested details for TD Ameritrade
#' accounts linked to the access token. Use \code{\link{act_data_list}} for an
#' output in list form. Request can be for current positions, current balances,
#' or current day orders. For historical orders, see \code{\link{order_search}}.
#'
#' @inheritParams act_data_list
#'
#' @return a data frame of requested account details
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' # Get stored refresh token
#' refreshToken = readRDS('/secure/location/')
#'
#' # Generate a new access token
#' accessToken = auth_new_accessToken(refreshToken, 'consumerKey')
#'
#' # Passing the accessToken is optional. The default will return balances
#' balances = act_data_df()
#' positions = act_data_df('positions',accessToken)
#' orders = act_data_df('orders')
#'
#' }
act_data_df = function(dataType=c('balances','positions','orders'),accessToken=NULL) {
 
  # Set values to Null to pass check()
  quantity <- accountId <- orderId <- instrument.symbol <- instruction <- NULL
  total_qty <- duration <- orderType <- instrument.cusip <- enteredTime <- NULL
  
  # Check Data Type
  if (missing(dataType)) dataType='balances'
  
  # Get Account Data in list form
  actData = act_data_list(dataType,accessToken)
  
  # Parse data depending on what dataType is
  if (dataType=='orders') {
    
    # Orders need to be parsed into entries and executions
    OrdrExecFinal=NULL
    OrderEnterFinal=NULL
    
    # Run a loop for each account associated with the access token
    for (acts in 1:length(actData)) {
    
      # Run a loop for each order within the account
      UnqOrdrs = actData[[acts]]$securitiesAccount$orderStrategies
      for (ords in 1:length(UnqOrdrs)) {
        
        # Get the high level order details and drop details for a clean data frame
        OrdrDet = UnqOrdrs[[ords]]
        OrdrDet$orderLegCollection = NULL
        OrdrDet$orderActivityCollection = NULL
        OrdrDet = data.frame(OrdrDet) %>% 
          dplyr::rename(total_qty = quantity)
        
        # Get the Entry details and merge with order details
        OrdrEnter = UnqOrdrs[[ords]]
        OrdrEnter = dplyr::bind_rows(lapply(OrdrEnter$orderLegCollection, data.frame))
        OrdrEnter = merge(OrdrEnter, OrdrDet)
        OrderEnterFinal = dplyr::bind_rows(OrderEnterFinal, OrdrEnter)
        
        # Get execution details when available
        OrdrExec = UnqOrdrs[[ords]]
        OrdrExec = dplyr::bind_rows(lapply(OrdrExec$orderActivityCollection,data.frame))
        OrdrEntDet = dplyr::select(OrdrEnter, accountId, orderId, instrument.symbol, instruction,
                                   total_qty, duration, orderType, instrument.cusip, enteredTime)
        OrdrExecAll = merge(OrdrEntDet, OrdrExec)
        OrdrExecFinal = dplyr::bind_rows(OrdrExecFinal, OrdrExecAll)
      }
    }
    
    actOutput = list(orderEntry = OrderEnterFinal, orderExecution = OrdrExecFinal)
    
  } else if (dataType=='positions') {
    
    # Pull out account and position details
    actOutput =  dplyr::bind_rows(lapply(actData, function(x) {
      # Merge account details (x) with position details (y)
      merge(x = data.frame(x$securitiesAccount)[,c(2,1,3:5)],
            # y contains the position details
            y = dplyr::bind_rows(lapply(x[[1]]$positions,data.frame)))
      }))
    
  } else {
    
    actOutput = dplyr::bind_rows(lapply(actData, function(x) {
      # Merge account details (x) with balance details (y)
      merge(x = data.frame(x$securitiesAccount)[,c(2,1,3:5)],
            # y contains the current cash balances
            y = data.frame(x[[1]]$currentBalances))
      }))
  }
  
  # Return the output from the IF function
  actOutput
  
}
