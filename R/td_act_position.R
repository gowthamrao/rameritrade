#' Get Position Data associated with TD Brokerage Account
#' 
#' The output will be a list of all details related to current positions
#' for a TD Ameritrade account. Use td_act_positionDF for a cleaner output
#' in a dataframe.
#'
#' @param accessToken A valid Access Token from td_auth_getaccess
#' @param accountNumber A TD Brokerage Account associated with the Access Token
#'
#' @return a list of all account details with balance information
#' @import httr
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' ### A valid refresh token can be fed into the function below for a new Access Token
#' positions = td_act_position('accessToken','123456789')
#' 
#' }
td_act_position = function(accessToken,accountNumber){
  
  ### Create URL specific to TD Brokerage Account
  posURL = base::paste0('https://api.tdameritrade.com/v1/accounts/',accountNumber,'?fields=positions')
  
  ### Get position data using a valid accessToken
  positionData <- httr::GET(posURL,httr::add_headers(.headers=c('Content-Type'='application/json',
                                         'Authorization' = base::paste("Bearer", accessToken))),encode='json')

  ### Return Position Date
  return(httr::content(positionData))
  
}