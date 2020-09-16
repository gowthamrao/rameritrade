#' Get Position Data associated with TD Brokerage Account
#' 
#' The output will be a list of all details related to current positions
#' for a TD Ameritrade account. Use act_position_df for a cleaner output
#' in a dataframe.
#'
#' @param accountNumber A TD Brokerage Account associated with the Access Token
#' @param accessToken A valid Access Token must be set using auth_get_access. 
#' The most recent access token will be used by default unless one is manually
#' passed into the function
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
act_position_list = function(accountNumber,accessToken=NULL){
  
  ### Get access token from options if one is not passed
  if (is.null(accessToken)) {
    accessToken = auth_get_accessToken()
    }
  
  ### Create URL specific to TD Brokerage Account
  posURL = paste0('https://api.tdameritrade.com/v1/accounts/',accountNumber,'?fields=positions')
  
  ### Get position data using a valid accessToken
  positionData <- httr::GET(posURL,httr::add_headers(.headers=c('Content-Type'='application/json',
                                         'Authorization' = paste("Bearer", accessToken))),encode='json')
  
  ### Print Message and send result as either a successful token or the POST
  if(positionData$status_code==200){
    Result = httr::content(positionData)
  }else{
    warning('Request Failed. Confirm Access Token and Account Number are valid.')
    Result = positionData
  }

  ### Return Position Date
  return(Result)
  
}