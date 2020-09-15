#' Get a new Access Token using a valid Refresh Token
#' 
#' An Access Token is required for most functions within rameritrade. 
#' It serves as a user log in for a TD Brokerage account. The token is valid for 
#' 30 minutes and allows the user to place trades, get account information,
#' get orider history, pull historical stock prices, etc. A Refresh Token is 
#' required to generate an Access Token. Functions td_auth_initrefresh or 
#' td_auth_getrefresh can be used to generate Refresh Tokens which stay valid
#' for 90 days. The Consumer Key is generated automatically when an App is 
#' registered on https://developer.tdameritrade.com/.  
#' 
#'
#' @param refreshToken An existing refresh token generated using td_auth_initrefresh or td_auth_getrefresh
#' @param consumerKey TD generated Consumer key associated with registered TD app
#'
#' @return Access Token that is valid for 30 minutes
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### A valid refresh token can be fed into the function below for a new Access Token
#' curRefToken = readRDS('/secure/location/')
#' accessToken = td_auth_getaccess(curRefToken,'APPCONSUMERKEY')
#' 
#' }
td_auth_getaccess = function(refreshToken,consumerKey){
  
  ### Get a new access token using an existing refresh token
  accessreq = list(grant_type='refresh_token',
                   refresh_token=refreshToken,
                   client_id=consumerKey)
  
  ### Post refresh token and retrieve access token
  getaccess = httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                         httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),body=accessreq,encode='form')
  
  ### Print Message and send result as either a successful token or the POST
  if(getaccess$status_code==200){
    print('Successful Login. Token will expire in 30 minutes')
    Result = httr::content(getaccess)$access_token
  }else{
    warning('Login Failed. Confirm Refresh Token and Consumer Key are valid.')
    Result = getaccess
    }
  
  ### Return Access Token
  return(Result)
}
