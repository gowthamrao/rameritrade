#' Get a new Refresh Token using an existing Refresh Token
#' 
#' A Refresh Token is used to generate an Access Token through the function
#' td_auth_getaccess. The initial Refresh Token must be generated manually 
#' using a URL specific to a registered app. Use td_auth_initurl to generate
#' an app specific URL and then use td_auth_initrefresh to process the 
#' authorization code and generate the initial refresh token. The refresh token
#' will expire every 90 days. This function uses the current refresh token to 
#' generate a new refresh token, avoiding the manual process above. 
#' 
#'
#' @param refreshToken An existing refresh token generated using td_auth_initrefresh or td_auth_getrefresh
#' @param consumerKey TD generated Consumer key associated with registered TD app
#'
#' @return refresh token that is valid for 90 days
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### A valid refresh token can be fed into the function below for a new refresh token
#' refToken = td_auth_getrefresh('CurrentRefreshToken','CONSUMERKEY')
#' 
#' saveRDS(refToken,'/secure/location/')
#' 
#' }
td_auth_getrefresh = function(refreshToken,consumerKey,verbose_output = FALSE){
  
  
  ### Get a New Refresh Token using existing refresh token before 90 day expiration
  refreshreq = list(grant_type='refresh_token',
                    refresh_token=refreshToken,
                    access_type='offline',
                    client_id=consumerKey)
  
  ### Post authorization request
  newrefresh = if(verbose_output){
    httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
               httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),
               body=refreshreq,encode='form',httr::verbose()) } else {
    httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
               httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),
               body=refreshreq,encode='form')         
               }
  
  ### Return only the refresh token even though an access token is also provided
  return(httr::content(newrefresh)$refresh_token)
  
}