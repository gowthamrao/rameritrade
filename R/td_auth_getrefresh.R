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
#' curRefToken = readRDS('/secure/location/')
#' newRefToken = td_auth_getrefresh(curRefToken,'APPCONSUMERKEY')
#' saveRDS(newRefToken,'/secure/location/')
#' 
#' }
td_auth_getrefresh = function(refreshToken,consumerKey){
  
  
  ### Get a New Refresh Token using existing refresh token before 90 day expiration
  refreshreq = list(grant_type='refresh_token',
                    refresh_token=refreshToken,
                    access_type='offline',
                    client_id=consumerKey)
  
  ### Post authorization request
  newrefresh = httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                          httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),
                                            body=refreshreq,encode='form') 
  
  if(newrefresh$status_code==200){
    print('Successful Refresh Token Generated')
    Result = httr::content(newrefresh)$refresh_token
  }else{
    warning(paste0('Token Generation failed. Check the following reasons:\n',
                   'Confirm the Refresh Token being used is still valid and did not expire after 90 days\n',
                   'Confirm the proper Consumer Key is being used, not the callback URL\n',
                   'If this warning persists, use td_auth_initurl and td_auth_initrefresh to generate a new initial Refresh Token\n',
                   'View this functions output and the TD Auth FAQ for more details.'))
    Result = newrefresh
  }
  
  ### Return only the refresh token even though an access token is also provided
  return(Result)
  
}