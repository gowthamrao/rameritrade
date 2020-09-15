#' Get an initial Refresh Token using the Authorization code URL
#' 
#' Once a URL has been generated using td_auth_initurl, a user can log into a 
#' TD brokerage account, granting access to the TD app. A successful log in 
#' will result in an authorization code embedded in the URL which will be fed
#' into this function. Once the button "Allow" is pressed, the user will be
#' redirected, potentially to "This site can't be reached". This indicates a
#' successful log in. The URL of the page contains the authorization code.
#' Paste the entire URL, not just the authorization code into this function.
#' The code will be an extremely long alpha numeric string.
#' 
#' The output of this function will be a refresh token which will be used
#' to gain access to the TD Brokerage account going forward. A successful 
#' submission will show "HTTP/1.1 200 OK" in the console with a verbose output.
#' The Refresh Token will last for 90 days, but you can use td_auth_getrefresh to 
#' reset the token before expiration. If the Refresh Token expires, a new
#' authorization code will need to be generated. 
#' 
#' The Refresh Token output should be saved in a very safe location, but
#' also accessible. It will be needed to generate an access token (which 
#' expire every 30 minutes) which is used in most of the functions within
#' this package.
#' 
#'
#' @param authcode_url Authorization URL Code generated from a successful login to the td_auth_initurl
#' @param callbackURL User generated Callback URL associated with registered TD app 
#' @param consumerKey TD generated Consumer key associated with registered TD app
#'
#' @return refresh token that is valid for 90 days
#' @import httr urltools
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### The URL after a successful login can be fed into the function below
#' refToken = td_auth_geturl('https://YourAppName/?code=Auhtorizationcode','https://YourAppName','CONSUMERKEY')
#' 
#' saveRDS(refToken,'/secure/location/')
#' 
#' }
td_auth_initrefresh = function(authcode_url,callbackURL,consumerKey,verbose_output = FALSE){
  
  ### Parse Access Token from URL and Decode the token
  decodedtoken = urltools::url_decode(gsub('.*code=','',authcode_url))
  
  ### Get Refresh Token using URL Authorization Code and registered TD Application
  ### Access token is good for 30 minutes, the refresh token is good for 90 days
  authreq = list(grant_type='authorization_code',
                 refresh_token='',
                 access_type='offline',
                 code=decodedtoken,
                 client_id=consumerKey,
                 redirect_uri=callbackURL)
  
  ### Post authorization request either using a verbose request or non-verbose
  authresponse = if(verbose_output){
     httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),
                body=authreq,encode='form',httr::verbose()) } else {
     httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),
                body=authreq,encode='form')              
                            }
  
  ### Return only the refresh token even though an access token is also provided
  return(httr::content(authresponse)$refresh_token)
  
}
