#' Auth Step 1: Initial Log In URL
#' 
#' Create URL for initial login to grant registered app permission to TD brokerage account
#' 
#' To generate the initial authorization and access the TD Ameritrade API an application 
#' needs to be registered at https://developer.tdameritrade.com/. Once an account
#' has been created, use My Apps to register an application. The two inputs needed are 
#' from the application, a Consumer Key provided by TD Ameritrade and a Callback URL 
#' which the user creates. The URL output should result in a landingpage to log into a 
#' TD Ameritrade Brokerage account.
#' 
#' The call back URL can be anything. The example below assumes the Callback URL is 
#' https://YourAppName. The Consumer Key is auto generated and can be found under 
#' My Apps > Keys. This function will use these two inputs to generate a URL where
#' the user can log in to their standard TD Ameritrade Brokerage Account and grant the
#' application access to the brokerage account, enabling the API. The Authorization 
#' Code generated at the end of the log in process will feed into auth_init_refreshToken.
#' For questions, please reference the guide at 
#' https://developer.tdameritrade.com/content/authentication-faq
#'
#' @param callbackURL User generated Callback URL associated with registered TD app 
#' @param consumerKey TD generated Consumer key associated with registered TD app
#'
#' @return login url to grant app permission to TD Brokerage account
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### Visit the URL below to log in
#' loginURL = auth_init_loginURL('https://myTDapp',
#'                               'consumerKey')
#' 
#' }
auth_init_loginURL = function(callbackURL,consumerKey){
  
  ### Generate URL specific to registered TD Application
  url = paste0('https://auth.tdameritrade.com/auth?response_type=code&redirect_uri=',callbackURL,'&client_id=',consumerKey,'%40AMER.OAUTHAP')
  return(url)
  
}




#' Auth Step 2: Obtain Initial Refresh Token
#' 
#' Get an initial Refresh Token using the Authorization code URL
#' 
#' Once a URL has been generated using auth_init_loginURL, a user can log into a 
#' TD brokerage account, granting access to the TD app. A successful log in 
#' will result in an authorization code embedded in the URL which will be fed
#' into this function. Once the button "Allow" is pressed, the user will be
#' redirected, potentially to "This site can't be reached". This indicates a
#' successful log in. The URL of the page contains the authorization code.
#' Paste the entire URL, not just the authorization code into this function.
#' The code will be an extremely long alpha numeric string.
#' The output of this function will be a refresh token which will be used
#' to gain access to the TD Brokerage account going forward. A successful 
#' submission will show "HTTP/1.1 200 OK" in the console with a verbose output.
#' The Refresh Token will last for 90 days, but you can use auth_new_refreshToken to 
#' reset the token before expiration. If the Refresh Token expires, a new
#' authorization code will need to be generated. 
#' 
#' The Refresh Token output should be saved in a very safe location, but
#' also accessible. It will be needed to generate an access token (which 
#' expire every 30 minutes) which is used in most of the functions within
#' this package.
#' 
#'
#' @param callbackURL User generated Callback URL associated with registered TD app 
#' @param consumerKey TD generated Consumer key associated with registered TD app
#' @param authcode_url Authorization URL Code generated from a successful login to the auth_init_loginURL
#'
#' @return refresh token that is valid for 90 days
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### The URL after a successful login can be fed into the function below
#' refreshToken = auth_init_refreshToken('https://myTDapp','consumerKey',
#'                                       'https://myTDapp/?code=Auhtorizationcode')
#' 
#' saveRDS(refreshToken,'/secure/location/')
#' 
#' }
auth_init_refreshToken = function(callbackURL,consumerKey,authcode_url){
  
  if(nchar(authcode_url)<500 | 
     !grepl('code=',authcode_url) | 
     substr(authcode_url,1,5) != 'https') stop(paste0('The auth code provided does not appear to be a valid auth code. ',
                                                      'Please confirm the URL link from a successful login was passed.'),
                                               call. = FALSE)
  
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
  authresponse = httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                            httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),
                            body=authreq,encode='form')
  
  ### Confirm status code of 200
  ram_status(authresponse,'. Review the TD Auth FAQ or the Auth Guide at https://developer.tdameritrade.com/ for more details')

  ### Modify Refresh token with expire times
  refreshToken = httr::content(authresponse)
  refreshToken$accessExpire = Sys.time() + refreshToken$expires_in
  refreshToken$refreshExpire = Sys.time() + refreshToken$refresh_token_expires_in
  refreshToken$createTime = Sys.time()
  
  ### Return full token sequence
  return(refreshToken)
  
}




#' Auth Step 3: Get Access Token
#' 
#' Get a new Access Token using a valid Refresh Token
#' 
#' An Access Token is required for most functions within rameritrade. 
#' It serves as a user log in for a TD Brokerage account. The token is valid for 
#' 30 minutes and allows the user to place trades, get account information,
#' get orider history, pull historical stock prices, etc. A Refresh Token is 
#' required to generate an Access Token. Functions auth_init_refreshToken or 
#' auth_new_refreshToken can be used to generate Refresh Tokens which stay valid
#' for 90 days. The Consumer Key is generated automatically when an App is 
#' registered on https://developer.tdameritrade.com/.  
#' By default, the Access Token is stored and will automatically be passed to
#' downstream function. However, the user can also submit an access token 
#' manually if multiple tokens are in use (for example: when managing more than
#' one log in.)
#' 
#'
#' @param refreshToken An existing refresh token generated using auth_init_refreshToken or auth_new_refreshToken
#' @param consumerKey TD generated Consumer key associated with registered TD app
#'
#' @return Access Token that is valid for 30 minutes. By default it is stored in options.
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### A valid refresh token can be fed into the function below for a new Access Token
#' currefreshToken = readRDS('/secure/location/')
#' accessToken = auth_new_accessToken(currefreshToken,'APPCONSUMERKEY')
#' 
#' }
auth_new_accessToken = function(refreshToken,consumerKey){
  
  ### Get default access token in environment if available
  accessToken <- getOption("td_access_token")
  
  ### If access token is not null and environment is interactive, check for expiration
  if(!is.null(accessToken) & interactive()){
    
    ### If access token has not expired, ask if new access token should be generated
    if(accessToken$expireTime>Sys.time()){
      ChkRef = utils::menu(c('Yes','No'),
                           title = paste0('Your default access token is still active, ',
                                          'are you sure you want a new Access Token?'))
      
      ### If user selects No, return default accessToken otherwise get new token
      if(ChkRef==2) return(accessToken)
    }
  }
  
  ### Validate Refresh Token
  ram_checkRefresh(refreshToken)
  
  ### Get a new access token using a refresh token
  accessreq = list(grant_type='refresh_token',
                   refresh_token=refreshToken$refresh_token,
                   client_id=consumerKey)
  
  ### Post refresh token and retrieve access token
  getaccess = httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                         httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),body=accessreq,encode='form')
  
  ### Confirm status code of 200
  ram_status(getaccess)
  
  ### Extract content and add expriation time to access token 
  accessToken = httr::content(getaccess)
  accessToken$expireTime = Sys.time() + lubridate::seconds(accessToken$expires_in) - lubridate::seconds(5)
  accessToken$createTime = Sys.time()
  
  ### Set Access Token to a default option
  options(td_access_token = accessToken)
  
  ### Return Access Token
  return(accessToken)
}




#' Auth Step 4: New Refresh Token before expiration
#' 
#' Get a new Refresh Token using an existing Refresh Token
#' 
#' A Refresh Token is used to generate an Access Token through the function
#' auth_new_accessToken. The initial Refresh Token must be generated manually 
#' using a URL specific to a registered app. Use auth_init_loginURL to generate
#' an app specific URL and then use auth_init_refreshToken to process the 
#' authorization code and generate the initial refresh token. The refresh token
#' will expire every 90 days. This function uses the current refresh token to 
#' generate a new refresh token, avoiding the manual process above. 
#' TD indicates they do look for frequent Refresh Token generation. 
#' This function should be used conservatively and as close to every 90 days as possible.
#' 
#'
#' @param refreshToken An existing refresh token generated using auth_init_refreshToken or auth_new_refreshToken
#' @param consumerKey TD generated Consumer key associated with registered TD app
#'
#' @return refresh token that is valid for 90 days
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ### A valid refresh token can be fed into the function below for a new refresh token
#' currefreshToken = readRDS('/secure/location/')
#' newrefreshToken = auth_new_refreshToken(currefreshToken,'APPCONSUMERKEY')
#' saveRDS(newrefreshToken,'/secure/location/')
#' 
#' }
auth_new_refreshToken = function(refreshToken,consumerKey){
  
  ### Validate Refresh Token
  ram_checkRefresh(refreshToken)
  
  ### Check age of refresh token
  daysUntilExp = as.numeric(as.Date(refreshToken$refreshExpire)-Sys.Date())
  
  ### If refresh token has more than 15 days check if refresh required
  if(daysUntilExp>15){
    
    ### If non-interactive and refresh token has more than 15 days, do not refresh
    if(interactive()==FALSE) return(refreshToken)
    
    ### If interactive, ask user if refresh is still needed
    ChkRef = utils::menu(c('Yes','No'),title = paste0('Your token still has ',daysUntilExp,' days until expiration.\n',
                                                      'Are you sure you want to reset your Refresh Token?'))
    
    ### If user selects no, return current refresh token
    if(ChkRef==2) return(refreshToken)
    }
  
  
  ### Get a New Refresh Token using existing refresh token before 90 day expiration
  refreshreq = list(grant_type='refresh_token',
                    refresh_token=refreshToken$refresh_token,
                    access_type='offline',
                    client_id=consumerKey)
  
  ### Post authorization request
  newrefresh = httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                          httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),
                          body=refreshreq,encode='form') 
  
  ### Confirm status code of 200
  ram_status(newrefresh,'. If the current Refresh Token has expired. Re-authenticate using the auth_init functions.')
  
  ### Modify Refresh token with expire times
  new_refreshToken = httr::content(newrefresh)
  new_refreshToken$accessExpire = Sys.time() + new_refreshToken$expires_in
  new_refreshToken$refreshExpire = Sys.time() + new_refreshToken$refresh_token_expires_in
  new_refreshToken$createTime = Sys.time()
  
  
  ### Return only the refresh token even though an access token is also provided
  return(new_refreshToken)
  
}




