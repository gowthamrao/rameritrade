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
#' loginURL = auth_init_loginURL('https://AppURL',
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
#' @param authcode_url Authorization URL Code generated from a successful login to the auth_init_loginURL
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
#' refreshToken = auth_init_loginURL('https://YourAppName/?code=Auhtorizationcode','https://YourAppName','CONSUMERKEY')
#' 
#' saveRDS(refreshToken,'/secure/location/')
#' 
#' }
auth_init_refreshToken = function(authcode_url,callbackURL,consumerKey){
  
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
  
  if(authresponse$status_code==200){
    print('Successful Refresh Token Generated')
    Result = httr::content(authresponse)$refresh_token
  }else{
    warning(paste0('Token Generation failed. Check the following reasons:\n',
                   'Confirm the authorization code follows a format of callbackURL?code=AUTHCODE\n',
                   'The authorization code can only be used once\n',
                   'The Callback URL and Consumer Key are correct\n',
                   'View this functions output and the TD Auth FAQ for more details.'))
    Result = authresponse
  }
  
  ### Return only the refresh token if successful, otherwise return full output
  return(Result)
  
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
#' @import httr
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
  
  ### Get a new access token using an existing refresh token
  accessreq = list(grant_type='refresh_token',
                   refresh_token=refreshToken,
                   client_id=consumerKey)
  
  ### Post refresh token and retrieve access token
  getaccess = httr::POST('https://api.tdameritrade.com/v1/oauth2/token', 
                         httr::add_headers('Content-Type'='application/x-www-form-urlencoded'),body=accessreq,encode='form')
  
  ### Print Message and send result as either a successful token or the POST
  if(getaccess$status_code==200){
    print('Successful Login. Token has been stored and will be valid for 30 minutes')
    Result = httr::content(getaccess)$access_token
    options(td_access_token = Result)
  }else{
    warning('Login Failed. Confirm Refresh Token and Consumer Key are valid.')
    Result = getaccess
  }
  
  ### Return Access Token
  return(Result)
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
#' 
#'
#' @param refreshToken An existing refresh token generated using auth_init_refreshToken or auth_new_refreshToken
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
#' currefreshToken = readRDS('/secure/location/')
#' newrefreshToken = auth_new_refreshToken(currefreshToken,'APPCONSUMERKEY')
#' saveRDS(newrefreshToken,'/secure/location/')
#' 
#' }
auth_new_refreshToken = function(refreshToken,consumerKey){
  
  
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
                   'If this warning persists, use auth_init_loginURL and auth_init_refreshToken to generate a new initial Refresh Token\n',
                   'View this functions output and the TD Auth FAQ for more details.'))
    Result = newrefresh
  }
  
  ### Return only the refresh token even though an access token is also provided
  return(Result)
  
}


### Get Access Token from Options
auth_get_accessToken <- function() {
   accessToken <- getOption("td_access_token")
  
  if (!is.null(accessToken)) {
    return(accessToken)
  }
  
  msg <- paste0(
    "An Access Token has not yet been set. Please use the auth_new_accessToken",
    "function, with a valid Refresh Token to create an Access Token."
  )
  warning(msg)
  
}

