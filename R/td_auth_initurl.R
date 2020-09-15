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
#' Code generated at the end of the log in process will feed into td_auth_initrefresh.
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
#' loginURL = td_auth_initurl('https://YourAppName',
#'                            'CONSUMERKEY')
#' 
#' }
td_auth_initurl = function(callbackURL,consumerKey){
  
  ### Generate URL specific to registered TD Application
  url = base::paste0('https://auth.tdameritrade.com/auth?response_type=code&redirect_uri=',callbackURL,'&client_id=',consumerKey,'%40AMER.OAUTHAP')
  return(url)
  
}