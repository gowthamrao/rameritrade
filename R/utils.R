### Utilities used throughout the package

### Get Access Token from Options. No authentication, just retrieving
ram_accessToken <- function(accessToken) {
  
  if (!is.null(accessToken)) {
    return(accessToken)
  }
  
  accessToken <- getOption("td_access_token")
  
  if (!is.null(accessToken)) {
    return(accessToken)
  }
  
  stop(paste0("An Access Token has not yet been set. Please use the auth_new_accessToken",
              "function, with a valid Refresh Token to create an Access Token."))

  
}

### Check if function did not return 200
ram_status = function(x,msg=NULL){
  
    SC = x$status_code
    if (SC!=200 & SC!=201) {
      
        ErrMsg = httr::content(x)$error
        stop(paste0(SC,' - ',ErrMsg,msg), call. = FALSE)
      
    }
  
}

### Set headers for all calls with an access token
ram_headers = function(accessToken){
  httr::add_headers('Authorization' = paste("Bearer", accessToken))
}
