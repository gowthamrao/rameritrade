# rameritrade
R package for interacting with the TD Ameritrade API. The author of this package is not in anyway associated with TD Ameritrade. The functions within this package have been tested under basic scenarios. There may be bugs or issues that could prevent you from executing trades or creates an execution that the user did not intend. The user will use this package at their own risk and not hold the author liable for any potential errors or issues encountered. 

## Authorization

Authorization to a TD Brokerage account requires a multi-step process. Once initial access is acheived, tokens can be used to maintain access. Below is a a summary of the steps required. More can be found at the TD authentication FAQ (https://developer.tdameritrade.com/content/authentication-faq). Details are also provided within the functions.
 1. Register an account with TD Ameritrade developer https://developer.tdameritrade.com/
 2. Create an app under My Apps and enter a Callback URL (for example: https://YourAppName)
 3. Use the function td_auth_initurl to generate a URL specific to the app for user log in
 4. Log in to a TD Brokerage account to grant the app access to the user account
 5. "Allow" the app access which will generate an authorization code
 6. Feed the authorization code into td_auth_initrefresh to get a refresh token
 7. The refresh token is valid for 90 days and can be used to generate an access token using td_auth_getaccess
 8. The access token will be used in all the other functions
 9. To reset the refresh token as it approaches expiration, you can follow steps 1-6 or use td_auth_getrefresh
 
## Terminology
Authorization Code - generated from the app specific URL when a TD Brokerage account logs in
Refresh Token - generated using the Authorization Code and is used to create access tokens. Refresh token is valid for 90 days
Access Token - generated using the Refresh Token and creates the connection to the API. Valid for 30 minutes.
