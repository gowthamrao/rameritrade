
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rameritrade

<!-- badges: start -->

<!-- badges: end -->

R package for interacting with the TD Ameritrade API. A TD Ameritrade
brokerage account and a TD Ameritrade Developer app are required. See
Authentication for more details. Due to the Charles Schwab acquisition
of TD Ameritrade, the fate of this API is unknown. This package could
stop working at any point if the acquisition results in the termination
of the API.

The author of this package is not in anyway associated with TD
Ameritrade. The functions within this package have been tested under
basic scenarios. There may be bugs or issues that could prevent a user
from executing trades or canceling trades. It is also possible trades
could be submitted in error. The user will use this package at their own
risk and not hold the author liable for any potential errors or issues
encountered. Specific to the order\_place function, please heed the
following warning: WARNING: TRADES THAT ARE SUCCESSFULLY ENTERED WILL BE
SUBMITTED IMMEDIATELY THERE IS NO REVIEW PROCESS. THIS FUNCTION HAS 100S
OF POTENTIAL COMBINATIONS AND ONLY A HANDFUL HAVE BEEN TESTED. IT IS
STRONGLY RECOMMENDED TO TEST THE DESIRED ORDER ON A VERY SMALL QUANTITY
WITH LITTLE MONEY AT STAKE. ANOTHER OPTION IS TO USE LIMIT ORDERS FAR
FROM THE CURRENT PRICE. TD AMERITRADE HAS THEIR OWN ERROR HANDLING BUT
IF A SUCCESSFUL COMBINATION IS ENTERED IT COULD BE EXECUTED IMMEDIATELY.
DOUBLE CHECK ALL ENTRIES BEFORE SUBMITTING.

## Installation

You can install rameritrade using:

``` r
# install.packages("devtools")
devtools::install_github("ttrevisan18/rameritrade")

# install.packages("rameritrade")
```

## Authentication

Authorization to a TD Brokerage account requires a multi-step
authentication process. Once initial authorization is achieved, tokens
can be used to maintain access. Below is a a summary of the steps
required. More can be found at the [TD authentication
FAQ](https://developer.tdameritrade.com/content/authentication-faq) or
the [Authentication
Guide](https://developer.tdameritrade.com/content/getting-started#registerApp).
Details are also provided within the functions.

1.  Register an account with [TD Ameritrade
    Developer](https://developer.tdameritrade.com/)
2.  Create an app under My Apps and enter a Callback URL (for example:
    <https://YourAppName>)
3.  Use the function auth\_init\_url to generate a URL specific to the
    app for user log in
4.  Log in to a TD Brokerage account to grant the app access to the user
    account
5.  “Allow” the app access which will generate an authorization code
6.  Feed the authorization code into auth\_init\_refresh to get a
    refresh token
7.  The refresh token is valid for 90 days and can be used to generate
    an access token using auth\_get\_access
8.  The access token will be used in all the other functions
9.  To reset the refresh token as it approaches expiration, you can
    follow steps 1-6 or use auth\_get\_refresh
10. TD has indicated they prefer infrequent token generation and will
    take action on excessive tokens being generated

#### Terminology

  - Authorization Code: generated from the app specific URL when a TD
    Brokerage account logs in
  - Refresh Token: generated using the Authorization Code and is used to
    create access tokens. Refresh token is valid for 90 days
  - Access Token: generated using the Refresh Token and creates the
    connection to the API. Valid for 30 minutes.

<!-- end list -->

``` r

### Step 1 - Register an app and generate a log in URL
### Register an App with TD Ameritrade Developer, create a Callback URL, and get a Consumer Key
### The callback URL can be anything (for example: https://myTDapp) 
### Use the auth_init_loginURL to generate an app specific URL. See the TD Authentication FAQ for issues.
callbackURL = 'https://myTDapp'
consumerKey = 'consumerKey'
auth_init_loginURL(callbackURL,consumerKey)
# "https://auth.tdameritrade.com/auth?response_type=code&redirect_uri=https://myTDapp&client_id=consumerKey%40AMER.OAUTHAP"

### Visit the URL above to see a TD log in screen. Log in with a TD Brokerage account to grant the app access. 


### Step 2 - Feed the Authorization Code URL into auth_init_refreshToken to get a Refresh Token
### The Authorization Code will be embedded in the URL once access is "Allowed"
### The page may indicate "This site can't be reached". The URL is still valid.
refreshToken = auth_init_refreshToken(callbackURL,consumerKey,'https://myTDapp/?code=AUTHORIZATIONCODE')
# "Successful Refresh Token Generated"

### Save the Refresh Token to a safe location so it can be retrieved as needed. It will be valid for 90 days.
saveRDS(refreshToken,'/secure/location/')

### Step 3 - Use the Refresh Token to get an Access Token
### The function will return an Access Token and also store it for use as a default token
refreshToken = readRDS('/secure/location/')
accessToken = auth_new_accessToken(refreshToken,consumerKey)
# "Successful Login. Token has been stored and will be valid for 30 minutes"

### Step 4 - When needed, the Refresh Token should be reset before it expires after 90 days. 
### TD indicates they do look for frequent Refresh Token generation. This function should be used conservatively. 
refreshToken = readRDS('/secure/location/')
refreshToken = auth_new_refreshToken(refreshToken,consumerKey)
# "Successful Refresh Token Generated"
saveRDS(refreshToken,'/secure/location/')
```

## Get Position Data

Use the position functions to get position details for a specified
account.

``` r
library(rameritrade)

refreshToken = readRDS('/secure/location/')
accessToken = auth_new_accessToken(refreshToken,consumerKey)
accountNumber = 1234567890
posList = act_position_list(accountNumber)
str(posList)
# $ securitiesAccount:List of 9
# ..$ type                   : chr "CASH"
# ..$ accountId              : chr "1234567890"
# ..$ roundTrips             : int 0
# ..$ isDayTrader            : logi FALSE
# ..$ isClosingOnlyRestricted: logi FALSE
# ..$ positions              :List of 2
# .. ..$ :List of 10
# .. .. ..$ shortQuantity                 : num 0
```

Show order examples

show example of using two access tokens
