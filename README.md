
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rameritrade

<!-- badges: start -->

<!-- badges: end -->

R package for the TD Ameritrade API, facilitating authentication,
trading, price requests, account balances, positions, order history,
option chains, and more. A user will need a TD Brokerage account and TD
Ameritrade developer app.

## Introduction

TD Ameritrade is one of many trading platforms that offer a trade API.
Others include Alpaca, RobinHood, InteractiveBrokers, and eTrade. Alpaca
and Robinhood offer great capabilities, and there are existing R
packages for both. Unfortunately, they do not offer the full
capabilities of a major brokerage firm such as IRAs, multiple accounts,
etc. InteractiveBrokers requires the IB workstation to be open and
active which can make it hard to build automated trading strategies
using tools like CRON jobs. Using the TD API you can fully automate
trade execution across multiple accounts and multiple log ins, assuming
you have access and permission to do so. This can be a great way to
dollar cost average into the market for an IRA\!

The initial authentication requires a few manual steps, but once initial
authorization is granted, all API calls can be fully automated without
needing to manually log in again. Additionally, because of the use of
tokens and a middle layer App, user name and passwords never need to be
entered into the R code. This can help protect the security of accounts
assuming tokens are stored securely.

### Disclosure

Due to the Charles Schwab acquisition of TD Ameritrade, the fate of the
TD API is unknown. There have been multiple indications that Schwab is
excited about the technology at TD Ameritrade and will retain as much as
possible. That being said, this package could stop working at any point
if the acquisition results in the termination of the API.

This software is in no way affiliated, endorsed, or approved by TD
Ameritrade or any of its affiliates. It comes with absolutely no
warranty and should not be used in actual trading unless the user can
read and understand the source code. The functions within this package
have been tested under basic scenarios. There may be bugs or issues that
could prevent a user from executing trades or canceling trades. It is
also possible trades could be submitted in error. The user will use this
package at their own risk.

Please heed the following warning for the order\_place function.
WARNING: TRADES THAT ARE SUCCESSFULLY ENTERED WILL BE SUBMITTED
IMMEDIATELY THERE IS NO REVIEW PROCESS. THE `order_place` FUNCTION HAS
HUNDREDS OF POTENTIAL COMBINATIONS AND ONLY A HANDFUL HAVE BEEN TESTED.
IT IS STRONGLY RECOMMENDED TO TEST THE DESIRED ORDER ON A VERY SMALL
QUANTITY WITH LITTLE MONEY AT STAKE. ANOTHER OPTION IS TO USE LIMIT
ORDERS FAR FROM THE CURRENT PRICE. TD AMERITRADE HAS THEIR OWN ERROR
HANDLING BUT IF A SUCCESSFUL COMBINATION IS ENTERED IT COULD BE EXECUTED
IMMEDIATELY. DOUBLE CHECK ALL ENTRIES BEFORE SUBMITTING.

## Installation

You can install rameritrade using:

``` r
# install.packages("devtools")
devtools::install_github("tonytrevisan/rameritrade")

# NOT CURRENTLY AVAILABLE ON CRAN
# install.packages("rameritrade")
```

## Authentication

Initial authorization to a TD Brokerage account requires a 3 step
authentication process. Once initial authorization is achieved, tokens
can be used to maintain access indefinitely. Below is a detailed summary
of the entire process followed by the code demonstrating the 3 step
process. More can be found at the [TD authentication
FAQ](https://developer.tdameritrade.com/content/authentication-faq) or
the [Authentication
Guide](https://developer.tdameritrade.com/content/getting-started#registerApp).
Details are also provided within the functions.

1.  Register an API Developer account with [TD Ameritrade
    Developer](https://developer.tdameritrade.com/).
2.  Create an app under My Apps. The app serves as a middle layer
    between the brokerage account and API.
3.  Identify the Consumer Key provided by TD (essentially an API Key).
4.  Under Edit App, create a Callback URL. This can be relatively simple
    (for example: `https://YourAppName`).
5.  Pass the Callback URL and Consumer Key to `auth_init_url` to
    generate a URL specific to the app for user log in.
6.  Visit the URL in a web browser and log in to a TD Brokerage account,
    granting the app access to the user account.
7.  When “Allow” is clicked, it will redirect to a blank page. The URL
    of this page is the authorization code.
8.  Feed the authorization code into `auth_init_refreshToken` to get a
    Refresh Token.
9.  The Refresh Token is valid for 90 days so be sure to store it
    somewhere safe. The Refresh Token is the only component needed from
    then on for account access. However, if your token expires or is
    lost, you can always follow steps 6-8 above.
10. The Refresh Token is used to generate an Access Token which gives
    account access for 30 minutes
11. The most recent Access Token is stored by default into Options.
    Passing it into the functions is optional unless accessing multiple
    accounts.
12. To reset the Refresh Token as it approaches expiration, you can
    follow steps 6-8 or use `auth_new_refreshToken`

Please note: TD has indicated they prefer infrequent token generation
and will take action on excessive tokens being generated

#### Terminology

  - Authorization Code: generated from the app specific URL when a TD
    Brokerage account logs in
  - Refresh Token: generated using the Authorization Code and is used to
    create access tokens. Refresh token is valid for 90 days
  - Access Token: generated using the Refresh Token and creates the
    connection to the API. Valid for 30 minutes.

## Authentication Example

The `auth_init` functions are used to gain initial access to the API.
They are used to generate a Refresh Token. Once a Refresh Token is
generated, the `auth_init` functions will not be required unless the
Refresh Token expires. This can be avoided using
`auth_new_refreshToken`, which uses an existing Refresh Token to create
a new Refresh Token.

``` r

# --------- Step 1 -----------
# Register an App with TD Ameritrade Developer, create a Callback URL, and get a Consumer Key.
# The callback URL can be anything (for example: https://myTDapp).
# Use the auth_init_loginURL to generate an app specific URL. See the TD Authentication FAQ for issues.

callbackURL = 'https://myTDapp'
consumerKey = 'APP_CONSUMER_KEY'

rameritrade::auth_init_loginURL(callbackURL,consumerKey)
# "https://auth.tdameritrade.com/auth?response_type=code&redirect_uri=https://myTDapp&client_id=consumerKey%40AMER.OAUTHAP"

# Visit the URL above to see a TD log in screen. Log in with a TD Brokerage account to grant the app access. 


# --------- Step 2 -----------
# A successful log in to the URL from Step 1 will result in a blank page once "Allow" is clicked. 
# The URL of this blank page is the authorization code. 
# The blank page may indicate "This site can't be reached". The URL is still valid.
# Feed the Authorization Code URL into auth_init_refreshToken to get a Refresh Token.

authCode = 'https://myTDapp/?code=AUTHORIZATIONCODE' # This could be over 1,000 alpha numeric characters
refreshToken = rameritrade::auth_init_refreshToken(callbackURL,consumerKey,authCode)
# "Successful Refresh Token Generated"

# Save the Refresh Token to a safe location so it can be retrieved as needed. It will be valid for 90 days.
saveRDS(refreshToken,'/secure/location/')


# --------- Step 3 -----------
# Use the Refresh Token to get an Access Token
# The function will return an Access Token and also store it for use as a default token in Options

refreshToken = readRDS('/secure/location/')
accessToken = rameritrade::auth_new_accessToken(refreshToken,consumerKey)
# "Successful Login. Token has been stored and will be valid for 30 minutes"

# Authentication has been completed. Other functions can now be used.


# --------- Step 4 (when needed) -----------
# The Refresh Token should be reset before it expires after 90 days. 
# TD indicates they do look for frequent Refresh Token generation. This function should be used conservatively. 

refreshToken = readRDS('/secure/location/')
refreshToken = rameritrade::auth_new_refreshToken(refreshToken,consumerKey)
# "Successful Refresh Token Generated"
saveRDS(refreshToken,'/secure/location/')
```

## Get Account Data

Use the `act_data_list` or `act_data_df` to get current account data,
including balances, positions, and current day orders.

``` r
library(rameritrade)

refreshToken = readRDS('/secure/location/')
consumerKey = 'APP_CONSUMER_KEY'
accessToken = rameritrade::auth_new_accessToken(refreshToken,consumerKey)
# By default balances are returned. Access token does not have to be passed
ActBal = rameritrade::act_data_list()  

str(ActBal)
# List of 2
# $ :List of 1
# ..$ securitiesAccount:List of 8
# .. ..$ type                   : chr "CASH"
# .. ..$ accountId              : chr ""
# .. ..$ roundTrips             : int 0
# .. ..$ isDayTrader            : logi FALSE
# .. ..$ isClosingOnlyRestricted: logi FALSE
# .. ..$ initialBalances        :List of 18
# .. .. ..$ accruedInterest           : num 0

# Get current account positions as a data frame
ActPos = rameritrade::act_data_df('positions')

str(ActPos)
# 'data.frame': 6 obs. of  19 variables:
# $ accountId                     : chr  "" ...
# $ type                          : chr  "CASH" "CASH" "CASH" "CASH" ...
# $ roundTrips                    : int  0 0 0 0 0 0
# $ isDayTrader                   : logi  FALSE FALSE FALSE FALSE TRUE TRUE
# $ isClosingOnlyRestricted       : logi  FALSE FALSE FALSE FALSE FALSE FALSE
# $ shortQuantity                 : num  0 0 0 0 0 0
# $ averagePrice                  : num  0 19.59 14.28 6.94 45.71 ...
```

## Get Pricing Data

Use the `price_` functions to get quotes or historical pricing. Quotes
will be real-time if the account has access to real-time quotes.

``` r
library(rameritrade)

refreshToken = readRDS('/secure/location/')
consumerKey = 'APP_CONSUMER_KEY'
accessToken = rameritrade::auth_new_accessToken(refreshToken, consumerKey)

### Quote data
SP500Qt = rameritrade::price_quote_df(c('SPY', 'IVV', 'VOO'))
str(SP500Qt)

# 'data.frame': 3 obs. of  48 variables:
# $ assetType                         : chr  "ETF" "ETF" "ETF"
# $ assetMainType                     : chr  "EQUITY" "EQUITY" "EQUITY"
# $ cusip                             : chr  "78462F103" "464287200" "922908363"
# $ assetSubType                      : chr  "ETF" "ETF" "ETF"
# $ symbol                            : chr  "SPY" "IVV" "VOO"
# $ description                       : chr  "SPDR S&P 500" "iShares Core S&P 500 ETF" "Vanguard S&P 500 ETF"
# $ bidPrice                          : num  331 332 305


# Historical Data
SP500H = rameritrade::price_hisotry_mult(c(c('SPY','IVV','VOO')))
head(SP500H)
# A tibble: 6 x 8
# ticker date       date_time            open  high   low close   volume
# <chr>  <date>     <dttm>              <dbl> <dbl> <dbl> <dbl>    <int>
# 1 SPY    2020-08-18 2020-08-18 01:00:00  338.  339.  337.  339. 38733908
# 2 SPY    2020-08-19 2020-08-19 01:00:00  339.  340.  337.  337. 68054244
# 3 SPY    2020-08-20 2020-08-20 01:00:00  335.  339.  335.  338. 42207826
# 4 SPY    2020-08-21 2020-08-21 01:00:00  338.  340.  338.  339. 55106628
# 5 SPY    2020-08-24 2020-08-24 01:00:00  342.  343   339.  343. 48588662
# 6 SPY    2020-08-25 2020-08-25 01:00:00  344.  344.  342.  344. 38463381


# Time series data
# History is only available back to a certain time depending on frequency
rameritrade::price_history_single('AAPL', startDate = '2020-09-01', freq='5min')
# # A tibble: 2,424 x 8
# ticker date       date_time            open  high   low close volume
# <chr>  <date>     <dttm>              <dbl> <dbl> <dbl> <dbl>  <int>
# 1 AAPL   2020-09-01 2020-09-01 07:00:00  132.  132.  132.  132. 203104
# 2 AAPL   2020-09-01 2020-09-01 07:05:00  132.  132.  132.  132.  85287
# 3 AAPL   2020-09-01 2020-09-01 07:10:00  132.  132   131.  131.  93742
# 4 AAPL   2020-09-01 2020-09-01 07:15:00  131.  132.  131.  132.  63895
# 5 AAPL   2020-09-01 2020-09-01 07:20:00  132.  132.  131.  131.  26498

```

## Placing Trades

Order entry offers hundreds of potential combinations. It is strongly
recommended to submit trades outside market hours first to test the
trade entries. See the [order sample
guide](https://developer.tdameritrade.com/content/place-order-samples)
for more examples. Please note, this function only allows for single
order entry and will not support some of the complex examples in the
guide.

``` r
library(rameritrade)

# Set Access Token using a valid Refresh Token
refreshToken = readRDS('/secure/location/')
consumerKey = 'APP_CONSUMER_KEY'
accessToken = rameritrade::auth_new_accessToken(refreshToken, consumerKey)
accountNumber = 1234567890

# Market Order
Ord0 = rameritrade::order_place(accountNumber,
                                ticker = 'PSLV',
                                quantity = 1,
                                instruction = 'BUY')
rameritrade::order_cancel(Ord0$orderId, accountNumber)
# [1] "Order Cancelled"



# Good till cancelled stop limit INCORRECT ENTRY
Ordr1 = rameritrade::order_place(accountNumber = accountNumber,
                                 ticker = 'SCHB',
                                 quantity = 1,
                                 instruction = 'buy',
                                 duration = 'good_till_cancel',
                                 orderType = 'stop_limit',
                                 limitPrice = 50,
                                 stopPrice = 49)
# Error: 400 - The stop price must be above the current ask for buy stop orders 
#        and below the bid for sell stop orders.



# Good till Cancelled Stop Limit Order correct entry
Ordr1 = rameritrade::order_place(accountNumber = accountNumber,
                                 ticker = 'SCHB',
                                 quantity = 1,
                                 instruction = 'buy',
                                 duration = 'good_till_cancel',
                                 orderType = 'stop_limit',
                                 limitPrice = 86,
                                 stopPrice = 85)
rameritrade::order_cancel(Ordr1$orderId, accountNumber)
# [1] "Order Cancelled"



# Trailing Stop Order
Ordr2 = rameritrade::order_place(accountNumber = accountNumber,
                                 ticker = 'SPY',
                                 quantity = 1,
                                 instruction = 'sell',
                                 orderType = 'trailing_stop',
                                 stopPriceBasis = 'BID',
                                 stopPriceType = 'percent',
                                 stopPriceOffset = 10)
rameritrade::order_cancel(Ordr2$orderId,accountNumber)
# [1] "Order Cancelled"

# Option Order
Ord3 = rameritrade::order_place(accountNumber = accountNumber,
                                ticker = 'SLV_091820P24.5',
                                quantity = 1,
                                instruction = 'BUY_TO_OPEN',
                                duration = 'Day',
                                orderType = 'LIMIT',
                                limitPrice = .02,
                                assetType = 'OPTION')
rameritrade::order_cancel(Ord3$orderId, accountNumber)
# [1] "Order Cancelled"
```

## Working with multiple accounts

Even though the most recent access token is stored by default, you can
save access tokens to manage multiple accounts assuming auth\_init was
used for two separate log ins.

``` r

library(rameritrade)
consumerKey = 'APP_CONSUMER_KEY'

refreshToken1 = readRDS('/secure/location/1')
accessToken1 = rameritrade::auth_new_accessToken(refreshToken1, consumerKey)

refreshToken2 = readRDS('/secure/location/2')
accessToken2 = rameritrade::auth_new_accessToken(refreshToken2, consumerKey)

ActBal1 = rameritrade::act_data_list(accessToken = accessToken1)

ActBal2 = rameritrade::act_data_list(accessToken = accessToken2)

```
