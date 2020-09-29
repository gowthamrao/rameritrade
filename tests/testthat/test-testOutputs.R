test_that("Function response type matches expectation", {
  
  ### No tests will work on Cran because no access
  ### All tests bundled into one to only require one log in
  skip_on_cran()
  
  tdKeys = readRDS('/home/rstudio/Secure/tdKeys.rds')
  callbackURL = tdKeys$callbackURL
  consumerKey = tdKeys$consumerKey
  accountNumber = tdKeys$account2
  refreshToken = readRDS('/home/rstudio/Secure/RefToken.rds')
  
  ### Login URL
  expect_match(auth_init_loginURL(callbackURL,consumerKey),'https')
  expect_match(auth_init_loginURL(callbackURL,consumerKey),callbackURL)
  expect_match(auth_init_loginURL(callbackURL,consumerKey),consumerKey)
  
  
  ### Confirm errors
  expect_error(auth_init_refreshToken(callbackURL,consumerKey,'https://myTDapp/?code=Auhtorizationcode'))
  expect_error(auth_new_accessToken('reftoken',consumerKey))
  expect_error(auth_new_accessToken(refreshToken$refresh_token,consumerKey))
  expect_error(auth_new_refreshToken('reftoken',consumerKey))
  expect_error(auth_new_refreshToken(refreshToken$refresh_token,consumerKey))
  
  ### Confirm response
  AccToken = auth_new_accessToken(refreshToken,consumerKey)
  
  expect_output(str(AccToken), "List of 6")
  

  ### Check account information
  expect_output(str(act_data_list()), "List of 2")
  expect_output(str(act_data_df('orders')), "List of 2")

  expect_error(act_data_list(accessToken = accessToken$access_token)) # expect fail
  expect_equal(is.data.frame(act_data_df()), TRUE)
  expect_equal(is.data.frame(act_data_df('positions')), TRUE)
  
  
  ### Check pricing
  SP500Qt = price_quote_df(c('SPY','IVV','VOO'))
  expect_equal(nrow(SP500Qt), 3)
 
  SP500H = price_hisotry_mult(c(c('SPY','IVV','VOO')))
  expect_equal(is.data.frame(SP500H), TRUE)
  expect_equal(length(unique(SP500H$ticker)), 3)
  expect_error(price_quote_list(accessToken = 'fail'))
  
  Over15 = price_hisotry_mult(c('SPY','IVV','VOO','NOBL','RALS',
                                       'TQQQ','SQQQ','IWM','UPRO','UVXY',
                                       'SPXU','SRTY','GDX','GDXJ','NUGT',
                                       'JNUG','DUST','JDST'))
  expect_equal(length(unique(Over15$ticker)), 15)
  
  ### Check Options
  SLV = option_chain('SLV')
  expect_output(str(SLV), "List of 2")
  expect_equal(nrow(SLV$fullChain)>100,TRUE)
  
  expect_equal(is.data.frame(transact_search(accountNumber)),TRUE)
  expect_equal(ncol(symbol_detail('aapl'))>40,TRUE)
  
  ### Orders
  # ORder search
  AllOrd = order_search(accountNumber)
  TestOrder = order_detail(AllOrd$executedOrders$orderId[[1]],accountNumber)
  expect_equal(length(TestOrder)>15,TRUE)
  
  ## Place order way above limit
  PSLVQt = price_quote_list('PSLV')
  Ord3 = order_place(accountNumber = accountNumber, ticker='PSLV',
                     quantity = 1, instruction='BUY', duration='Day',
                     orderType = 'LIMIT', limitPrice = round(PSLVQt$PSLV$bidPrice*.5,2))
  Ord3Res = order_cancel(Ord3$orderId,accountNumber)
  
  expect_equal(ncol(Ord3),5)
  expect_match(Ord3Res,'https')
  expect_match(Ord3Res,'accounts')
  expect_match(Ord3Res,'orders')
  
  ### ORder errors
  expect_error(order_place(accountNumber = accountNumber, ticker='SLBB_091820P24.5',
                                quantity = 1, instruction='BUY_TO_OPEN', duration='Day',
                                orderType = 'LIMIT', limitPrice = .02, assetType = 'OPTION'))
  expect_error(order_place(accountNumber = accountNumber,ticker='pslv',
                      quantity = 1,instruction='buy',duration='good_till_cancel',
                      orderType = 'stop_limit',limitPrice=round(PSLVQt$PSLV$bidPrice*.75,2),stopPrice=round(PSLVQt$PSLV$bidPrice*.8,2)))
  
  
})

