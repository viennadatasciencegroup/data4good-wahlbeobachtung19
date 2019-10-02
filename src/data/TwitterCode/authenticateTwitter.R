

# DATE: 11.09.2019
# Authenticating to Twitter

# General Twitter usage tips:
# Go to developer.twitter.com
# ai_psp account approved

# Hints on setting up Authentication:
# http://thinktostart.com/twitter-authentification-with-r/
# Callback URL: http://127.0.0.1:1410


# This twitter website is also interesting:
# https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/get-statuses-lookup

# We have to alter the search_term code to allow premium search options:
# https://developer.twitter.com/en/docs/tweets/search/api-reference/premium-search#SearchRequests



library(rtweet) 




#########################################################################################
###### Authentication  ##################################################################
#########################################################################################

## This is needed only once.  The token is then saved as an environment variable, and available every successive time.
## Redo this process only if re-generating keys (in mySecrets.R)

source("R Code/mySecrets.R")

## access token method: create token and save it as an environment variable
TwToken <- create_token(
  app = "NRW2019 1",
  consumer_key = my_api_key,
  consumer_secret = my_api_secret,
  access_token = my_access_token,
  access_secret = my_access_token_secret)


# test it:
df <- search_tweets("@ArminWolf")


# Even though create_token creates and saves an environment token variable,
# which is automatically loaded whenever we use the rtweet library,
# we may want to save and load the token explicitly, in order to
# 1) share the token
# 2) be able to run different apps using different tokens

saveRDS(TwToken, "Data/TwData/Tokens/Token.RDS")

# test it:
TwToken <- readRDS("Data/TwData/Tokens/Token.RDS")
df2 <- search_tweets("@ArminWolf", token = TwToken)

rm(df, df2)
