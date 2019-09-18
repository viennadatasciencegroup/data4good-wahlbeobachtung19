
# DATE: 04.09.2018
# Facebook API authentication

library(Rfacebook)


#### Using Rfacebook #### ---------------------------------------------------------------
# https://github.com/pablobarbera/Rfacebook

## Authentication: ## -------------------------------------------------------------------
# Some useful hints:
# http://thinktostart.com/analyzing-facebook-with-r/
# App API authentication no longer works, because FB does not accept http local host. It has to be https

# we need to create an https tunnel to port 1410, using ngrok:
# at the terminal, type:
# ./ngrok http 1410
## on Windows: first cd ..\.. until the prompt says C:> ... and then type:  ngrok http 1410
# ngrok will pull up a screen, with a temporary webaddress.  Copy and paste that into TestApp1.
# Then run the following code:

# Set the App ID & Keys

source("R Code/mySecrets.R")
# disable http/2, it is causing problems!!!  but this doesn't seem to work at this level ...
# probably need to fix it inside fbOAuth
httr::set_config(httr::config(http_version = 0))
FBToken <- fbOAuth(app_id = FB_ID, app_secret = FB_Key, extended_permissions = FALSE)

# quick check that it worked
(info <- getUsers("me", token = FBToken))

# chenge the token number each time!
FBPath <- file.path("Data", "FBData", "Tokens", "FBToken_2")
saveRDS(FBToken, file = FBPath)


# # How to load the token:
# FBPath <- file.path("Data", "FBData", "FBToken_7")
# FBToken <- readRDS(FBPath)

### Some additional info sources ### ----------------------------------------------------


# So, public post access is available as a requested feature after app review
# in order to do an app review, several things are needed
# 1) Company logo
# 2) Website for privacy URL
# 3) Company website
# 4) Screencast of how FB info will be used

# Where to go to get info:
# https://developers.facebook.com/docs/facebook-login/review
# https://developers.facebook.com/docs/facebook-login/review/requirements
# https://developers.facebook.com/docs/apps/review
# https://developers.facebook.com/docs/apps/review/server-to-server-apps/
# https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS



# https://developers.facebook.com/docs/graph-api/using-graph-api/#paging

# For Error Handling:
# https://developers.facebook.com/docs/graph-api/using-graph-api/error-handling


