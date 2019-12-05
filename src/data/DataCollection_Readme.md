
# Data Collection Instructions

This directory contains two separate folders: FBCode and TwitterCode.  It also contains the script setUpPoliticiansList.R
As a second step (after Twitter and FB authentication, see below), the script setUpPoliticiansList.R should be run, in order to convert the csv file with public Twitter and FB profiles to follow into a format ready for use with every data download procedure, and to test that the listed profiles exist and are public.

General helper functions that are needed by both the FB and the Twitter download procedures are contained in the file "GeneralUtilityFuncs.R"


## FBCode

This folder holds code necessary for downloading data from the Facebook API.

As a first step, the script authenticateFB.R needs to be run, in order to generate user tokens that will allow the download from FB public pages.  This presumes that a FB app has been created, the FB API approval process has been completed, and public pages permissions acquired.  The procedure for doing so is far from trivial!

The FB app codes needed for the authentication and download procedures should be kept in a secret file (in this case, in the file "mySecrets.R", which also holds the Twitter app secrets, and is _not_ included in the GitHub code)

Once a sufficient number of user tokens has been created (the more user tokens, the more data can be downloaded per hour), and the list of profiles to follow set up with setUpPoliticiansList.R, data must be downloaded in the following order:

1. Download the posts - these are the postings directly on the FB public page.  Use the collectFBPosts.R script
1. Download the comments to the posts:  use the collectFBcomments.R script
1. Download the replies to the comments: use the collectFBreplies.R script

The folder myRfacebook contains modifications to the Rfacebook library package, as it is no longer being maintained, and updates had to be made in order for it to continue to work with current FB API settings


## TwitterCode

This folder holds code necessary for downloading data from the Twitter API.

As a first step, the script authenticateTwitter.R needs to be run, in order to generate a (only one token neeeded!) token that will allow the download of Twitter data from public twitter profiles.  This presumes that a Twitter app has been created, and the Twitter app approval process completed.  The Twitter app codes needed for the authentication and download procedures should be kept in a secret file (in this case, in the file "mySecrets.R", which also holds the FB app secrets, and is _not_ included in the GitHub code)

After the setUpPoliticians.R script has ben run, data can be regularly downloaded from Twitter using the collectTwitterComments.R script.  If instead a keyword search is required, run the script KeyWordSearch.R



