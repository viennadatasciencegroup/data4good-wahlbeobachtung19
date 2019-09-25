#!/usr/bin/python
""" fetch_ad_lib

Fetch data from FB Ad Lib API.

Author: datadonk23
Date: 25.09.19 
"""

import os, sys, logging
logging.basicConfig(level=logging.INFO)

import pandas as pd
import requests
from requests.exceptions import HTTPError

sys.path.append(os.path.abspath(os.path.join(os.getcwd(),
                                             os.pardir, os.pardir, "config")))
from config import credentials



def fetch_ads(id):
    """ FIXME

    :param id:
    :return:
    """
    ads_archive_url = "https://graph.facebook.com/v4.0/ads_archive"

    try:
        response = requests.get(ads_archive_url,
                     params={
                         "ad_reached_countries": "['AT']",
                         "ad_type": "POLITICAL_AND_ISSUE_ADS",
                         "fields": "spend,page_name",
                         "limit": 800,
                         "search_page_ids": id,
                         "access_token": credentials.ad_lib_access_token
                             })

        # If the response was successful, no Exception will be raised
        response.raise_for_status()
    except HTTPError as http_err:
        print(f'HTTP error occurred: {http_err}')
    except Exception as err:
        print(f'Other error occurred: {err}')
    else:
        print('Success!')
        print(response.json())

    return id


if __name__ == "__main__":
    data_path = "/mnt/DATA/NRW2019 Dropbox/data 4good/Info Lists"#FIXME DBaccess

    logging.info("Load Fanpages list")
    fanpages_df = pd.read_csv(os.path.join(data_path, "Test_AdsList.csv"))
    #for _, row in fanpages_df.iterrows():
    #    ads_data = fetch_ads(row.ID)
    fetch_ads(fanpages_df.ID.head(1).values[0])
