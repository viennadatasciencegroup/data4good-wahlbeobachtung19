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



def request(url, params=None):
    """ Make API request

    :param url: page URL
    :param params: parameters of request
    :return: response
    """
    try:
        if params:
            response = requests.get(url, params)
        else:
            response = requests.get(url)

        # If the response was successful, no Exception will be raised
        response.raise_for_status()
    except HTTPError as http_err:
        print(f'HTTP error occurred: {http_err}')
    except Exception as err:
        print(f'Other error occurred: {err}')
    else:
        print("Request successful")
        return response


def scrape_ads(id):
    """ Scrape ads from one source (specified by user ID).

    :param id: user ID
    :return: list of ad dicts
    """
    ads_archive_url = "https://graph.facebook.com/v4.0/ads_archive"
    fields = "ad_creation_time,ad_creative_body,ad_creative_link_caption," \
             "ad_creative_link_description,ad_creative_link_title," \
             "ad_delivery_start_time,ad_delivery_stop_time,ad_snapshot_url," \
             "currency,demographic_distribution,funding_entity,impressions," \
             "page_id,page_name,region_distribution,spend"
    params = {"ad_active_status": "ALL",
              "ad_reached_countries": "['AT']",
              "ad_type": "POLITICAL_AND_ISSUE_ADS",
              "fields": fields,
              "limit": 25,
              "search_page_ids": id,
              "access_token": credentials.ad_lib_access_token}

    # Inital request
    response = request(ads_archive_url,params)
    response_json = response.json()
    #FIXME check if in relevant timeframe
    results = response_json["data"]

    # Pagination requests
    last_page = False
    while not last_page:
        try:
            if "next" in response_json["paging"].keys():
                url = response_json["paging"]["next"]
                response = request(url)
                response_json = response.json()
                if len(response_json["data"]) != 0:
                    results.append(response_json["data"])
                print(len(response_json["data"]))
            else:
                last_page = True
        except KeyError:
            last_page = True

    return results


if __name__ == "__main__":
    data_path = "/mnt/DATA/NRW2019 Dropbox/data 4good/Info Lists"#FIXME DBaccess

    logging.info("Load Fanpages list")
    fanpages_df = pd.read_csv(os.path.join(data_path, "Test_AdsList.csv"))
    #FIXME loop through sources
    #for _, row in fanpages_df.iterrows():
    #    ads_data = fetch_ads(row.ID)
    results = scrape_ads(fanpages_df.ID.head(1).values[0])
    print(len(results))

    scraped_data = pd.DataFrame(results, columns=[
        "ad_creation_time", "ad_creative_body", "ad_creative_link_caption",
        "ad_creative_link_description", "ad_creative_link_title",
        "ad_delivery_start_time", "ad_delivery_stop_time", "ad_snapshot_url",
        "currency", "demographic_distribution", "funding_entity",
        "impressions", "page_id", "page_name", "region_distribution", "spend"])

    for res in results:
        print(len(res))
        print(res)

    print(scraped_data.shape)
    print(scraped_data.ad_delivery_stop_time)
