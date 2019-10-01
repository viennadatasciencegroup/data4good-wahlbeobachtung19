#!/usr/bin/python
""" scrape_ad_lib

Scrapes data from FB Ad Lib API.

The script loops through provided list of sources and scrapes all ads,
which were delivered in a specific time window. It merges all interim results
into a final result csv-file eventually.

Constraint: Due to the nature of Ad Lib API, there's no way to determine how
long an ad was delivered by FB, if the advertiser hasn't specified a stop
date.

Author: datadonk23
Date: 25.09.19 
"""

import os, sys, logging, glob
from datetime import date, datetime, timezone
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
        logging.critical(f'HTTP error occurred: {http_err}')
    except Exception as err:
        logging.critical(f'Other error occurred: {err}')
    else:
        logging.info("Request successful")
        return response


def results_within_window(response, window):
    """ Filters ads which were delivered within time window.

    :param response: response to Graph API request
    :type response: requests.Response
    :param window: inclusive boundaries of time window in which ad must be
                   delivered. (from_date, until_date)
    :type window: (datetime.date, datetime.date)
    :return: list of ads, which were delivered in time window
    """
    global missing_stop_accounts
    relevant_results = []

    for ad in response.json()["data"]:
        ad_name = ad["page_name"]
        ad_from = datetime.strptime(ad["ad_delivery_start_time"],
                                    "%Y-%m-%dT%H:%M:%S%z")
        ad_from_date = datetime.date(ad_from.astimezone(timezone.utc))
        if "ad_delivery_stop_time" in ad.keys():
            ad_to = datetime.strptime(ad["ad_delivery_stop_time"],
                                      "%Y-%m-%dT%H:%M:%S%z")
            ad_to_date = datetime.date(ad_to.astimezone(timezone.utc))
        # ATTENTION if not specified, there's no way to determine how long the
        # ad ran. They ran until advertiser stops it or campaign budget is
        # spent. Both can't be determined from API responses.
        else:
            if ad_name not in missing_stop_accounts:
                missing_stop_accounts.append(ad_name)
                logging.warning("Missing 'ad_delivery_stop_time'-field")
            ad_to_date = date.today()

        # Overlap in ranges: (StartA <= EndB) and (EndA >= StartB)
        if (ad_from_date <= window[1]) and (ad_to_date >= window[0]):
            relevant_results.append(ad)

    return relevant_results


def scrape_ads(id, name, window, data_path):
    """ Scrape ads from one source (specified by user ID).

    :param id: user ID
    :type id: int
    :param name: user name
    :type name: str
    :param window: inclusive boundaries of time window in which ad must be
                   delivered. (from_date, until_date)
    :type window: (datetime.date, datetime.date)
    :param data_path: path of data folder
    :type data_path: str
    :return: pd.DF of scraped ads
    """
    ads_archive_url = "https://graph.facebook.com/v4.0/ads_archive"
    fields = "ad_creation_time,ad_creative_body,ad_creative_link_caption," \
             "ad_creative_link_description,ad_creative_link_title," \
             "ad_delivery_start_time,ad_delivery_stop_time,ad_snapshot_url," \
             "currency,demographic_distribution,funding_entity,impressions," \
             "page_id,page_name,region_distribution,spend"
    columns = fields.split(",")
    params = {"ad_active_status": "ALL",
              "ad_reached_countries": "['AT']",
              "ad_type": "POLITICAL_AND_ISSUE_ADS",
              "fields": fields,
              "limit": 25,
              "search_page_ids": int(id),
              "access_token": credentials.ad_lib_access_token}
    relevant_ads = []

    # Inital request
    response = request(ads_archive_url,params)
    relevant_ads.extend(results_within_window(response, window))

    # Pagination requests
    last_page = False
    while not last_page:
        try:
            if "next" in response.json()["paging"].keys():
                url = response.json()["paging"]["next"]
                response = request(url)
                if len(response.json()["data"]) != 0:
                    relevant_ads.extend(results_within_window(response, window))
            else:
                last_page = True
        except KeyError:
            last_page = True

    # Make DF of relevant ads and persist it as intermediate result
    scraped_ads = pd.DataFrame(relevant_ads, columns=columns)
    f_path = os.path.join(data_path, name.lower().replace(" ", "_") + ".csv")
    scraped_ads.to_csv(f_path, index=False)

    return scraped_ads


def combine_intermediate_results(data_path, results_fpath):
    """ Comnines interim results DFs to final result DF.

    :param data_path: path of data folder
    :type data_path: str
    :param results_fpath: filepath for final results
    :type results_fpath: str
    :return: DF of all scraped ads
    """
    interim_results = []
    interim_results_fpath = glob.glob(os.path.join(data_path, "*.csv"))
    if results_fpath in interim_results_fpath:
        interim_results_fpath.remove(results_fpath)

    for fpath in interim_results_fpath:
        interim_results.append(pd.read_csv(fpath))

    return pd.concat(interim_results, axis=0)


if __name__ == "__main__":
    # FIXME DBaccess
    info_path = "/mnt/DATA/NRW2019 Dropbox/data 4good/Info Lists"
    data_path = "/mnt/DATA/NRW2019 Dropbox/data 4good/CSVData/ads"
    results_fpath = os.path.join(data_path, "AdLibAll.csv")
    missing_stop_accounts = []

    # Get sources list
    logging.info("Load Fanpages list")
    fanpages_df = pd.read_csv(os.path.join(info_path, "AdsListID.csv"))

    # Loop through sources list and scrape ads for each source
    for _, row in fanpages_df.iterrows():
        logging.info("Scrape ads from " + row.Name)
        ads_data = scrape_ads(row.userID, row.Name,
                              (date(2019, 9, 8), date(2019, 9, 29)), data_path)

    logging.info("Count of accounts with missing 'ad_delivery_stop_time': " +
                 str(len(missing_stop_accounts)))
    logging.info("Accounts with missing 'ad_delivery_stop_time': " +
                 "; ".join(missing_stop_accounts))

    # Merge all interim results in one DF of all scraped ads
    logging.info("Concatinating intermediate results")
    result = combine_intermediate_results(data_path, results_fpath)

    logging.info("Persisting final results")
    result.to_csv(results_fpath, index=False)
