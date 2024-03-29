#!/usr/bin/python
""" scrape_ad_lib

Scrapes data from FB Ad Lib API.

The script loops through provided list of sources and scrapes all ads,
which were delivered in a specific time window. It merges all interim results
into a final result csv-file eventually.

Constraint: Due to the nature of Ad Lib API, there's no way to determine how
long an ad was delivered by FB, if the advertiser hasn't specified a stop
date. For this case, a decision day must be specified. The script will than
fetch ads only, which were started after this day until end of monitoring
period day and a stop of delivery day is assumed on last day of monitoring
period.

Author: datadonk23
Date: 08.10.19
"""

import os, sys, logging
from datetime import date, datetime, timezone
logging.basicConfig(level=logging.INFO)

import pandas as pd
import dropbox
import requests
from requests.exceptions import HTTPError

sys.path.append(os.path.abspath(os.path.join(os.getcwd(),
                                             os.pardir, os.pardir, "config")))
from config import credentials


def request(url, params=None):
    """ Make API request.

    :param url: page URL
    :param params: parameters of request
    :return: response
    :rtype: requests.Response
    """
    try:
        if params:
            response = requests.get(url, params)
        else:
            response = requests.get(url)

        # If the response was successful, no Exception will be raised
        response.raise_for_status()
    except HTTPError as http_err:
        logging.critical(f"HTTP error occurred: {http_err}")
    except Exception as err:
        logging.critical(f"Other error occurred: {err}")
    else:
        logging.info("Request successful")
        return response


def results_within_window(response, window, non_stop_decision_date):
    """ Filters ads which were delivered within time window.

    :param response: response to Graph API request
    :type response: requests.Response
    :param window: inclusive boundaries of time window in which ad must be
                   delivered. (from_date, until_date)
    :type window: (datetime.date, datetime.date)
    :param non_stop_decision_date: ads which were started after this day,
                                   will be fetched although stop field is not
                                   specified
    :type non_stop_decision_date: datetime.date
    :return: list of ads, which were delivered in time window
    :rtype: list
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
        # ad ran. They're running  until advertiser stops it or campaign
        # budget is spent. Both can't be determined from API responses.
        # Therefore we're using this policy, if 'ad_delivery_stop_time' is not
        # specified:
        #   * We are fetching only ads with a delivery start after
        #     non_stop_decision_date (01.08.2019) until end of monitoring
        #     period (29.09.2019) and assume deliverance has been stopped at
        #     the end of monitoring period (= election day) as well.
        else:
            # log fan page account which contains unreliable data
            if ad_name not in missing_stop_accounts:
                missing_stop_accounts.append(ad_name)
                logging.warning("Missing 'ad_delivery_stop_time'-field")

            # implement workaround policy:
            # check if delivery started before decision date
            if ad_from_date < non_stop_decision_date:
                # assumed to stop before decision day -> will not be fetched
                ad_to_date = ad_from_date
            # or delivery started after monitoring period
            elif ad_from_date > window[1]:
                # today as placeholder end date -> will not be fetched
                ad_to_date = datetime.today()
            else:
                # assumed to stop on election day -> will be fetched
                ad_to_date = window[1]

        # Overlap in ranges: (StartA <= EndB) and (EndA >= StartB)
        if (ad_from_date <= window[1]) and (ad_to_date >= window[0]):
            relevant_results.append(ad)

    return relevant_results


def scrape_ads(id, name, window, non_stop_decision_date, data_path, user_dbx):
    """ Scrape ads from one source (specified by user ID).

    :param id: user ID
    :type id: int
    :param name: user name
    :type name: str
    :param window: inclusive boundaries of time window in which ad must be
                   delivered. (from_date, until_date)
    :type window: (datetime.date, datetime.date)
    :param non_stop_decision_date: ads which were started after this day,
                                   will be fetched although stop field is not
                                   specified
    :type non_stop_decision_date: datetime.date
    :param data_path: path of data folder
    :type data_path: str
    :param user_dbx: Dropbox access as authenticated team user
    :type user_dbx: dropbox.dropbox.Dropbox
    :return: DF of scraped ads
    :rtype: pd.DataFrame
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
    response = request(ads_archive_url, params)
    relevant_ads.extend(results_within_window(response, window,
                                              non_stop_decision_date))

    # Pagination requests
    last_page = False
    while not last_page:
        try:
            if "next" in response.json()["paging"].keys():
                url = response.json()["paging"]["next"]
                response = request(url)
                if len(response.json()["data"]) != 0:
                    filtered_ads = results_within_window(response, window,
                                                         non_stop_decision_date)
                    relevant_ads.extend(filtered_ads)
            else:
                last_page = True
        except KeyError:
            last_page = True

    # Make DF of relevant ads, clean snapshot URL (remove access token!)
    scraped_ads = pd.DataFrame(relevant_ads, columns=columns)
    cleaned_ads = scraped_ads.copy()
    cleaned_ads["ad_snapshot_url"] = scraped_ads["ad_snapshot_url"].apply(
        lambda x: x[:x.rfind("&access_token=")])

    # persist it as intermediate result
    f_path = os.path.join(data_path, name.lower().replace(" ", "_") + ".csv")
    user_dbx.files_upload(bytes(cleaned_ads.to_csv(index=False), "utf-8"),
                          f_path, mode=dropbox.files.WriteMode.overwrite)

    return cleaned_ads


def combine_intermediate_results(data_path, results_fname, user_dbx):
    """ Combines interim results DFs to final result DF.

    :param data_path: path of data folder
    :type data_path: str
    :param results_fname: filename of combined results CSV file
    :type results_fname: str
    :param user_dbx: Dropbox access as authenticated team user
    :type user_dbx: dropbox.dropbox.Dropbox
    :return: DF of all scraped ads
    :rtype: pd.DataFrame
    """
    interim_results = []
    interim_results_files = []
    for entry in user_dbx.files_list_folder(data_path, limit=500).entries:
        if str(entry.name).endswith(".csv"):
            interim_results_files.append(str(entry.name))

    if results_fname in interim_results_files:
        interim_results_files.remove(results_fname)

    for file_name in interim_results_files:
        _, res = user_dbx.files_download(os.path.join(data_path, file_name))
        interim_results.append(pd.read_csv(res.raw))

    return pd.concat(interim_results, axis=0)


if __name__ == "__main__":
    # Dropbox client
    team_dbx = dropbox.DropboxTeam(credentials.dropbox_team_access_token)
    team_root = team_dbx.with_path_root(dropbox.common.PathRoot.namespace_id(
        credentials.dropbox_team_namespace_id))
    user_dbx = team_root.as_user(credentials.dropbox_team_member_id)

    # Paths and filenames
    info_path = "/Data/Info Lists"
    data_path = "/Data/CSVData/ads"
    results_fname = "Test_AdLibAll.csv"

    # Monitoring period constraints
    monitoring_period_start = date(2019, 9, 8)
    monitoring_period_stop = date(2019, 9, 29)
    monitoring_window = (monitoring_period_start, monitoring_period_stop)
    missing_stop_accounts = []  # bookkeep accounts w/ missing stop dates
    non_stop_decision_date = date(2019, 8, 1)

    # Get sources list
    logging.info("Load FanPages list")
    _, res_sources = user_dbx.files_download(os.path.join(info_path,
                                                          "AdsListID.csv"))
    fanpages_df = pd.read_csv(res_sources.raw)

    # Loop through sources list and scrape ads for each source
    for _, row in fanpages_df.iterrows():
        logging.info("Scrape ads from " + row.Name)
        ads_data = scrape_ads(row.userID, row.Name, monitoring_window,
                              non_stop_decision_date, data_path, user_dbx)

    logging.info("Count of accounts with missing 'ad_delivery_stop_time': " +
                 str(len(missing_stop_accounts)))
    logging.info("Accounts with missing 'ad_delivery_stop_time': " +
                 "; ".join(missing_stop_accounts))

    # Merge all interim results in one DF of all scraped ads
    logging.info("Concatinating intermediate results")
    result = combine_intermediate_results(data_path, results_fname, user_dbx)

    logging.info("Persisting final results")
    dump_path = os.path.join(data_path, results_fname)
    user_dbx.files_upload(bytes(result.to_csv(index=False), "utf-8"),
                          dump_path, mode=dropbox.files.WriteMode.overwrite)
