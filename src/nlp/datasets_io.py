#!/usr/bin/python
""" datasets_io

IO Util functions for dataset loading and persisting.

Author: datadonk23
Date: 24.09.19 
"""

import os
import pandas as pd


def load_data(path):
    """ Load Datasets

    Loads FB and Twitter datasets and merge them together.

    :param path: data directory path
    :return: DF
    """
    # Load FB data and append source tag
    fb_posts = pd.read_csv(os.path.join(path, "FBPolTimeLines.csv"))
    fb_posts["media_source"] = "fb"
    fb_comments = pd.read_csv(os.path.join(path, "FBUserComments.csv"))
    fb_comments["media_source"] = "fb"

    # Load Twitter data and append source tag
    twitter_posts = pd.read_csv(os.path.join(path, "TwPolTimeLines.csv"))
    twitter_posts["media_source"] = "tw"
    twitter_comments = pd.read_csv(os.path.join(path, "TwUserComments.csv"))
    twitter_comments["media_source"] = "tw"

    # Merge FB and Twitter DFs
    raw_data = pd.concat(
        [fb_posts, fb_comments, twitter_posts, twitter_comments], sort=False)

    return raw_data


def dump_data(df, path):
    """ Dump cleaned DF

    :param df: DF of cleaned data
    :param path: data directory path
    :return: -
    """
    dump_path = os.path.join(path, "Test_CleanedTextDF.csv")
    df.to_csv(dump_path, index=False)
