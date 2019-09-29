#!/usr/bin/python
""" datasets_io

IO Util functions for dataset loading and persisting.

Author: datadonk23
Date: 29.09.19
"""

import os
import pandas as pd


def load_data(path):
    """ Load Datasets

    Loads Post and Comments datasets and merge them together.

    :param path: data directory path
    :param path: str
    :return: DF of raw data
    """
    # Load Posts and Comments DF
    posts = pd.read_csv(os.path.join(path, "PolPosts.csv"), low_memory=False)
    comments = pd.read_csv(os.path.join(path, "UserComments.csv"),
                           low_memory=False)

    # Merge Posts with Comments DFs
    raw_data = pd.concat([posts, comments], sort=False)

    return raw_data


def dump_data(df, path):
    """ Dump cleaned DF

    :param df: DF of cleaned data
    :param df: pd.DataFrame
    :param path: data directory path
    :param path: str
    :return: -
    """
    dump_path = os.path.join(path, "CleanedTextDF_interim.csv")
    df.to_csv(dump_path, index=False)
