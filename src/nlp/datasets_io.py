#!/usr/bin/python
""" datasets_io

IO Util functions for dataset loading and persisting.

Author: datadonk23
Date: 29.09.19
"""

import os, sys
sys.path.append(os.path.abspath(os.path.join(os.getcwd(),
                                             os.pardir, os.pardir, "config")))
from config import credentials
import dropbox
import pandas as pd


def load_data(path):
    """ Load datasets

    Loads Post and Comments datasets and merge them together.

    :param path: data directory path
    :param path: str
    :return: DF of raw data
    """
    # Dropbox client
    team_dbx = dropbox.DropboxTeam(credentials.dropbox_team_access_token)
    team_root = team_dbx.with_path_root(dropbox.common.PathRoot.namespace_id(
        credentials.dropbox_team_namespace_id))
    user_dbx = team_root.as_user(credentials.dropbox_team_member_id)

    # Load Posts and Comments DF
    _, res_posts = user_dbx.files_download(os.path.join(path, "PolPosts.csv"))
    posts = pd.read_csv(res_posts.raw, low_memory=False)
    _, res_comments = user_dbx.files_download(os.path.join(path,
                                                           "UserComments.csv"))
    comments = pd.read_csv(res_comments, low_memory=False)

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
    # Dropbox client
    team_dbx = dropbox.DropboxTeam(credentials.dropbox_team_access_token)
    team_root = team_dbx.with_path_root(dropbox.common.PathRoot.namespace_id(
        credentials.dropbox_team_namespace_id))
    user_dbx = team_root.as_user(credentials.dropbox_team_member_id)

    # Dump
    dump_path = os.path.join(path, "CleanedTextDF_interim.csv")
    user_dbx.files_upload(bytes(df.to_csv(index=False), "utf-8"),
                          dump_path, mode=dropbox.files.WriteMode.overwrite)
