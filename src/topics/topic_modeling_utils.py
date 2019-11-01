#!/usr/bin/python
""" topic_modeling_utils

Utility methods for Topic Modeling script.

Author: datadonk23
Date: 31.10.19 
"""

import os, sys
sys.path.append(os.path.abspath(os.path.join(os.getcwd(),
                                             os.pardir, os.pardir, "config")))
from config import credentials
import dropbox


def load_data(path):
    """ Load Datasets

    Loads Post dataset.

    :param path: data directory path
    :param path: str
    :return: DF of raw Posts data
    """
    # Dropbox client
    team_dbx = dropbox.DropboxTeam(credentials.dropbox_team_access_token)
    team_root = team_dbx.with_path_root(dropbox.common.PathRoot.namespace_id(
        credentials.dropbox_team_namespace_id))
    user_dbx = team_root.as_user(credentials.dropbox_team_member_id)

    # Load Posts
    _, res_posts = user_dbx.files_download(os.path.join(path, "PolPosts.csv"))
    posts = pd.read_csv(res_posts.raw, low_memory=False)

    return posts


def save_data(data, path):
    """ Persist data (incl. assigned topics) to disk.

    :param data: DF with assigned topics to persist
    :type data: pd.DataFrame
    :param path: data directory path
    :type path: str
    :return: -
    """
    # Dropbox client
    team_dbx = dropbox.DropboxTeam(credentials.dropbox_team_access_token)
    team_root = team_dbx.with_path_root(dropbox.common.PathRoot.namespace_id(
        credentials.dropbox_team_namespace_id))
    user_dbx = team_root.as_user(credentials.dropbox_team_member_id)

    # Persist to disk
    persist_fpath = os.path.join(path, "topics", "final_topics_LDAn40.csv")
    user_dbx.files_upload(bytes(data.to_csv(index=False), "utf-8"),
                          persist_fpath, mode=dropbox.files.WriteMode.overwrite)
