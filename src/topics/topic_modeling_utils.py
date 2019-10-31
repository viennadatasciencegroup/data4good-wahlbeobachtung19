#!/usr/bin/python
""" topic_modeling_utils

Utility methods for topic modeling script.

Author: datadonk23
Date: 31.10.19 
"""

import os, sys
sys.path.append(os.path.abspath(os.path.join(os.getcwd(),
                                             os.pardir, os.pardir, "config")))
from config import credentials
import dropbox
import pyLDAvis
import pyLDAvis.sklearn
import numpy as np
import pandas as pd


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


def preprocess(data):
    """ Preprocess Posts dataset.

    :param data: DF of Posts
    :type data: pd.DataFrame
    :return: DF of preprocessed data
    """
    # Remove unnecessary cols
    data_cols = data.columns.to_list()
    data_cols.remove("text")  # keep this
    data_cols.remove("textID")  # keep this
    data.drop(data_cols, axis=1, inplace=True)

    # Rename cols
    data.columns = ["text", "textID"]

    # Remove empty texts
    data.text.replace("", np.nan, inplace=True)
    data.dropna(subset=["text"], inplace=True)
    data = data[~data.text.str.isspace()]

    # Remove duplicated texts
    data.drop_duplicates(subset=["text"], keep="first", inplace=True)

    return data


def print_scores(model, tfidf_v, name):
    """ Prints Log-Likelihood and Perplexity score of model to stdout.

    :param model: fitted LDA model
    :type model: sklearn.decomposition.LatentDirichletAllocation
    :param tfidf_v: Tfidf vectors of corpus
    :type tfidf_v: scipy.sparse.csr.csr_matrix
    :return: -
    """
    print("Scores for", name)
    print("Log Likelihood:", model.score(tfidf_v))
    print("Perplexity:", model.perplexity(tfidf_v), "\n")


def visualize_model(model, tfidf, tfidf_v, figures_path):
    """ Visualization of LDA model.

    :param model: fitted LDA model
    :type model: sklearn.decomposition.LatentDirichletAllocation
    :param tfidf: Tfidf vectorizer
    :type tfidf: sklearn.feature_extraction.text.TfidfVectorizer
    :param tfidf_v: Tfidf vectors of corpus
    :type tfidf_v: scipy.sparse.csr.csr_matrix
    :param figures_path: Path to persist visualization
    :type figures_path: str
    :return: -
    """
    viz_fpath = os.path.join(figures_path, "topics", "topics_LDAn40_model.html")

    p = pyLDAvis.sklearn.prepare(model, tfidf_v, tfidf, mds="tsne")
    pyLDAvis.save_html(p, viz_fpath)


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
