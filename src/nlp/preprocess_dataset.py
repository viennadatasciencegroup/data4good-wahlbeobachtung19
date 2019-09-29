#!/usr/bin/python
""" preprocess_dataset

Preprocess dataset of FB and Twitter data.

Author: datadonk23
Date: 24.09.19 
"""

import os, re, string
import nltk
nltk_data_path = os.path.abspath(
    os.path.join(os.getcwd(), os.pardir, os.pardir, "data/external/nltk_data"))
nltk.data.path.append(nltk_data_path)
from nltk import word_tokenize


def remove_columns(df):
    """ Remove unnecessary columns.

    :param df: DF of raw data
    :return: DF
    """
    remove_cols = ["userID", "id", "replyToID", "replyToUser", "origPost",
                   "Level", "Sampled", "CommentsAvail", "RepAvail", "FileNum",
                   "verified", "quoted_text", "quoted_screen_name",
                   "quoted_status_id", "quoted_favorite_count",
                   "quoted_retweet_count", "quoted_verified", "retweet_text",
                   "retweet_screen_name", "retweet_status_id",
                   "retweet_favorite_count", "retweet_retweet_count",
                   "retweet_verified", "source", "replyLevel"]

    return df.drop(remove_cols, axis=1)


def clean_text(text):
    """ Util: Cleans text string.
    > Lowercase string
    > Replace game scores with "GAME_SCORE" placeholder
    > Punctuation removal
    > Replace numbers with "NUM" placeholder

    :param text: raw text string
    :return: cleaned text string
    """
    lowercased = text.lower()
    scores_removed = re.sub(r"(\d+) ?(-|:) ?(\d+)", "GAME_SCORE ", lowercased)
    punctuations = string.punctuation + "„" + "”"
    punct_removed = scores_removed.translate(str.maketrans("", "", punctuations))
    num_replaced = re.sub(r"\b\d+\b", "NUM ", punct_removed)

    return num_replaced


def preprocess_data(df):
    """ Preprocess data.
    > Removes rows w/o text
    > Removes columns, which are unnecessary for further modeling
    > Cleans text string
    > Tokenize text string

    :param df: raw DF
    :return: cleaned DF
    """
    # Remove rows w/o text
    data = df.dropna(subset=["text"], how="all")

    # Remove unnecessary cols
    cleaned_data = remove_columns(data)

    # Clean text
    cleaned_data["cleaned_text"] = cleaned_data.text.apply(clean_text)

    # Tokenize text
    cleaned_data["tokens"] = cleaned_data.cleaned_text.apply(word_tokenize)

    return cleaned_data
