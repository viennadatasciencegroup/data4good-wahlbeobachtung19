#!/usr/bin/python
""" preprocess_dataset

Preprocess dataset of FB and Twitter data.

Author: datadonk23
Date: 29.09.19
"""

import os, re, string
import nltk
nltk_data_path = os.path.abspath(
    os.path.join(os.getcwd(), os.pardir, os.pardir, "data/external/nltk_data"))
nltk.data.path.append(nltk_data_path)
from nltk import word_tokenize


def clean_text(text):
    """ Util: Cleans text string.
    > Lowercase string
    > Replace game scores with "GAME_SCORE" placeholder
    > Punctuation removal
    > Replace numbers with "NUM" placeholder

    :param text: raw text string
    :type text: str
    :return: cleaned text string
    :rtype: str
    """
    lowercased = text.lower()
    scores_removed = re.sub(r"(\d+) ?(-|:) ?(\d+)", "GAME_SCORE ", lowercased)
    punctuations = string.punctuation + "„" + "”"
    punct_removed = scores_removed.translate(str.maketrans("", "",
                                                           punctuations))
    num_replaced = re.sub(r"\b\d+\b", "NUM", punct_removed)

    return num_replaced


def clean_dataframe(df):
    """ Cleans dataframe:
    > Removes rows w/o text
    > Cleans text string
    > Tokenize text string

    :param df: raw DF
    :type df: pd.DataFrame
    :return: cleaned DF
    :rtype: pd.DataFrame
    """
    # Remove rows w/o text
    data = df.dropna(subset=["text"], how="all").copy(deep=True)

    # Clean text
    data["cleaned_text"] = data.text.apply(clean_text)

    # Tokenize text
    data["tokens"] = data.cleaned_text.apply(word_tokenize)

    return data
