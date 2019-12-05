#!/usr/bin/python
""" spacy_tokenizer

Spacy-based tokenizers.

Author: datadonk23
Date: 13.11.19
"""

import spacy
from spacymoji import Emoji
import nltk
nltk.data.path.append("../../data/external/nltk_data")
from nltk.corpus import stopwords
from sklearn.feature_extraction.stop_words import ENGLISH_STOP_WORDS


stopwords_eng = set(stopwords.words("english")).union(set(ENGLISH_STOP_WORDS))

nlp = spacy.load("de_core_news_md", disable=["ner", "parser", "tagger"])
emoji = Emoji(nlp, merge_spans=False)
nlp.add_pipe(emoji, first=True)


def spacy_tokenizer(doc):
    """ Custom Spacy-based tokenizer for Sentiment Analysis Prototyping. """
    tokens = [token for token in nlp(doc)]
    cleaned_tokens = []
    for token in tokens:
        if not token.is_punct:
            if not token.is_digit:
                if not token.is_space:
                    if not token.like_url:
                        if not token.like_num:
                            if not token.like_email:
                                if not token.is_stop:
                                    if len(token) > 3 or token._.is_emoji:
                                        cleaned_tokens.append(token)

    return [token.orth_ for token in cleaned_tokens]


def plain_tokenizer(doc):
    """ Plain tokens. """
    return [token.orth_ for token in nlp(doc)]


def topic_tokenizer(doc):
    """ Custom tokenizer for topic modeling. Cleans tokens & returns lemmas. """
    tokens = [token for token in nlp(doc)]
    cleaned_tokens = []
    for token in tokens:
        if not token.is_punct:
            if not token.is_digit:
                if not token.is_space:
                    if not token.like_url:
                        if not token.like_num:
                            if not token.like_email:
                                if not token.is_stop:
                                    if not token.is_currency:
                                        if not token._.is_emoji:
                                            if len(token) > 1:
                                                if "@anonymer_user" in token.orth_:
                                                    pass
                                                else:
                                                    cleaned_tokens.append(token)

    return [token.lemma_ for token in cleaned_tokens]


def topic_tokenizer2(doc):
    """ Custom tokenizer for topic modeling. Same as topic_tokenizer,
    but removes all @[USER] pattern and english stopwords. Cleans tokens &
    returns lemmas. """
    tokens = [token for token in nlp(doc)]
    cleaned_tokens = []
    for token in tokens:
        if not token.is_punct:
            if not token.is_digit:
                if not token.is_space:
                    if not token.like_url:
                        if not token.like_num:
                            if not token.like_email:
                                if not token.is_stop:
                                    if not token.is_currency:
                                        if not token._.is_emoji:
                                            if len(token) > 1:
                                                # remove all @[user]
                                                if "@" in token.orth_:
                                                    pass
                                                # remove english stopwords
                                                elif token.orth_ in stopwords_eng:
                                                    pass
                                                else:
                                                    cleaned_tokens.append(token)

    return [token.lemma_ for token in cleaned_tokens]
