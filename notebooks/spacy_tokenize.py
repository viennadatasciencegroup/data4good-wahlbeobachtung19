#!/usr/bin/python
""" spacy_tokenize

Util: Spacy tokenizer to import in NB (to avoid PickelingError in sklearn).

Author: datadonk23
Date: 08.11.19 
"""

import spacy
from spacymoji import Emoji
import nltk
nltk.data.path.append("../data/external/nltk_data")
from nltk.corpus import stopwords
from sklearn.feature_extraction.stop_words import ENGLISH_STOP_WORDS


stopwords_eng = set(stopwords.words("english")).union(set(ENGLISH_STOP_WORDS))


nlp = spacy.load("de_core_news_md", disable=["ner", "parser", "tagger"])
emoji = Emoji(nlp, merge_spans=False)
nlp.add_pipe(emoji, first=True)

def spacy_tokenizer(doc):
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
    return [token.orth_ for token in nlp(doc)]


def topic_tokenizer(doc):
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
                                                if "@" in token.orth_: #removes all @[user]
                                                    pass
                                                elif token.orth_ in stopwords_eng:
                                                    pass
                                                else:
                                                    cleaned_tokens.append(token)
    
    return [token.lemma_ for token in cleaned_tokens]
