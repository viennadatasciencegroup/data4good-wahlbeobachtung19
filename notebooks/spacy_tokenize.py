#!/usr/bin/python
""" spacy_tokenize

Util: Spacy tokenizer to import in NB (to avoid PickelingError in sklearn).

Author: datadonk23
Date: 15.10.19 
"""

import spacy
from spacymoji import Emoji

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
