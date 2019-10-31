#!/usr/bin/python
""" topic_modeling

Trains a Topic Model (LDA w/ 40 components) on Tf-idf vectorized text data and
assigns the most probable topic to the Social Media Posts.

Author: datadonk23
Date: 31.10.19 
"""

import os
import logging
logging.basicConfig(level=logging.INFO)
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import LatentDirichletAllocation
from joblib import dump

logging.info("Load util scripts")
from topics.topic_modeling_utils import (load_data, preprocess, save_data,
                                         print_scores, visualize_model)
from nlp.spacy_tokenizer import topic_tokenizer


if __name__ == "__main__":
    random_state = 23
    data_path = "/Data/CSVData"
    model_path = "../../models"
    figures_path = "../../reports/figures/"

    logging.info("Load dataset")
    posts = load_data(data_path)

    logging.info("Preprocess dataset")
    preprocessed_data = preprocess(posts)

    logging.info("Vectorize text")
    tfidf = TfidfVectorizer(lowercase=True, tokenizer=topic_tokenizer,
                            max_features=40000)
    tfidf_v = tfidf.fit_transform(preprocessed_data.text)

    logging.info("Persist vectorizer")
    dump(tfidf, os.path.join(model_path, "topic_vectorizer",
                             "final_tfidf.joblib"))
    dump(tfidf_v, os.path.join(model_path,
                               "topic_vectorizer", "final_tfidf_v.joblib"))

    logging.info("Modeling")
    model = LatentDirichletAllocation(n_components=40, n_jobs=1,
                                      random_state=random_state, verbose=1)
    doc_topic_distr = model.fit_transform(tfidf_v)

    logging.info("Persist model")
    dump(model, os.path.join(model_path, "topic_lda", "final_LDAn40.joblib"))

    logging.info("Evaluate model")
    print_scores(model, tfidf_v, "LDA_n40")
    visualize_model(model, tfidf, tfidf_v, figures_path)

    logging.info("Assign topics to documents")
    data = preprocessed_data.copy()
    data["topic_distribution"] = doc_topic_distr.tolist()
    data["topic"] = doc_topic_distr.argmax(axis=1)

    logging.info("Persist dataset")
    save_data(data, data_path)
