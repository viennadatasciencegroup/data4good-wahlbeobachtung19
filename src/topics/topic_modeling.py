#!/usr/bin/python
""" topic_modeling

Trains a Topic Model (LDA w/ 40 components) on Tf-idf vectorized text data and
assigns the most probable topic to the Social Media Posts.

Author: datadonk23
Date: 31.10.19 
"""

import os, logging, warnings
logging.basicConfig(level=logging.INFO)
warnings.filterwarnings("ignore", category=DeprecationWarning)
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import LatentDirichletAllocation
from joblib import dump
import pyLDAvis
import pyLDAvis.sklearn
import numpy as np
import pandas as pd

logging.info("Load util scripts")
from topics.topic_modeling_utils import load_data, save_data
from nlp.spacy_tokenizer import topic_tokenizer


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
    """ Visualization of LDA model. Persisted as standalone HTML file.

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


def assign_topic(data, doc_topic_distr):
    """ Assigns topic to documnents of corpus.

    :param data: DF of preprocessed text data
    :type data: pd.DataFrame
    :param doc_topic_distr: Array of topic distribution per doc of corpus
    :type doc_topic_distr: np.array
    :return: DF incl assigned topics
    :rtype: pd.DataFrame
    """
    data["topic_distribution"] = doc_topic_distr.tolist()
    data["topic"] = doc_topic_distr.argmax(axis=1)

    return data


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
    topic_data = assign_topic(preprocessed_data, doc_topic_distr)

    logging.info("Persist dataset")
    save_data(topic_data, data_path)
