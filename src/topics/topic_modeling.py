#!/usr/bin/python
""" topic_modeling

Trains a Topic Model (LDA w/ 20 components) on Tf-idf vectorized text data and
assigns the most probable topic to the Social Media Posts.

Author: datadonk23
Date: 13.11.19
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
from nlp.spacy_tokenizer import topic_tokenizer2


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


def filter_empty_vecs(dtm, data):
    """ Removes rows which have an empty tfidf vector from doc-term matrix and
    train dataset.

    :param dtm: Tfidf doc-term matrix
    :type dtm: scipy.sparse.csr.csr_matrix
    :param data: DF of preprocessed data
    :type data: pd.DataFrame
    :return: (tfidf_dtm, data)
    :rtype: (scipy.sparse.csr.csr_matrix, pd.Dataframe)
    """
    empty_vecs = np.array(dtm.sum(axis=1) == 0).ravel()

    # Filter empty vecs from doc-term matrix and empty rows from corpus DF
    dtm_filtered = dtm[~empty_vecs]
    data.drop(data.index[empty_vecs.tolist()], inplace=True)

    return dtm_filtered, data


def print_scores(model, dtm, name):
    """ Prints Log-Likelihood and Perplexity score of model to stdout.

    :param model: fitted LDA model
    :type model: sklearn.decomposition.LatentDirichletAllocation
    :param dtm: Tfidf doc-term matrix
    :type dtm: scipy.sparse.csr.csr_matrix
    :return: -
    """
    print("Scores for", name)
    print("Log Likelihood:", model.score(dtm))
    print("Perplexity:", model.perplexity(dtm), "\n")


def visualize_model(model, tfidf, tfidf_dtm, figures_path):
    """ Visualization of LDA model. Persisted as standalone HTML file.

    :param model: fitted LDA model
    :type model: sklearn.decomposition.LatentDirichletAllocation
    :param tfidf: Tfidf vectorizer
    :type tfidf: sklearn.feature_extraction.text.TfidfVectorizer
    :param tfidf_dtm: Tfidf doc-term matrix
    :type tfidf_dtm: scipy.sparse.csr.csr_matrix
    :param figures_path: Path to persist visualization
    :type figures_path: str
    :return: -
    """
    viz_fpath = os.path.join(figures_path, "topics", "topics_final_LDAn20.html")

    p = pyLDAvis.sklearn.prepare(model, tfidf_dtm, tfidf, sort_topics=False,
                                 mds="tsne")
    pyLDAvis.save_html(p, viz_fpath)


def assign_topic(data, doc_topic_distr):
    """ Assigns dominant topic to documents of corpus.

    :param data: DF of preprocessed and filtered text data
    :type data: pd.DataFrame
    :param doc_topic_distr: Array of topic distribution per doc of corpus
    :type doc_topic_distr: np.array
    :return: DF incl assigned topics
    :rtype: pd.DataFrame
    """
    data["topic_distribution"] = doc_topic_distr.tolist()
    data["topic"] = np.argmax(doc_topic_distr, axis=1) + 1

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
    tfidf = TfidfVectorizer(lowercase=True, tokenizer=topic_tokenizer2,
                            max_features=40000)
    tfidf_dtm = tfidf.fit_transform(preprocessed_data.text)
    tfidf_dtm_filtered, filtered_data = filter_empty_vecs(tfidf_dtm,
                                                          preprocessed_data)

    logging.info("Persist vectorizer")
    dump(tfidf, os.path.join(model_path, "topic_vectorizer",
                             "final_tfidf.joblib"))
    dump(tfidf_dtm_filtered, os.path.join(model_path, "topic_vectorizer",
                                          "final_tfidf_dtm.joblib"))

    logging.info("Modeling")
    model = LatentDirichletAllocation(n_components=20, n_jobs=1,
                                      random_state=random_state, verbose=1)
    doc_topic_distr = model.fit_transform(tfidf_dtm_filtered)

    logging.info("Persist model")
    dump(model, os.path.join(model_path, "topic_lda", "final_LDAn20.joblib"))

    logging.info("Evaluate model")
    print_scores(model, tfidf_dtm_filtered, "LDA_n20")
    visualize_model(model, tfidf, tfidf_dtm_filtered, figures_path)

    logging.info("Assign topics to documents")
    topic_data = assign_topic(filtered_data, doc_topic_distr)

    logging.info("Persist dataset")
    save_data(topic_data, data_path, "final_topics_LDAn20")
