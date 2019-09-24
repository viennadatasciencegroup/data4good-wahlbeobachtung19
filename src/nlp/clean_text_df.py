#!/usr/bin/python
""" clean_text_df

Generates a combined DF of FB & Twitter data. Cleans and tokenize the text of
this DF.

Author: datadonk23
Date: 24.09.19 
"""

import logging
logging.basicConfig(level=logging.INFO)

from nlp.datasets_io import load_data, dump_data
from nlp.preprocess_dataset import preprocess_data


if __name__ == "__main__":
    data_path = "/mnt/DATA/NRW2019 Dropbox/data 4good/CSVData" #FIXME DB access

    logging.info("Load dataset")
    raw_data = load_data(data_path)

    logging.info("Preprocess dataset")
    preprocessed_data = preprocess_data(raw_data)

    logging.info("Persist cleaned dataset")
    dump_data(preprocessed_data, data_path)
