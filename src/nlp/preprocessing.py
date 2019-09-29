#!/usr/bin/python
""" preprocessing

Combines DFs of raw Posts and Comments. Cleans and tokenizes text in
resulting DF.

Author: datadonk23
Date: 29.09.19
"""

import logging
logging.basicConfig(level=logging.INFO)

from nlp.datasets_io import load_data, dump_data
from nlp.preprocess_dataset import clean_dataframe


if __name__ == "__main__":
    # FIXME DB access
    data_path = "/mnt/DATA/NRW2019 Dropbox/data 4good/CSVData"

    logging.info("Load dataset")
    raw_data = load_data(data_path)

    logging.info("Preprocess dataset")
    preprocessed_data = clean_dataframe(raw_data)

    logging.info("Persist cleaned dataset")
    dump_data(preprocessed_data, data_path)
