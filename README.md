[![Deon badge](https://img.shields.io/badge/ethics%20checklist-deon-brightgreen.svg?style=popout-square)](http://deon.drivendata.org/)

data4good-wahlbeobachtung19
==============================

Monitor and analyze Social Media campaigns of major political actors during national elections 2019 in Austria.


### Description

For the Austrian parliamentary elections held on September 29, 2019, data4good teamed up with [wahlbeobachtung.org](https://www.wahlbeobachtung.org/) to monitor the public social media profiles of the political candidates, leading influencers, and the major Austrian Press. Public Twitter and Facebook profiles were monitored from September 8 until October 4. During this period, over 26000 politicians posts, and 1.1 million generic user comments were collected. Here is the code of statistical, sentiment and topic analysis of the data.

Results are visualized and published in this [App](https://rania.shinyapps.io/exploreTrends/). 


Project Organization
------------

    ├── LICENSE
    ├── README.md          <- The top-level README for developers using this project.
    │
    ├── cache              <- Cached R data.
    │
    ├── config             <- Project configuration settings and credentials (VC excluded)
    │   ├── credentials.py       <- Needs to be created (instructions in `config/README.md`).
    │   └── global.dcf           <- Global settings in DCF format.
    │
    ├── data
    │   ├── external       <- Data from third party sources.
    │   ├── interim        <- Intermediate data that has been transformed.
    │   ├── processed      <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── environment.yml    <- Conda environment file for reproducint the analysis environment.
    │                         Generated with `env export > environment.yml`.
    │
    ├── ETHICS.md          <-Ethics checklist.
    │
    ├── lib                <- R helper functions.
    │   ├── globals.R      <- Project specific functions.
    │   └── helpers.R      <- Helper functions.
    │
    ├── logs               <- Stores log files.
    │
    ├── models             <- Trained and serialized models, model predictions, or model summaries.
    │   ├── topic_vectorizer  <- Text vecotrizers for Topic modeling.
    │   └── topic_lda         <- LDA Topic models.
    │
    ├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
    │                         the creators initials, and a short `-` delimited description, e.g.
    │                         `1.0-jqp-initial-data-exploration`.
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting.
    │         └── topics   <- Topic modeling graphics and figures.
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`.
    │
    └── src                <- Source code for use in this project.
        ├── __init__.py    <- Makes src a Python module.
        │
        ├── analytics           <- Scripts to perform result oriented text analytics. 
        │
        ├── comment_evaluation  <- Shiny R script for comment evaluation app.
        │
        ├── data           <- Scripts to download, collect, persist or generate data.
        │
        ├── nlp            <- Scripts to clean and preprocess raw text data (NLP pipeline).
        │
        ├── sentiment      <- Scripts to perform sentiment analysis.
        │
        ├── topics         <- Scripts to perform topic modeling.
        │
        ├── visualization  <- Scripts to create exploratory and results oriented visualizations.
        │
        └── test           <- Test scripts.




## Contributors

[Rania Wazir](https://github.com/rrania4r), [Thomas Treml](https://github.com/datadonK23), [Liad Magen](https://github.com/liadmagen), Martin Fenz


## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
