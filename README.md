# Wellcome Ideathon 2023

**The Outbreak Outliers** - London School of Hygiene and Tropical Medicine

## Noticeboard

-   **1st of July 2023**
    -   You can now find `vax_tweets_v0.RDS`
        -   This is a serialised version of the original data set with the following changes:
            -   The following columns are added:
                -   `tweet_id`, `roberta_loc_score`, `roberta_loc_guess`, `distilBERT_sentiment`, `distilBERT_score`
            -   Three rows with only missing values were removed
            -   The `date` column is already formatted as date
            -   Square brackets were removed from the `hashtags` column
        -   You can find it in data \> interim \> `vax_tweets_v0.RDS`
-   **30th of June 2023**
    -   You can find the initial data inspection analysis as jupyter notebook [here](https://github.com/dzanahmed/welcome-ideathon-lshtm/blob/main/notebooks/2023-06-29-wm-initial_data_inspection.ipynb)
    -   PDF and HTML versions can be found on the [Google Drive](https://drive.google.com/drive/folders/1KqvLO_R7nze59G0euTbrdTeu7DDfmTak)
        -   Note that the HTML version may require downloading the files first.

## Quick Start

All documentation is available at our [Google Drive](https://drive.google.com/drive/folders/1ZrdLlu4LGe-u2xZihLHmbji46DL9KnZS?usp=sharing).\
Also check the references folder

## About

### This Repository

Github repo for team The Outbreak Outliers (MSc HDS \@ LSTHM) - Wellcome Ideathon 2023

The project's layout is based on the requirements from Wellcome and the suggestions made in [Cookiecutter Data Science](https://drivendata.github.io/cookiecutter-data-science/#cookiecutter-data-science) and in the paper by William Noble in [PLOS Computational Biology](https://doi.org/10.1371/journal.pcbi.1000424). See Appendix A bellow for a description about what each folder is meant for.

### Us

[Gabriel](https://github.com/gabrielbattcock) - Future Bollywood star in the making

[Oliver](https://github.com/oliverodolin)

[Szymon](https://github.com/vvitomino)

[Dzan](https://github.com/dzanahmed)

[Walter](https://github.com/Walter-Muruet)

## APPENDICES

### APPENDIX A

├── LICENSE ├── challenge.md \<- The challenge's background, objectives and questions.\
├── README.md \<- This file.\
\|\
├── code \<- Source code for use in this project.\
├── data \<- Scripts to generate simulated data (if applicable).\
├── features \<- Scripts to turn raw data into features.\
├── models \<- Scripts to train models and then use trained models to make\
│ predictions.\
└── visualization \<- Scripts to create visualizations.\
\|\
├── data\
├── synthetic \<- Any simulated data (if applicable).\
├── interim \<- Intermediate data that has been transformed.\
├── processed \<- The final, canonical data sets for modeling and final results.\
└── raw \<- The original, immutable data dump.\
│\
├── models \<- Trained and serialized models, model predictions, or model summaries.\
│\
├── notebooks \<- Jupyter notebooks. Naming convention is a number (for ordering),\
│ the creator's initials, and a short `-` delimited description, e.g.\
│ `1.0-wm-initial-data-exploration`.\
│\
├── references \<- Data dictionaries, manuals, and all other explanatory materials.\
│\
├── reports \<- Generated analyses as HTML, PDF, Markdown, etc.\
└── figures \<- Generated graphics and figures to be used in reporting.\
\|\
├── results \<- Final results and presented contents.
