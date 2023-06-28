# README

This is your private repository for working on the challenges in the Wellcome Data Science Ideathon.
This repository is maintained and monitored by Wellcome staff and will be made public after July 13 2023.
Feel free to create additional folders in this repository but please use the existing ones as follows:

* `data` - Any data that is loaded from your scripts (excluding data scraped/downloaded from the web) should be uploaded to this folder. Simulated data should be reproducible.
* `code` - All code used as part of your solution should be uploaded this folder and is expected to be reproducible.
* `results` - Final results, including presented slides and other content, should be uploaded to this folder.

# Getting started
## Project structure
This project uses a standardised setup as a means to make it easier for newcomers
(as well as for our future selves, hello future me!) to understand and follow all the steps done to solve the challenge.

The structure outline here is still tentative and subject to change as the project evolves. Relevant updates to the documentation will be provided to reflect any changes to project structure

The project structure is based on the folders provided by the Wellcome Ideathon Team (Thanks Raphael) as well as the ones described in [Cookiecutter Data Science](https://drivendata.github.io/cookiecutter-data-science/#cookiecutter-data-science) and by William Noble in [PLOS Computational Biology](https://doi.org/10.1371/journal.pcbi.1000424).

A concise description of the folders and their contents can be found under APPENDIX A. 

# LICENCE

The code in this repository is licenced under a permissive [MIT licence](https://opensource.org/licenses/MIT). All other content is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). This means you may use any content in this repository as long as you credit the authors.

# APPENDICES
## APPENDIX A
├── LICENSE
├── challenge.md       <- The challenge's background, objectives and questions.
├── README.md          <- This file.
├── data
│   ├── synthetic      <- Any simulated data (if applicable).
│   ├── interim        <- Intermediate data that has been transformed.
│   ├── processed      <- The final, canonical data sets for modeling and final results.
│   └── raw            <- The original, immutable data dump.
│
├── models             <- Trained and serialized models, model predictions, or model summaries.
│
├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
│                         the creator's initials, and a short `-` delimited description, e.g.
│                         `1.0-wm-initial-data-exploration`.
│
├── references         <- Data dictionaries, manuals, and all other explanatory materials.
│
├── reports            <- Generated analyses as HTML, PDF, Markdown, etc.
│   └── figures        <- Generated graphics and figures to be used in reporting.
│
├── code               <- Source code for use in this project.
|   ├── data           <- Scripts to generate data (if applicable).
|   ├── features       <- Scripts to turn raw data into features.
|   ├── models         <- Scripts to train models and then use trained models to make
|   │                     predictions.
|   └── visualization  <- Scripts to create visualizations.
|
├── results            <- Final results and presented contents. 
