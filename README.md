# :large_orange_diamond: Wellcome Ideathon 2023

### :chart: **The Outbreak Outliers** - London School of Hygiene and Tropical Medicine


## :blue_book: Quick Start

To get the app running, source `runApp('code/app/vaxwatch_app.R')` and execute from command line, or by opening the file in Rstudio and running a Shiny app. 
This Shiny-based app will display dashboards that represent our platform and final product. 

## 	:notebook_with_decorative_cover: About

### :chart_with_upwards_trend: Presentation

Presentation is available as PDF: [VAXWATCH.pdf](https://github.com/WellcomeIdeathon2023/The_Outbreak_Outliers/blob/main/VAXWATCH.pdf)

### :computer: This Repository

Github repo for team The Outbreak Outliers (MSc HDS \@ LSTHM) - Wellcome Ideathon 2023

Our app is designed to track the number of tweets expressing attitudes that have been shown in previous work to increase vaccine hesitancy. Vaccine hesitancy refers to delay in acceptance or refusal of vaccination despite availability of vaccination services. We identify these attitudes by running tweets through a series of filters. These filters 'detect' the presence of a topic, i.e., vaccination, in a particular tweet by comparing the words in the tweet with a list of keywords related to that topic (e.g., 'vaccine', 'jab').

Tweets that pass a 'vaccination' filter are passed on to a 'hesitancy' filter, that is intended to detect attitudes expressing hesitancy. These tweets are then passed to more specific filters to detect specific attitudes contributing to hesitancy, like concerns about the safety of the vaccine (safety filter). The number of tweets passing through each filter, displayed on the 'Overview' page, have been aggregated by the date of tweet. A time-series of each topic has been plotted, with a sidebar slider to set boundaries for beginning and start dates. To account for the volatile nature of the date, a 7 day rolling average was applied to each group. All visualisations were plotted using ggplot2, a package in R.

Sentiment analysis describes a computational process for determining whether a writer's attitude towards a particular topic is positive, negative, or neutral. In our app, we use an existing validated sentiment analysis tool, Vader analysis (https://github.com/cjhutto/vaderSentiment), to determine the sentiment of tweets related to vaccination, as well as the sentiment of tweets that express a particular attitude (e.g., mention safety concerns surrounding the vaccine).

The project's layout is based on the requirements from Wellcome and the suggestions made in [Cookiecutter Data Science](https://drivendata.github.io/cookiecutter-data-science/#cookiecutter-data-science) and in the paper by William Noble in [PLOS Computational Biology](https://doi.org/10.1371/journal.pcbi.1000424). 

### :man_student: The Team

[Gabriel](https://github.com/gabrielbattcock)

[Oliver](https://github.com/oliverodolin)

[Szymon](https://github.com/vvitomino)

[Dzan](https://github.com/dzanahmed)

[Walter](https://github.com/Walter-Muruet)
