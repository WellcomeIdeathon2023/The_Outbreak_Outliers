mainPanel(
  
  h2("About us"),
  p("Wel(l)come to our app. We are The Outbreak Outliers, a group of MSc Health Data Science students at
    the London School of Hygiene and Tropical Medicine (LSHTM). We are Dzan Ahmed Jesenkovic, Gabriel Battcock, Oliver Dolin,
    Szymon Jakobsze, Walter Muruet Gutierrez"),
  
  p("Our app is designed to track the number of tweets expressing attitudes that have been shown in previous work to increase
    vaccine hesitancy. Vaccine hesitancy refers to delay in acceptance or refusal of vaccination despite availability of 
    vaccination services. We identify these attitudes by running tweets through a series of filters. These filters 'detect' the
    presence of a topic, i.e., vaccination, in a particular tweet by comparing the words in the tweet with a list of keywords related 
    to that topic (e.g., 'vaccine', 'jab').
    \n
    \n
    Tweets that pass a 'vaccination' filter are passed on to a 'hesitancy' filter, that is intended to detect attitudes expressing
    hesitancy. These tweets are then passed to more specific filters to detect specific attitudes contributing to hesitancy, like concerns
    about the safety of the vaccine (safety filter).
    
    \n
    \n
    Sentiment analysis describes a computational process for determining whether a writer's attitude towards a particular topic
    is positive, negative, or neutral. In our app, we use an existing validated sentiment analysis tool, Vader analysis (https://github.com/cjhutto/vaderSentiment),
    to determine the sentiment of tweets related to vaccination, as well as the sentiment of tweets that express a particular attitude (e.g., mention safety concerns surrounding the vaccine).
    
    \n
    \n
    The 'Overview' page 
    
    \n
    \n
    The 'Sentiment' 
     The number of tweets by each category have been aggreagted by the date of tweet. To account for volitile nature
     of the date, a 7 day rolling average was applied to each group. 
     \n\n
     Visualisation have been plotted using ggplot2, a package in R. A time-series of each topic has been plotted,
     with a sidebar slider to set boundaries for beginning and start dates.
    "
    )
)