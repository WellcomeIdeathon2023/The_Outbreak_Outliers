mainPanel(
  
  h2("About us"),
  p("Wel(l)come to our app. We are The Outbreak Outliers, a group of MSc Health Data Science at
    the London School of Hygiene and Tropical Medicine. We are Dzan Ahmed Jesenkovic, Gabriel Battcock, Oliver Dolin,
    Szymon Jakobsze, Walter Muruet Gutierrez"),
  
  p("Our app gives insight to vaccine hesitancy. This is achieved through a combination of topic modelling and
     sentiment analysis. This splits the tweets into predefined topics, and then classifies them into being of
     positive, negative or neutral sentiment. 
     \n
     \n
     Topics have been chosen from previous literature about reasons for vaccine hesitancy 
     e.g. safety concerns and mistrust issues. A key word regular expression search was conducted on the data set 
     of tweets for each filter. 
     \n\n
     Basic sentiment analysis has been conducted using Vader analysis (https://github.com/cjhutto/vaderSentiment)
     which is designed to accurately analyze and interpret sentiments expressed in social media platforms. 
     \n\n
     The number of tweets by each category have been aggreagted by the date of tweet. To account for volitile nature
     of the date, a 7 day rolling average was applied to each group. 
     \n\n
     Visualisation have been plotted using ggplot2, a package in R. A time-series of each topic has been plotted,
     with a sidebar slider to set boundaries for beginning and start dates. 
     
     
    "
    )
)