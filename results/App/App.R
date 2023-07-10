#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#------------------ Install Required Packages ----------------------------------
req_pkgs <- function(){
    
    pkg_list <- list(
        .shiny         = c('shiny', 'bslib', 'thematic', 'bsicons'),
        .utils         = c('here', 'magrittr'),
        .rprogramming  = c('rlang', 'purrr'),
        .dataintake     = c('readr'),
        .datawrangling = c('dplyr', 'forcats', 'janitor','lubridate', 
                           'tidyr', 'zoo'),
        .lin_modelling = c('segmented'),
        .visualisations = c('ggplot2', 'ggrepel', 'wordcloud')
    )
    
    to_install <- setdiff(unlist(pkg_list), installed.packages())
    
    if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
}
req_pkgs()

#------------------ Load Packages into Namespace --------------------------------

library(shiny)
library(bslib)
library(magrittr)

#------------------ Source App functions ---------------------------------------
here::here('results', 'App', 'app_code') %>%
    list.files(pattern = '[.]R$', full.names = TRUE) %>%
    purrr::walk(source)

#-------------------------------------------------------------------------------
#Data
## Base dataset
tweets_df <- 
  here::here('results', 'App', 'app_data', 'tweets_df.csv') %>%
  load_base_dataset() %>%
  add_date_aggregations(date)

all_hashtags <- 
  tweets_df %>%
  dplyr::pull(hashtags) %>%
  explode_python_lists() %>%
  stringr::str_to_lower()

# Main dataset
# tweets_df <- 
#   here::here('results', 'App', 'app_data', 'tweets.rds') %>%
#   readr::read_rds()
# 
# # Tweet sentiment
# tweets <- 
#     here::here('results','App', 'app_data', 'tweets.rds') %>%
#     readRDS()
# 
# # Prepare the transformed data
# sentiment_df <- 
#     tweets %>% 
#     dplyr::group_by(date, VADER_label) %>%
#     dplyr::summarise(count = dplyr::n(), .groups = 'drop')
# 
# 
# topic_df <- 
#     tweets %>%
#     dplyr::group_by(date, topic) %>%
#     dplyr::summarise(count = dplyr::n(), .groups = 'drop')
# 
# 
# events_df <- data.frame(
#     event = c("WHO approve emergency BioNTech vaccine in Netherlands",
#               "COVISHIELD approved for use",
#               "Moderna vaccine approved for use in EU"),
#     date = as.Date(c(
#         "2020-12-18",
#         "2021-02-15",
#         "2021-04-30")))
# 
# min_tweet_date = min(lubridate::ymd(tweets$date), na.rm = TRUE)
# max_tweet_date = max(lubridate::ymd(tweets$date), na.rm = TRUE)
# Sentiment data ---------------------------------------------------------------
# Read in the sentiment time series
# Todo: Allow to get sentiment analyses by different methods
sentiment_ts <- 
    here::here('results','App','app_data', 'sentiment_timeseries_basic.csv') %>%
    readr::read_csv(
        col_types = readr::cols_only(
            date = readr::col_date(format = '%Y-%m-%d %H:%M:%S'),
            sentiment_vader = readr::col_integer()),
        show_col_types = FALSE,
        progress = FALSE) %>%
    dplyr::arrange(date)

# Create columns to allow the user change the aggregation level
sentiment_ts <- 
    sentiment_ts %>%
    dplyr::transmute(
        sentiment = sentiment_vader,
        Daily = date,
        Weekly = cut.Date(date, breaks = 'week', start.on.monday = TRUE),
        Weekly = as.Date(Weekly),
        Fortnightly = cut.Date(date, breaks = '2 weeks', start.on.monday = TRUE),
        Fortnightly = as.Date(Fortnightly),
        Monthly = cut.Date(date, breaks = '1 month', start.on.monday = TRUE),
        Monthly = as.Date(Monthly))

# Read in sentiment forecast
sentiment_fc <- 
    here::here('results', 'App', 'app_data', 'sentiment_timeseries_forecast.csv') %>%
    readr::read_csv(
        col_types = readr::cols(
            date = readr::col_date(),
            sentiment_vader = readr::col_double()),
        show_col_types = FALSE,
        progress = FALSE) %>%
    dplyr::arrange(date)

sentiment_fc <-
    sentiment_fc %>%
    dplyr::rename(sentiment = sentiment_vader,
                  Daily = date) %>%
    dplyr::mutate(.forcasted = TRUE)

fcst_sntmnt <- 
    dplyr::bind_rows(
        sentiment_ts %>%
            dplyr::group_by(Daily) %>%
            dplyr::summarise(sentiment = mean(sentiment, na.rm = TRUE)) %>%
            dplyr::mutate(.forcasted = FALSE),
        sentiment_fc) %>%
    dplyr::arrange(Daily)
    

# variables for change point regression
.twts_med_date <- sentiment_ts %>% dplyr::pull(Daily) %>% median(na.rm = TRUE)

# Topic Modelling --------------------------------------------------------------
tm_df <- 
    here::here('results', 'App', 'app_data', 'topic_modelling.csv') %>%
    readr::read_csv(col_types = readr::cols(
        tweet_id = readr::col_integer(),   
        `0` = readr::col_double(),   
        `1` = readr::col_double(),   
        `2` = readr::col_double(),  
        `3` = readr::col_double(),
        `4` = readr::col_double(),  
        `5` = readr::col_double(),
        # topic = readr::col_factor(levels = c('0', '1', '2', '3', '4', '5'),
        #                           include_na = FALSE),
        topic = readr::col_integer(),
        date = readr::col_date(format = '%Y-%m-%d %H:%M:%S'),               
        tknzd_words = readr::col_character()
    )) %>%
    dplyr::mutate(
        topic = dplyr::case_when(
          topic %in% c(0,5) ~ 'Vaccination slots',
          topic == 1 ~ 'News',
          topic == 2 ~ 'Stats updates',
          topic == 3 ~ 'Vaccine hesistancy',
          topic == 4 ~ 'Vacine support'),
        topic = factor(topic),
        tknzd_words = stringr::str_remove_all(tknzd_words, '^\\[|\\]$')) %>%
    janitor::clean_names()

.words_per_topic <- get_topic_words(tm_df, topic, tknzd_words)


#--------------------------- Graphic Settings ----------------------------------
# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default theme
ggplot2::theme_set(ggplot2::theme_dark())

#-------------------------------User interface ---------------------------------
# Side bar ---------------------------------------------------------------------
# side_bar

# sidebar options
.twts_date_lwr <- sentiment_ts %>% dplyr::pull(Daily) %>% min(na.rm = TRUE)
.twts_date_upr <- sentiment_ts %>% dplyr::pull(Daily) %>% max(na.rm = TRUE)

# Pages ------------------------------------------------------------------------
# Summary
## Total tweets vs tweets scrapped
.box_total_tweets <- 
  value_box(
    title = 'Number of tweets scrapped (total)',
    value = '100,000',
    showcase = bsicons::bs_icon('chat'),
    theme_color = 'danger',
    height = '120px')

.box_relevant_tweets <- 
  value_box(
    title = 'Number of vaccine related tweets',
    value = prettyNum(nrow(tweets_df), big.mark = ','),
    showcase = bsicons::bs_icon('chat-left-dots'),
    theme_color = 'success',
    height = '120px')

.cols_tweet_nums_boxes <- 
  layout_column_wrap(
    width = "150px",
    height = "120px",
    fill = FALSE,
    .box_total_tweets,
    .box_relevant_tweets)

## Tweets timeline
.card_twts_timeline <- 
  card(
    full_screen = TRUE,
    card_header('How many tweets per day?'),
    plotOutput('tweet_ts')
  )

.card_top_words <- 
  card(
    full_screen = TRUE,
    card_header('What was being said?'),
    plotOutput('wcloud_all_words')
  )

.card_usr_location <- 
  card(
    full_screen = TRUE,
    card_header('Where are the tweets coming from (If known)?'),
    plotOutput('usr_loc')
  )

.card_top_hashtags <- 
  card(
    full_screen = TRUE,
    card_header('What was trending?'),
    plotOutput('wcloud_all_hashtags')
  )

  
.cols_summary_bottom <- 
  layout_column_wrap(
    width = "150px",
    height = "120px",
    fill = TRUE,
    .card_usr_location,
    .card_usr_location,
    .card_top_words)

## Summary page nav
.panel_summary <- 
  nav_panel(
  title = 'Summary',
  .cols_tweet_nums_boxes,
  .card_twts_timeline,
  .cols_summary_bottom
  )


# Sentiment change overtime
.card_sentiment_brkpoint <- 
    card(
        full_screen = TRUE,
        card_header('Overall sentiment change'),
        plotOutput("chngpt_plot")
    )
.card_sentiment_frcst <- 
    card(
        full_screen = TRUE,
        card_header('Predicted sentiment\n(Next 30 days)'),
        plotOutput("frcst_plot")
    )

# Topic modelling
## wordclouds
.card_tm_wordcloud <-
  card(
    full_screen = TRUE,
    card_header('Top words in topic'),
    plotOutput('tm_wordcloud'))

.card_tm_wordclouds_1 <-
    card(
        full_screen = TRUE,
        card_header('Most common words (Topic 1)'),
        plotOutput('wordcloud1'))
.card_tm_wordclouds_2 <-
    card(
        full_screen = TRUE,
        card_header('Most common words (Topic 2)'),
        plotOutput('wordcloud2'))
.card_tm_wordclouds_3 <-
    card(
        full_screen = TRUE,
        card_header('Most common words (Topic 3)'),
        plotOutput('wordcloud3'))
.card_tm_wordclouds_4 <-
    card(
        full_screen = TRUE,
        card_header('Most common words (Topic 4)'),
        plotOutput('wordcloud4'))
.card_tm_wordclouds_5 <-
    card(
        full_screen = TRUE,
        card_header('Most common words (Topic 5)'),
        plotOutput('wordcloud5'))
.card_tm_wordclouds_6 <-
    card(
        full_screen = TRUE,
        card_header('Most common words (Topic 6)'),
        plotOutput('wordcloud6'))

word_clouds <- 
    list(
        .card_tm_wordclouds_1,
        .card_tm_wordclouds_2,
        .card_tm_wordclouds_3,
        .card_tm_wordclouds_4,
        .card_tm_wordclouds_5,
        .card_tm_wordclouds_6)
# Topic counts
.card_tm_freqs <- 
    card(
        full_screen = TRUE,
        card_header('Topics (Counts)'),
        plotOutput('tm_freqs')
    )



# Full UI ----------------------------------------------------------------------
ui <- 
    page_navbar(
        title = 'The Outbreak Outliers',
        .panel_summary,
        nav_panel(
            title = 'Sentiment',
            .card_sentiment_brkpoint,
            .card_sentiment_frcst
            ),
        nav_panel(
            title = 'Topics',
            layout_columns(.card_tm_wordcloud, .card_tm_freqs)
        ),
        nav_panel(
          title = 'Forecast'
        ),
        nav_panel(
          title = 'About'
        ),
        theme = bs_theme(
            bootswatch = "darkly",
            base_font = font_google("Inter"),
            navbar_bg = "#2d2d2d"
        ))

# Define server logic required to draw plots
server <- function(input, output, session) {
    
  output$tweet_ts <- 
    renderPlot(expr = {
      plot_tweets_ts(tweets_df)
    })
  
  output$usr_loc <- 
    renderPlot(expr = {
      tweets_df %>%
        dplyr::filter(!user_location %in% c('Other', 'Missing', 'Global')) %>%
        plot_location_frequencies()
    })
  
  output$wcloud_all_hashtags <- 
    renderPlot(expr = {
      gen_wordcloud_from_vec(all_hashtags)
    })
  
  output$chngpt_plot <- 
        renderPlot(expr = {
        plot_changepoint(sentiment_ts, 
                         Daily,
                         #!!input$chngpt_freq, 
                         .twts_date_lwr,
                         #input$chgpt_dt_range[1],
                         .twts_date_upr,
                         #input$chgpt_dt_range[2],
                         .twts_med_date
                         #input$chngpt_dt
                         )
          })
    
    output$frcst_plot <- 
        renderPlot(expr = {
            plot_sentiment_forcast(fcst_sntmnt)
        })
    
    output$wordcloud1 <- 
        renderPlot(expr = {
            gen_wordcloud_from_vec(.words_per_topic[['topic1']])
        })
    
    output$wordcloud2 <- 
        renderPlot(expr = {
            gen_wordcloud_from_vec(.words_per_topic[['topic2']])
        })
    
    output$wordcloud3 <- 
        renderPlot(expr = {
            gen_wordcloud_from_vec(.words_per_topic[['topic3']])
        })
    
    output$wordcloud4 <- 
        renderPlot(expr = {
            gen_wordcloud_from_vec(.words_per_topic[['topic4']])
        })
    
    output$wordcloud5 <- 
        renderPlot(expr = {
            gen_wordcloud_from_vec(.words_per_topic[['topic5']])
        })
    
    output$wordcloud6 <- 
        renderPlot(expr = {
            gen_wordcloud_from_vec(.words_per_topic[['topic6']])
        })
    
    output$tm_freqs <- 
        renderPlot(expr = {
            plot_topics_freq(tm_df, topic)
        })
}

shinyApp(ui, server)