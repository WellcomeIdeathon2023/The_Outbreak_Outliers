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
                           'stringr', 'tidyr', 'zoo'),
        .lin_modelling = c('segmented'),
        .visualisations = c('ggplot2', 'ggrepel', "webr", 'wordcloud')
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

all_words <- 
  tweets_df %>%
  dplyr::pull(words) %>%
  explode_python_lists() %>%
  stringr::str_to_lower()


# SENTIMENT --------------------------------------------------------------------

# Sentiment - Tweets----
tweets <- 
    here::here('results', 'App', 'app_data', 'tweets.rds') %>% 
    readr::read_rds()

# Sentiment - Sentiment ----
vader_df <-
    here::here('results', 'App', 'app_data', 'tweets.rds') %>% 
    load_sentiment_counts()

textblob_df <- 
    here::here('results', 'App', 'app_data', 'textblob_sentiment_analysis.csv') %>%
    load_textblob_subjectivity()

VADERblob_df <- 
    dplyr::left_join(
        tweets %>%
            dplyr::select(
                tweet_id,
                date,
                sentiment = VADER_label), 
        textblob_df %>%
            dplyr::select(-score), 
        by = "tweet_id")


# MENTIONS ---------------------------------------------------------------------
# Mentions - vaccine uptake----
vacmentions_df <- 
    here::here('results', 'App', 'app_data', 'vaccine_intent.csv') %>%
    load_vaccination_intent() %>%
    add_date_aggregations(date)

# Mentions - change in sentiment----   
sentiment_ts <- 
    here::here('results','App','app_data', 'sentiment_timeseries_basic.csv') %>%
    readr::read_csv(
        col_types = readr::cols_only(
            date = readr::col_date(format = '%Y-%m-%d %H:%M:%S'),
            sentiment_vader = readr::col_integer()),
        show_col_types = FALSE,
        progress = FALSE) %>%
    dplyr::arrange(date)

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

# Mentions - topics----  
tm_df <- 
    here::here('results', 'App', 'app_data', 'topic_modelling.csv') %>% 
    load_topic_clusters()



events <- data.frame(
    event = c("WHO approve emergency BioNTech vaccine in Netherlands",
              "COVISHIELD approved for use",
              "Moderna vaccine approved for use in EU"
    ),
    date = as.Date(c(
        "2020-12-18",
        "2021-02-15",
        "2021-04-30"
    )
    )
)



# FORECAST ---------------------------------------------------------------------
# Forecast - sentiment ----
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
    

#--------------------------- GRAPHIC SETTINGS ----------------------------------
# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default theme
ggplot2::theme_set(ggplot2::theme_dark())

#------------------------------- USER INTERFACE --------------------------------
# Side bar ---------------------------------------------------------------------
# sidebar options

## Date range
.twts_date_lwr <- sentiment_ts %>% dplyr::pull(Daily) %>% min(na.rm = TRUE)
.twts_date_upr <- sentiment_ts %>% dplyr::pull(Daily) %>% max(na.rm = TRUE)

## Starting change point (NOT LONGER IMPLEMENTED - may remove)
.twts_med_date <- sentiment_ts %>% dplyr::pull(Daily) %>% median(na.rm = TRUE)

## Full sidebar
app_sidebar <- 
    sidebar(
    title = "Date range",
    sliderInput(
        inputId = "date_range",
        label = "Time scope",
        min = .twts_date_lwr,
        max = .twts_date_upr,
        value = c(.twts_date_lwr, .twts_date_upr)
    )
)

# App pages --------------------------------------------------------------------
# Summary page ----
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
    height = "80px",
    fill = FALSE,
    .box_total_tweets,
    .box_relevant_tweets)

## Tweets timeline
.card_twts_timeline <- 
  card(
    full_screen = TRUE,
    card_header('How many tweets per day?'),
    plotOutput('sum_tweet_ts')
  )

## User location
.card_usr_location <- 
  card(
    full_screen = TRUE,
    card_header('Where were the tweets coming from (if known)?'),
    plotOutput('sum_usr_loc')
  )

## Full Summary Panel
.panel_summary <- 
  nav_panel(
  title = 'Summary',
  .cols_tweet_nums_boxes,
  .card_twts_timeline,
  .card_usr_location
  )

#-------------------------------------------------------------------------------
# Sentiment page----
## Sentiment timeline
.card_sentiment_ts <- 
    card(
    full_screen = TRUE,
    status = "primary",
    card_header("How many tweets where positive/negative/neutral?"),
    plotOutput("sntmnt_ts")
)

## Bottom row
### Sentiment-subjectivity wheel
.card_sentiment_subj_wheel <- 
    card(
        full_screen = TRUE,
        status = "primary",
        card_header('How objective were the tweets?'),
        plotOutput("sntmnt_subj")
    )

### Emotion radial plot
.card_sentiment_emo_rad <- 
    card(
        full_screen = TRUE,
        status = "primary",
        card_header('Emotions (to add)')
    )

.cols_sntmn <- 
    layout_column_wrap(
    width = "150px",
    height = "360px",
    fill = TRUE,
    .card_sentiment_subj_wheel,
    .card_sentiment_emo_rad)

## Full Sentiment Panel
.panel_sentiment <- 
    nav_panel(
    title = 'Sentiment',
    .card_sentiment_ts,
    .cols_sntmn
)
#-------------------------------------------------------------------------------
# Change in sentiment ----
## Sentiment change overtime
.card_sentiment_changepoints <- 
    card(
        full_screen = TRUE,
        card_header('How overall sentiment changed over time?'),
        plotOutput("mntns_changeplot")
    )

## Full change in sentiment panel
.panel_chng_sntmnt <- 
    nav_panel(
        title = 'Change in sentiment',
        .card_sentiment_changepoints)

#-------------------------------------------------------------------------------
# Mentions ----
## Vaccination uptake mentions
.card_uptake_mentions <- 
    card(
        full_screen = TRUE,
        card_header("How many tweets were about getting vaccinated?"),
        plotOutput("mntns_uptake")
    )

## Topic
.card_topics_pie <- 
    card(
        full_screen = TRUE,
        card_header('What topics were the most tweeted?'),
        plotOutput("mntns_topics"))

.card_topics_cloud <- 
    card(
        full_screen = TRUE,
        card_header('What words were the most tweeted?'),
        plotOutput("mntns_wordcloud"))

.cols_mntns <- 
    layout_column_wrap(
        width = "150px",
        height = "360px",
        fill = TRUE,
        .card_topics_pie,
        .card_topics_cloud)

## Full mentions panel
.panel_mentions <- 
    nav_panel(
        title = 'Mentions',
        .cols_mntns,
        .card_uptake_mentions)

#-------------------------------------------------------------------------------
# Forecast ----
## Sentiment forecast
.card_sentiment_frcst <- 
    card(
        full_screen = TRUE,
        card_header('What is the expected sentiment in the next 30 days?'),
        plotOutput("frcst_plot")
    )

## Variable importance
.card_varImp_frcst <- 
    card(
        full_screen = TRUE,
        card_header('What variables were the most important by the algorithm?'),
        card_body(
            markdown('Coming soon . . .')
            )
    )

## Full forecast panel
.panel_forecast <- 
    nav_panel(
        title = 'Forecast',
        .card_sentiment_frcst,
        .card_varImp_frcst
    )

#-------------------------------------------------------------------------------
# Methods ----

# Filtering
.card_methods_filtering <- 
    card(
        full_screen = TRUE,
        card_header('How were tweets filtered?')
    )

## Sentiment analysis
.card_methods_sentiment <- 
    card(
        full_screen = TRUE,
        card_header('How was sentiment analysed?')
    )

## ML/DL methods
.card_methods_mldl <-
    card(
        full_screen = TRUE,
        card_header('What algorithms we used?')
    )

## Full methods panel
.panel_methods <-
    nav_panel(
    title = 'Methods',
    .card_methods_filtering,
    .card_methods_sentiment,
    .card_methods_mldl
    )

#-------------------------------------------------------------------------------
# About us ----

## About the team
.card_about_team <- 
    card(
        card_header('The Outbreak Outliers'),
        card_body(
            markdown(
            'London School of Hygiene and Tropical Medicine  
            MSc Health Data Science  
            
                     Team Members:  
                     - Gabriel  
                     - Oliver  
                     - Szymon  
                     - Dzan  
                     - Walter')
            )
    )

## Full about us panel
.panel_about <-
    nav_panel(
    title = 'About',
    .card_about_team
)

# Full UI ----------------------------------------------------------------------
ui <- 
    page_navbar(
        title = 'The Outbreak Outliers',
        sidebar = app_sidebar,
        .panel_summary,
        .panel_sentiment,
        .panel_chng_sntmnt,
        .panel_mentions,
        .panel_forecast,
        .panel_methods,
        .panel_about,
        theme = bs_theme(
            bootswatch = "darkly",
            base_font = font_google("Inter"),
            navbar_bg = "#2d2d2d"
        ))

# Define server logic required to draw plots
server <- function(input, output, session) {
    
# Summary plots ----------------------------------------------------------------
    
    output$sum_tweet_ts <- 
    renderPlot(expr = {
      plot_tweets_ts(tweets_df)
    })
  
  output$sum_usr_loc <- 
    renderPlot(expr = {
      tweets_df %>%
        dplyr::filter(!user_location %in% c('Other', 'Missing', 'Global')) %>%
        plot_location_frequencies(
            .date_range_lwr = input$date_range[1],
            .date_range_upr = input$date_range[2])
    })
  
# Sentiment plots --------------------------------------------------------------
  output$sntmnt_ts <- renderPlot({
      plot_sentiment_ts(vader_df,
                        .date_range_lwr = input$date_range[1],
                        .date_range_upr = input$date_range[2])
  })
  
  output$sntmnt_subj <- renderPlot({
      plot_textblob(VADERblob_df,
                    .date_range_lwr = input$date_range[1],
                    .date_range_upr = input$date_range[2])
  })
  
# Mentions plots ---------------------------------------------------------------
  
  output$mntns_uptake <- renderPlot({
      plot_vaccination_mentions(vacmentions_df,
                                .date_range_lwr = input$date_range[1],
                                .date_range_upr = input$date_range[2])
  })
  
  output$mntns_changeplot <- 
        renderPlot(expr = {
        plot_changepoint(sentiment_ts, 
                         Daily,
                         #!!input$chngpt_freq, 
                         #.twts_date_lwr,
                         .date_range_lwr = input$date_range[1],
                         .date_range_upr = input$date_range[2],
                         #.twts_date_upr,
                         #input$chgpt_dt_range[2],
                         .twts_med_date
                         #input$chngpt_dt
                         )
          })
    
  output$mntns_topics <- renderPlot({
      plot_topics_pie(tm_df,
                      .date_range_lwr = input$date_range[1],
                      .date_range_upr = input$date_range[2])
  })
  
  output$mntns_wordcloud <- renderPlot({
      plot_wordcloud(tm_df, tknzd_words,
                     .date_range_lwr = input$date_range[1],
                     .date_range_upr = input$date_range[2])
  })
  
# Forecast plots ---------------------------------------------------------------
    output$frcst_plot <- 
        renderPlot(expr = {
            plot_sentiment_forcast(fcst_sntmnt)
        })

}

shinyApp(ui, server)