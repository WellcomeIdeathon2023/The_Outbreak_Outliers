library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(bslib)
library(forcats)
library(bsicons)
library(tidyr)
library(zoo)
library(readr)

# 
tweets <- readRDS("tweets.rds")
regex <- read_csv("../../data/interim/cascading_regex_filters_results.csv")
my_palette <- c("#FF5A5F", "#FFB400", "#007A87",  "#FFAA91", "#7B0051")


# Prepare the transformed data
df <- tweets %>% 
  group_by(date, VADER_label) %>%
  summarise(count = as.double(n()))%>%
  pivot_wider(names_from = VADER_label, values_from = count) %>%
  ungroup() %>% 
  mutate(Positive = rollapply(Positive,7,mean,align='right',fill = 0, na.rm = T),
         Negative = rollapply(Negative,7,mean,align='right',fill= 0 , na.rm = T ),
         Neutral = zoo::rollapply(Neutral,7,mean,align='right',fill= 0, na.rm = T)) %>% 
  tidyr::pivot_longer(cols =-date ) %>% 
  rename(avg = value,
         VADER_label = name) %>% 
  left_join(tweets %>% 
              group_by(date, VADER_label) %>%
              summarise(count = as.double(n())), 
            by = c("date", "VADER_label")
  )
  
  
#generating topics from regular expressions
topic_df <- regex %>% 
  inner_join(tweets %>% select(tweet_id, date, VADER_label), by = "tweet_id") %>% 
  group_by(date) %>% 
  summarise(vaccine_filter = sum(vaccine_filter == TRUE),
            hesitancy_filter = sum(hesitancy_filter == TRUE),
            safety_filter = sum(safety_filter == TRUE),
            mistrust_filter = sum(mistrust_filter == TRUE)) %>% 
  ungroup() %>% 
  mutate(vaccine_filter = rollapply(vaccine_filter,7,mean,align='right',fill = 0, na.rm = T),
         hesitancy_filter = rollapply(hesitancy_filter,7,mean,align='right',fill= 0 , na.rm = T ),
         safety_filter = zoo::rollapply(safety_filter,7,mean,align='right',fill= 0, na.rm = T),
         mistrust_filter = rollapply(mistrust_filter,7,mean,align='right',fill= 0 , na.rm = T )
         ) %>% 
  tidyr::pivot_longer(cols =-date ) %>% 
  rename(avg = value,
         topic = name) %>% 
  left_join(regex %>% 
              inner_join(tweets %>% select(tweet_id, date, VADER_label), by = "tweet_id") %>% 
              group_by(date) %>% 
              summarise(vaccine_filter = sum(vaccine_filter == TRUE),
                        hesitancy_filter = sum(hesitancy_filter == TRUE),
                        safety_filter = sum(safety_filter == TRUE),
                        mistrust_filter = sum(mistrust_filter == TRUE)) %>% 
              ungroup() %>% 
              tidyr::pivot_longer(cols =-date ) %>% 
              rename(count = value,
                     topic = name),
            by = c("date", "topic")
  ) 


attitude <- 
  tweets %>% 
  left_join(regex, by = "tweet_id") %>% 
  select(-text, -VADER_compound_score, -topic, -tweet_id) %>% 
  filter(vaccine_filter =  TRUE) %>% 
  select(-vaccine_filter) %>% 
  group_by(date) %>% 
  summarise(hesitancy_filter = sum(hesitancy_filter == TRUE),
            safety_filter = sum(safety_filter == TRUE),
            mistrust_filter = sum(mistrust_filter == TRUE)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  pivot_longer(-c(date))

attitude_sentiment <- 
  tweets %>% 
  left_join(regex, by = "tweet_id") %>% 
  select(-text, -VADER_compound_score, -topic, -tweet_id) %>% 
  filter(vaccine_filter =  TRUE) %>% 
  select(-vaccine_filter) %>% 
  group_by(date, VADER_label) %>% 
  summarise(hesitancy_filter = sum(hesitancy_filter == TRUE),
            safety_filter = sum(safety_filter == TRUE),
            mistrust_filter = sum(mistrust_filter == TRUE)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  pivot_longer(-c(date, VADER_label))

  


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



vbs <- list(
  value_box(
    title = "Total tweets analysed", 
    value = nrow(tweets),
    showcase = bs_icon("bar-chart"),
    theme_color = "danger",
    height = "120px"
    # p("The 1st detail")
  ),
  value_box(
    title = "Validated tweets", 
    value = "456",
    showcase = bs_icon("graph-up"),
    theme_color = "warning",
    height = "120px"
    # p("The 2nd detail"),
    # p("The 3rd detail")
  ),
  value_box(
    title = "3rd value", 
    value = "789",
    showcase = bs_icon("pie-chart"),
    p("The 4th detail"),
    p("The 5th detail"),
    p("The 6th detail")
  )
)

layout_column_wrap(
  width = "150px",
  height = "120px",
  !!!vbs
)





# Define UI


ui <- page_navbar(
    title = "The Outbreak Outliars",
    sidebar = sidebar(
      title = "Dashboard controls",
      # selectInput(
      #   inputId = "label_filter",
      #   label = "Select Sentiment Label",
      #   choices = c("All", "Negative", "Positive", "Neutral"),
      #   selected = "All"
      # ),
      
      # This doesnt work is the box chose, needs to be fixed
      
      checkboxGroupInput(
        inputId = "topic_filter",
        label = "Select Topic",
        choices =
          #c(unique(topic_df$topic)),
          c("Safety filter" = "safety_filter",
                    "Mistrust filter" = "mistrust_filter",
                     "Vaccine filter"  =   "vaccine_filter",
                      "Hesitancy filter" =  "hesitancy_filter"),
        selected = c(unique(topic_df$topic))
      ),
      
      
      # This is the box check for the sentiment but also requires changes in the output function 
      # to work properly
      
      checkboxGroupInput(
        inputId = "label_filter",
        label = "Select Sentiment Label",
        choices = c(unique(df$VADER_label)),
        selected = c(unique(df$VADER_label))
      ),
      
      
      sliderInput(
        inputId = "date_range",
        label = "Select Time Bounds",
        min = min(ymd(tweets$date), na.rm = TRUE),
        max = max(ymd(tweets$date), na.rm = TRUE),
        value = c(min(ymd(tweets$date), na.rm = TRUE), max(ymd(tweets$date), na.rm = TRUE))
      )
    ),
   nav_panel("Topic",
            layout_column_wrap(
               width = "150px",
               height = "120px",
               fill = FALSE,
               vbs[[1]], vbs[[2]]
             ),
            card(
              width = 12,
              status = "primary",
              card_header("Topic modelling"),
              plotOutput("topic_time")
            )
    ,
    card(
      width = 12,
      status = "primary",
      card_header("Topic modelling"),
      plotOutput("topicPlot")
    )
  ),
  nav_panel("Sentiment", 
            navset_card_tab(
              title = "Time series by attitudes towards vaccine",
              nav_panel("Mistrust", plotOutput("mistrust_plot")),
              nav_panel("Safety", plotOutput("safety_plot")),
              nav_panel("Hesistancy", plotOutput("hesitancy_plot"))
            ),
            navset_card_tab(
              title = "Time series of attitudes by sentiment",
              nav_panel("Mistrust", plotOutput("mistrust_sentiment")),
              nav_panel("Safety", plotOutput("safety_sentiment")),
              nav_panel("Hesistancy", plotOutput("hesitancy_sentiment"))
            )
            ),
  
  
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    navbar_bg = "#2d2d2d"
  )
  )

s

# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default theme
theme_set(theme_dark())

# Define server
server <- function(input, output) {
  
  # Render the sentiment plot
  output$sentimentPlot <- renderPlot({
    filtered_df <- df
    events <- events
   
    # if (input$label_filter != "All") {
    #   # filtered_df <- df[df$VADER_label %in% input$label_filter,]
    #   # filtered_df
    #   if(is.null(input$label_filter)){NULL}
    #   
    #     filtered_df <- filtered_df %>%
    #     filter(VADER_label %in% input$label_filter)
    # }
    
    
    
       filtered_df <- filtered_df %>%
         filter(VADER_label %in% input$label_filter)
      
        
       
    
    filtered_df <- filtered_df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    filtered_events <- events %>% 
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    ggplot(data = filtered_df, aes(x = date, color = VADER_label, group = VADER_label)) +
      geom_point(aes(y =count), alpha = 0.5) +
      geom_line(aes(y= avg))+
      # This might need to be fixed, we have some flags to be appearable 
      
      
      # geom_smooth() +
      # geom_vline(data = filtered_events,aes(xintercept=date), alpha = 0.5) +
      # geom_text(data = filtered_events,
      #           mapping = aes(x = date, y = 400 , label = event),
      #           inherit.aes = FALSE,
      #           hjust = 1)+geom_line(aes(y = avg)) + 
      
      
      labs(x = "Date", y = "Count of Tweets", color = "7-day rolling average") +
      scale_color_manual(
        values = c(Negative = "#ff4444", Positive = "#00C851", Neutral = "#FFdd00"),
        labels = c(Negative = "Negative", Positive = "Positive", Neutral = "Neutral")
      ) +
      # geom_vline(data = events, aes(xintecept = date)) + 
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) +
      scale_x_date(limits = input$date_range, expand = c(0, 0), date_labels = "%b %d, %Y")
  })
  
  # Render the topic plot
  output$topicPlot <- renderPlot({
    filtered_topic_df <- topic_df
    
# This is the function that filters the output to options selected from the box menu
# Need to be fixed
    
    
    filtered_topic_df <- filtered_topic_df %>%
        filter(topic %in% input$topic_filter)

    filtered_topic_df <- filtered_topic_df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])

    filtered_topic_df %>% 
      group_by(topic) %>% 
      summarise(val = sum(count)) %>% 
      mutate(topic = fct_reorder(topic, val)) %>%
      ungroup() %>% 
    
      ggplot( aes(x=topic, y=val, fill = topic)) +
      geom_bar(stat="identity",   
               alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      scale_color_manual(
        values = c(
          "vaccine_filter" = "#ff4444",
          "hesitancy_filter" = "#00C851",
          "safety_filter" = "#FF8800",
          "mistrust_filter" = "#FFC400"
          # "Topic 5" = "#4285F4"
        ),
        labels = c(
          "Vaccine" = "vaccine_filter",
          "Topic 2" = "hesitancy_filter",
          "Topic 3" = "safety_filter",
          "Topic 4" = "mistrust_filter"
          # "Topic 5" = "Topic 5"
        )) +
      
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
  })

  # Render the topic time series
  output$topic_time <- renderPlot({
    filtered_topic_df <- topic_df
    
    # This is the function that filters the output to options selected from the box menu
    # Need to be fixed
    
    
      filtered_topic_df <- filtered_topic_df %>%
        filter(topic%in%input$topic_filter) #%>% 
        # filter(VADER_label %in% input$label_filter)
  
    filtered_topic_df <- filtered_topic_df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    ggplot(data = filtered_topic_df, aes(x = date, color = topic, group = topic)) +
      # geom_point(aes(y = count), alpha = 0.5) +
      geom_line(aes(y = avg)) +
      labs(x = "Date", y = "Count of Tweets") +
      scale_color_manual(
        values = c(Negative = "#ff4444", Positive = "#00C851"),
        labels = c(Negative = "Negative", Positive = "Positive")) +
      scale_color_manual(
        values = c(
          "vaccine_filter" = "#ff4444",
          "hesitancy_filter" = "#00C851",
          "safety_filter" = "#FF8800",
          "mistrust_filter" = "#FFC400"
          # "Topic 5" = "#4285F4"
        ),
        labels = c(
          "Topic 1" = "vaccine_filter",
          "Topic 2" = "hesitancy_filter",
          "Topic 3" = "safety_filter",
          "Topic 4" = "mistrust_filter"
          # "Topic 5" = "Topic 5"
        )) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d"))
    
      }) 
  
  # Render the time series for mistrust
  output$mistrust_plot <- renderPlot({
    
    mistrust_attitude <- 
      attitude %>% filter(name == 'mistrust_filter') 
      
    ggplot(mistrust_attitude, aes(x=date, y=value)) +
      geom_point(alpha = 0.3, color = "#FFAA91") +
      geom_smooth(color = "#FFAA91") +
      ylab("Number of tweets") +
      xlab("Date") +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
    
  })
  
  # Render the time series for safety
  output$safety_plot <- renderPlot({
    
    mistrust_attitude <- 
      attitude %>% filter(name == 'safety_filter') 
    
    ggplot(mistrust_attitude, aes(x=date, y=value)) +
      geom_point(alpha = 0.3, color = "#FFAA91") +
      geom_smooth(color = "#FFAA91") +
      ylab("Number of tweets") +
      xlab("Date") +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
    
  })
  
  # Render the time series for hesitancy
  output$hesitancy_plot <- renderPlot({
    
    mistrust_attitude <- 
      attitude %>% filter(name == 'hesitancy_filter') 
    
    ggplot(mistrust_attitude, aes(x=date, y=value)) +
      geom_point(alpha = 0.3, color = "#FFAA91") +
      geom_smooth(color = "#FFAA91") +
      ylab("Number of tweets") +
      xlab("Date") +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
    
  })
  
  # Render the time series for mistrust
  output$mistrust_sentiment <- renderPlot({
    
    mistrust_sent_df <- 
      attitude_sentiment %>% filter(name == 'mistrust_filter') 
    
    ggplot(mistrust_sent_df, aes(x=date, y=value, color = VADER_label)) +
      geom_point(alpha = 0.3) +
      geom_smooth() +
      ylab("Number of tweets") +
      xlab("Date") +
      scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87")) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
    
  })
  
  # Render the time series for safety sentiment
  output$safety_sentiment <- renderPlot({
    
    safety_sent_df <- 
      attitude_sentiment %>% filter(name == 'safety_filter') 
    
    ggplot(safety_sent_df, aes(x=date, y=value, color = VADER_label)) +
      geom_point(alpha = 0.3) +
      geom_smooth() +
      ylab("Number of tweets") +
      xlab("Date") +
      scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87")) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
    
  })
  
  # Render the time series for hesitancy sentiment
  output$hesitancy_sentiment <- renderPlot({
    
    hesitancy_sent_df <- 
      attitude_sentiment %>% filter(name == 'hesitancy_filter') 
    
    ggplot(hesitancy_sent_df, aes(x=date, y=value, color = VADER_label)) +
      geom_point(alpha = 0.3) +
      geom_smooth() +
      ylab("Number of tweets") +
      xlab("Date") +
      scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87")) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
    
  })

    
  }
  

# Run the Shiny app
shinyApp(ui = ui, server = server)
