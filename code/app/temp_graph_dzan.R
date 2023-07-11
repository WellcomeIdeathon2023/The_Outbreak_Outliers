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


tweets <- readRDS("code/app/tweets.rds")
regex <- read_csv("data/interim/cascading_regex_filters_results.csv")

filtered_df <- df
# events <- events

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
    filter(date >= input$date_range[1] &
               date <= input$date_range[2])

filtered_events <- events %>%
    filter(date >= input$date_range[1] &
               date <= input$date_range[2])

filtered_df |> group_by(VADER_label) |>
    mutate(prosjek = mean(count, na.rm = TRUE))

filtered_df |> filter(VADER_label == "Negative") |>
    ggplot() +
    geom_histogram(aes(x = count))


filtered_df <- filtered_df |> group_by(VADER_label) |>
    mutate(
        mean = mean(count, na.rm = T),
        q1 = quantile(count, 0.25, na.rm = T),
        q3 = quantile(count, 0.75, na.rm = T)
    ) |>
    ungroup() |>
    group_by(date) |>
    mutate(daily_tweets = sum(count, na.rm = T)) |>
    group_by(VADER_label, date) |>
    mutate(sent_pct = count / daily_tweets) |>
    ungroup() |>
    group_by(VADER_label) |>
    mutate(
        mean_pct = mean(sent_pct, na.rm = T),
        q1_pct = quantile(sent_pct, 0.25, na.rm = T),
        q3_pct = quantile(sent_pct, 0.75, na.rm = T)
    ) |>
    mutate(redflag = case_when(count > q3 &
                                   VADER_label == "Negative" ~ sent_pct))

filtered_df |> mutate(redfleg2 = ifelse(count>q3, count, NA))

filtered_df |> 
    ggplot(aes(x=date, y=sent_pct, fill=VADER_label, group=VADER_label))+
    geom_line(size=.7)

filtered_df |>
    mutate(VADER_label = factor(VADER_label, levels = c("Positive", "Neutral", "Negative"))) |>
    group_by(date, VADER_label) %>%
    summarise(n = sum(count)) %>%
    mutate(percentage = n / sum(n)) |> 
    ggplot(aes(x = date, fill = VADER_label, group = VADER_label)) +
    geom_area(aes(y = percentage)) +
    geom_ribbon(aes(y=percentage, ymin=0.25, ymax=redflag, fill="red"))+
    
    #geom_point(aes(y =count), alpha = 0.5) +
    #geom_line(aes(y= avg))+
    # This might need to be fixed, we have some flags to be appearable
    
    
    # geom_smooth() +
    # geom_vline(data = filtered_events,aes(xintercept=date), alpha = 0.5) +
    # geom_text(data = filtered_events,
    #           mapping = aes(x = date, y = 400 , label = event),
    #           inherit.aes = FALSE,
#           hjust = 1)+geom_line(aes(y = avg)) +


labs(x = "Date", y = "Count of Tweets", color = "7-day rolling average") +
    scale_color_manual(
        values = c(
            Negative = "#ff4444",
            Positive = "#00C851",
            Neutral = "#FFdd00"
        ),
        labels = c(
            Negative = "Negative",
            Positive = "Positive",
            Neutral = "Neutral"
        )
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
    )
#+
#   scale_x_date(limits = input$date_range, expand = c(0, 0), date_labels = "%b %d, %Y")
