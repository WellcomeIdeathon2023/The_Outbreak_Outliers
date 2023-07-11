load_sentiment_counts <- function(.path_to_rds){
        readr::read_rds(.path_to_rds) %>%
        .gen_freq_tbl_vars(date, VADER_label) %>% 
        tidyr::pivot_wider(
            names_from = VADER_label, 
            values_from = count) %>%
        .get_rolling_averages() %>%
        tidyr::pivot_longer(cols = -date) %>% 
        dplyr::rename(
            avg = value,
            VADER_label = name) %>% 
        dplyr::left_join(
            readr::read_rds(.path_to_rds) %>%
                .gen_freq_tbl_vars(date, VADER_label),
            by = c("date", "VADER_label"))
}

#.gen_freq_tbl_vars is located in utils.R

.get_rolling_averages <- function(.dataset){
    .dataset %>%
        dplyr::mutate(
            Positive  = zoo::rollapply(
                data  = Positive,
                width = 7,
                FUN   = mean,
                align ='right',
                fill  = 0, 
                na.rm = TRUE),
            Negative  = zoo::rollapply(
                data  = Negative,
                width = 7,
                FUN   = mean,
                align ='right',
                fill  = 0 , 
                na.rm = TRUE),
            Neutral   = zoo::rollapply(
                data  = Neutral,
                width = 7,
                FUN   = mean,
                align = 'right',
                fill  = 0, 
                na.rm = TRUE)) 
}