plot_textblob <- function(.dataset, .date_range_lwr, .date_range_upr){
    if (rlang::is_missing(.date_range_lwr)) .date_range_lwr <- as.Date('1993-08-01')
    if (rlang::is_missing(.date_range_upr)) .date_range_upr <- Sys.Date()
    
    .dataset %>%
        dplyr::filter(
            dplyr::between(date, .date_range_lwr, .date_range_upr)
        ) %>%
        .gen_freq_tbl_vars(sentiment, subjectivity) %>%
        webr::PieDonut(
            mapping = ggplot2::aes(sentiment, subjectivity, count = count),
            labelpositionThreshold = 1,
            labelposition = 10
        )
    
}