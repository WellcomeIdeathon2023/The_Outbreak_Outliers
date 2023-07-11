plot_sentiment_ts <- function(.dataset, .sentiment_label, .date_range_lwr, .date_range_upr){
    if (!rlang::is_missing(.sentiment_label)) {
        .dataset <- 
            .dataset %>%
            dplyr::filter(VADER_label %in% .sentiment_label)
    }
    if (rlang::is_missing(.date_range_lwr)) .date_range_lwr <- .get_default_lwr_date()
    if (rlang::is_missing(.date_range_upr)) .date_range_upr <- .get_default_upr_date()
    
    .dataset %>%
        #dplyr::filter(
        #    dplyr::between(date, .date_range_lwr, .date_range_upr)) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = date, 
                colour = VADER_label,
                group = VADER_label)) +
        ggplot2::geom_point(
            mapping = ggplot2::aes(
                y = count),
            alpha = 0.5) +
        ggplot2::geom_line(
            mapping = ggplot2::aes(
                y = avg)) +
        ggplot2::labs(
            x = '',
            y = 'Number of tweets',
            colour = 'Rolling average\n(7 day window)') +
        ggplot2::scale_colour_manual(
            values = c("#ff4444", "#00C851", "#FFdd00"),
            labels = c('Negative', 'Positive', 'Neutral')) +
        ggplot2::scale_x_date(date_labels = '%b %Y') +
        ggplot2::theme(legend.position = 'bottom') +
        ggplot2::geom_vline(
            xintercept = .date_range_lwr, 
            linetype = 'dotted',
            colour = "#7B0051") + 
        ggplot2::geom_vline(
            xintercept = .date_range_upr, 
            linetype = 'dotted',
            colour = "#7B0051")
}