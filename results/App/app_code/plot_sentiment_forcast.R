plot_sentiment_forcast <- function(.dataset){
    .dataset %>%
        dplyr::filter(
            dplyr::between(Daily, as.Date('2021-10-15'), as.Date('2022-10-14'))
            ) %>%
        dplyr::mutate(
            .forcasted = dplyr::if_else(.forcasted, 'Predicted', 'Observed'),
            .forcasted = factor(.forcasted, levels = c('Observed', 'Predicted'))
        ) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = Daily, 
                y = sentiment)) +
        ggplot2::geom_point(mapping = ggplot2::aes(colour = .forcasted)) +
        ggplot2::geom_smooth(method = 'loess', se = FALSE, linetype = 'dashed',
                             size = 0.5) +
        ggplot2::geom_label(
            mapping = ggplot2::aes(x = as.Date('2021-10-30'), y = 0.5), 
            label = 'Positive sentiment') +
        ggplot2::geom_label(
            mapping = ggplot2::aes(x = as.Date('2021-10-30'), y = -0.5), 
            label = 'Negative sentiment') +
        ggplot2::labs(colour = '') +
        ggplot2::scale_y_continuous(limits = c(-1,1)) +
        ggplot2::scale_x_date(date_labels = '%d %b %Y') +
        ggplot2::theme(legend.position = 'bottom')
}