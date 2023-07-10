plot_topics_freq <- function(.dataset, .topic_col, .date_range_lwr, .date_range_upr){
    if (rlang::is_missing(.date_range_lwr)) .date_range_lwr <- as.Date("2020-08-09")
    if (rlang::is_missing(.date_range_upr)) .date_range_upr <- as.Date("2022-09-14")
    .topic_col <- rlang::enquo(.topic_col)
    
    .dataset %>%
        dplyr::filter(dplyr::between(date, .date_range_lwr, .date_range_upr)) %>%
        dplyr::mutate(!!.topic_col := forcats::fct_infreq(!!.topic_col),
                      !!.topic_col := forcats::fct_rev(!!.topic_col)) %>%
        dplyr::group_by(!!.topic_col) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        #dplyr::arrange(dplyr::desc(n)) %>%
        # dplyr::mutate(!!.topic_col := forcats::fct_inorder(!!.topic_col),
        #               !!.topic_col := forcats::fct_rev(!!.topic_col)) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = n, 
                y = !!.topic_col,
                fill = !!.topic_col)) +
        ggplot2::geom_col() +
        ggplot2::labs(y = '', x = 'Number of tweets') +
        ggplot2::theme(legend.position = 'none')
        
}