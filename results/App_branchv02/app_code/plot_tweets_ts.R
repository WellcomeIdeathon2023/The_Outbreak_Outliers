plot_tweets_ts <- function(.dataset, .date_col = 'Daily', .date_range_lwr, .date_range_upr){
  if (rlang::is_missing(.date_range_lwr)) .date_range_lwr <-  as.Date("1988-01-01")
  if (rlang::is_missing(.date_range_upr)) .date_range_upr <-  Sys.Date()
  .date_col <- .safe_symbols(substitute(.date_col))
  
  .dataset %>%
    dplyr::filter(
      dplyr::between(!!.date_col, .date_range_lwr, .date_range_upr)) %>%
    dplyr::group_by(!!.date_col) %>%
    dplyr::summarise(
      n = dplyr::n(),
      verified = sum(user_verified, na.rm = TRUE)) %>%
    .plot_ts(!!.date_col, n) +
    ggplot2::labs(
      y = 'Tweets\n(counts)',
      x = '') +
    ggplot2::scale_x_date(date_labels = '%b %Y') +
    ggplot2::geom_rug(
      mapping = ggplot2::aes(y = verified),
      sides = 'b',
      colour = "#7B0051") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 0.85))
}

.plot_ts <- function(.dataset, .x_axis, .y_axis){
  .x_axis = rlang::enquo(.x_axis)
  .y_axis = rlang::enquo(.y_axis)
  
  .dataset %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(x =!!.x_axis, y =  !!.y_axis)) +
    ggplot2::geom_point(colour = "#007A87") +
    ggplot2::geom_smooth(
      method = 'loess', 
      se = FALSE, 
      linetype = 'dashed', 
      linewidth = 1,
      colour = "#FFB400")
}