plot_location_frequencies <-function(.dataset, .date_col = 'Daily', .date_range_lwr, .date_range_upr){
  if (rlang::is_missing(.date_range_lwr)) .date_range_lwr <-  as.Date("1988-01-01")
  if (rlang::is_missing(.date_range_upr)) .date_range_upr <-  Sys.Date()
  .date_col <- .safe_symbols(substitute(.date_col))
  
  .dataset %>%
    dplyr::filter(
      dplyr::between(!!.date_col, .date_range_lwr, .date_range_upr)) %>%
    .plot_hcols(user_location, 15) +
    ggplot2::labs(x = '', y = 'Number of tweets') +
    ggplot2::theme(legend.position = 'none')
    
}