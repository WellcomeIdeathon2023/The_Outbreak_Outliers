add_date_aggregations <- function(.dataset, .date_col){
  .date_col <- .safe_symbols(substitute(.date_col))
  
  .dataset %>%
    dplyr::mutate(
      Daily = !!.date_col,
      Weekly = cut.Date(!!.date_col, breaks = 'week', start.on.monday = TRUE),
      Weekly = as.Date(Weekly),
      Fortnightly = cut.Date(!!.date_col, breaks = '2 weeks', start.on.monday = TRUE),
      Fortnightly = as.Date(Fortnightly),
      Monthly = cut.Date(!!.date_col, breaks = '1 month', start.on.monday = TRUE),
      Monthly = as.Date(Monthly)) %>%
    dplyr::select(-!!.date_col)
}