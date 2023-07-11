.safe_symbols <- function(x){
  rlang::sym(gsub('\\"', replacement = '', deparse(x)))
}

.get_app_palette <- function(){
  c("#FF5A5F", "#FFB400", "#007A87", "#FFAA91", "#7B0051")
}

.gen_freq_tbl_vars <- function(.dataset, ...){
    .dataset %>%
        dplyr::group_by(...) %>%
        dplyr::count() %>%
        dplyr::rename(count = n) %>%
        dplyr::ungroup() 
}

.gen_freq_tbl_1var <- function(.dataset, .group_col){
    .group_col <- .safe_symbols(substitute(.group_col))
    
    .dataset %>%
        dplyr::mutate(
            !!.group_col := forcats::fct_infreq(!!.group_col),
            !!.group_col := forcats::fct_rev(!!.group_col)) %>%
        dplyr::group_by(!!.group_col) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(n))
}

.get_default_lwr_date <- function(){
    as.Date('1993-08-01')
}

.get_default_upr_date <- function(){
    Sys.Date()
}
