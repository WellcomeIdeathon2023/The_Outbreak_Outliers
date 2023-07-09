#' Proportion of missing values
#' 
#' This function report the proportion of missing values with confidence 
#'  intervals
#'  
#' @importFrom dplyr transmute
#' 
#' @importFrom glue glue
#' 
#' @param .dataset A tibble or dataframe object
#' @param .conf_lvl Confidence level for the returned confidence interval. Must
#'  be a single number between 0 and 1. Defaults to 0.95.
#' @param .include_pval Logical. Should the output include a column for p values?
#'  Defaults to FALSE.
#' 
#' @returns A tibble.
#' 
#' @export
missing_prop_table <- function(.dataset, .conf_lvl = 0.95, .include_pval = FALSE){
    .gen_raw_tbl_miss(.dataset, .conf_lvl) %>%
        dplyr::transmute(
            Column = names(.dataset),
            Missing = prettyNum(.count, big.mark = ','),
            `Proportion CI` = glue::glue('{round(.prop,2)} [{round(.lwr_ci,2)}, {round(.upr_ci,2)}]'),
            .pval) %>%
        .include_p_val(.include_pval)
        
}
# Internals ===================================================================
#' Generate table with raw values
#' 
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' 
#' @noRd
.gen_raw_tbl_miss <- function(.dataset, .conf_lvl){
    purrr::map(.dataset, is.na) %>%
        purrr::map(
            .gen_rough_row, 
            .conf_lvl = .conf_lvl) %>%
        purrr::list_rbind()
}
#------------------------------------------------------------------------------
#' Generate a row for each column in the data
#' 
#' @noRd
.gen_rough_row <- function(.x, .conf_lvl){
    .get_trial_counts(.x) %>%
        c(.,list(conf.level = .conf_lvl)) %>%
        .get_estimates()
}
#------------------------------------------------------------------------------
#' Calculate counts of successes and trials
#' 
#' @noRd
.get_trial_counts <- function(.x){
    list(x = sum(.x), n = length(.x))
}
#------------------------------------------------------------------------------
#' Compute the proportion, conf. interval and p value for each column
#' 
#' @importFrom tibble tibble
#' 
#' @noRd
.get_estimates <- function(.arg_lst){
    .props <- do.call(prop.test, .arg_lst)
    tibble::tibble(
        .count  = .arg_lst$x,
        .prop   = .props$estimate,
        .lwr_ci = .props$conf.int[1],
        .upr_ci = .props$conf.int[2],
        .pval   = .props$p.value)
}
#------------------------------------------------------------------------------
#' Should the output include p values?
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' 
#' @noRd
.include_p_val <- function(.tbl, .include_pval){
    if (.include_pval) {
        .tbl %>%
            dplyr::mutate(p = .pretty_pvals(.pval))
                
    } else {
        .tbl %>%
            dplyr::select(-.pval)
    }
}
#------------------------------------------------------------------------------
#' Format p values (vectorised)
#' 
#' @noRd   
.pretty_pvals <- function(.pval){
    if (.pval < 0.001) {
        return('< 0.001')
    }
    format(.pval, nsmall = 3)
}
.pretty_pvals <- Vectorize(.pretty_pvals)