#' Create a univariate bar plot
#' 
#' This function creates bar plots for a single categorical variable. It is
#'  intended mainly to visualise categorical variables with large cardinality.
#'  
#' @param .dataset A dataframe or tibble object
#' @param .column Name of a column in `.dataset`
#' @param .y_label Optional. A character string to label the plotted column.
#'  If `NULL`, which is the default value, the column's name will be used.
#' @param .drop_na Logical. Whether to drop `NA`s. Defaults to `FALSE`
#' @param .n_largest An integer or `NULL`. If an integer n, only the levels
#'  with the n largest frequencies will be included in the plot. If `NULL`, 
#'  which is the default, all levels are included.
#'  
#' @returns A ggplot object
#'
#' @export
univar_cat_bar_plot <- function(.dataset, .column, .y_label = NULL, .drop_na = FALSE, .n_largest = NULL){
    .column_sym <- .safe_as_string(substitute(.column))
    
    .dataset %>%
        .drop_missing_vals(.column_sym, .drop_na) %>%
        .build_freq_tbl(.column_sym) %>%
        .keep_n_largest(.n_largest) %>%
        .plot_cat_bar(.column_sym) %>%
        .finalise_cat_bars(.y_label)
}
#Internals=====================================================================
#' Safely coerce an argument to a string (reused)
#' 
#' @importFrom stringr str_remove_all
#' 
#' @importFrom rlang sym
#' 
#' @noRd
.safe_as_string <- function(.obj){
    deparse(.obj) %>%
        stringr::str_remove_all('\\"') %>%
        rlang::sym()
}
#------------------------------------------------------------------------------
#' Drop rows with missing values
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' 
#' @importFrom tidyr replace_na
#' 
#' @noRd
.drop_missing_vals <- function(.dataset, .column_sym, .drop_na){
    if (.drop_na) {
        .dataset %>% 
            dplyr::filter(!is.na(!!.column_sym))
    } else {
        .dataset %>% 
            dplyr::mutate(
                !!.column_sym := dplyr::na_if(!!.column_sym, ''),
                !!.column_sym := tidyr::replace_na(
                    !!.column_sym, 
                    'Missing value (NA)')) 
    }
    
}
#------------------------------------------------------------------------------
#' Calculate frequencies for each level of .column
#' 
#' @importFrom dplyr arrange
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' 
#' 
#' @importFrom forcats fct_infreq
#' @importFrom forcats fct_rev
#' 
#' @importFrom tidyr replace_na
#' 
#' @noRd
.build_freq_tbl <- function(.dataset, .column_sym) {
    .dataset %>%
        dplyr::mutate(
            !!.column_sym := forcats::fct_infreq(!!.column_sym),
            !!.column_sym := forcats::fct_rev(!!.column_sym)) %>%
        dplyr::group_by(!!.column_sym) %>%
        dplyr::count() %>%
        dplyr::arrange(dplyr::desc(n))
}
#------------------------------------------------------------------------------
#' Return levels with n largest frequencies
#' 
#' @noRd
.keep_n_largest <- function(.dataset, .n_largest) {
    if (is.null(.n_largest)) {
        return(.dataset)
    }
    .dataset %>%
        head(.n_largest)
}
#------------------------------------------------------------------------------
#' Calculate frequencies for each level of .column
#' 
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' 
#' @noRd
.plot_cat_bar <- function(.dataset, .column_sym){
    .dataset %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = n, y = !!.column_sym)) +
        ggplot2::geom_col(
            mapping = ggplot2::aes(
                colour = !!.column_sym), 
            fill = 'transparent')
}
#------------------------------------------------------------------------------
#' Calculate frequencies for each level of .column
#' 
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' 
#' @importFrom ggthemes theme_tufte
#' 
#' @noRd
.finalise_cat_bars <- function(.a_ggplot, .y_label){
    .y_label <- .gen_ylabel(.y_label, .a_ggplot)
    
    .a_ggplot +
        ggplot2::labs(y = .y_label, x = 'Counts') +
        ggthemes::theme_tufte() +
        ggplot2::theme(
            text = ggplot2::element_text(family = 'Helvetica', size = 12),
            axis.text.x = ggplot2::element_text(angle = 35),
            legend.position = 'none'
        )
}
#------------------------------------------------------------------------------
#' Generate y labels
#' 
#' @importFrom ggplot2 ggplot_build
#' 
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_title
#' 
#' @noRd
.gen_ylabel <- function(.y_label, .a_ggplot){
    if (!is.null(.y_label)) {
        return(.y_label)
    }
    .ggbuilb <- ggplot2::ggplot_build(.a_ggplot) 
    names(.ggbuilb$plot$data)[1] %>%
        stringr::str_replace_all(pattern = '[_|.]', replacement = ' ') %>%
        stringr::str_to_title()
}