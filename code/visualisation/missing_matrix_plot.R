#' A ggplot-based visualisation for missing values
#' 
#' This function creates a missing values matrix visualisation to help 
#'  better understanding missingness pattern of a dataset
#' 
#' @param .dataset A data frame or tibble class object
#' @param .title A character string. Optional.
#' @param .subtitle A character string. Optional.
#' 
#' @returns A ggplot class object
#' 
#' @export
missing_matrix_plot <- function(.dataset, .title = '', .subtitle = ''){
    .dataset %>%
        .missing_wide_to_long() %>%
        .ggmissing_basic_plot() %>%
        .ggmissing_prettify(.title, .subtitle) %>%
        .ggmissing_finalise()
}

# Internals ===================================================================
#' Create a data set with missing values indicators and pivot it longer
#' 
#' @importFrom  dplyr across
#' @importFrom  dplyr mutate
#' 
#' @importFrom forcats fct_inorder
#' 
#' @importFrom  tidyr pivot_longer
#' 
#' @importFrom tidyselect everything
#' 
#' @noRd
.missing_wide_to_long <- function(.dataset){
    .dataset %>%
        dplyr::mutate(
            dplyr::across(.cols = tidyselect::everything(), .fns = is.na),
            .row_index = dplyr::row_number() ) %>%
        tidyr::pivot_longer(
            cols = -.row_index, 
            names_to = 'Column',
            values_to = 'Missing' ) %>%
        dplyr::mutate(Column = forcats::fct_inorder(Column))
}
#------------------------------------------------------------------------------
#' Create a basic ggplot2 plot
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_raster
#' @importFrom
#' 
#' @noRd
.ggmissing_basic_plot <- function(.long_dataset){
    .long_dataset %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = Column, 
                y = .row_index, 
                fill = Missing)) +
        ggplot2::geom_raster()
}
#------------------------------------------------------------------------------
#' Add labels and formatting to a ggplot
#' 
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' 
#' @noRd
.ggmissing_prettify <- function(.basic_plot, .title, .subtitle){
    .basic_plot +
        ggplot2::labs(
            title = .title,
            subtitle = .subtitle,
            x = '',
            y = 'Observation\nindex\n') +
        ggplot2::scale_x_discrete(
            expand = c(0,0), 
            position = 'top') +
        ggplot2::scale_fill_manual(
            breaks = c(TRUE),
            labels = c(''),
            values = c('#ffffb3', '#999999')) +
        ggplot2::theme(
            text = ggplot2::element_text(family = 'Helvetica', size = 12),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 0),
            legend.position = 'bottom',
            legend.title = ggplot2::element_text(size = 10),
            legend.key = ggplot2::element_rect(
                colour = 'black', 
                linewidth = 1.5))
        
}
#------------------------------------------------------------------------------
#' Add breaks to the y axis (Observation indices)
#'
#'
#' @importFrom ggplot2 scale_y_reverse
#' 
#' @noRd
.ggmissing_finalise <- function(.pretty_plot){
    .y_breaks <- .get_y_breaks(.pretty_plot)
    
    .pretty_plot +
        ggplot2::scale_y_reverse(
            expand = c(0,0),
            breaks = .y_breaks) 
}
#-------------------------------------------------------------------------------
#' Create breaks for the y axis
#'
#' @importFrom ggplot2 ggplot_build
#' 
#' @noRd
.get_y_breaks <- function(.a_ggplot){
    .gg_built <- ggplot2::ggplot_build(.a_ggplot)
    .y_index <- unique(.gg_built$plot$data$.row_index)
    
    .min_y <- min(.y_index)
    .max_y <- max(.y_index)
    
    as.integer(seq.int(.min_y, .max_y, length.out = 20))
}