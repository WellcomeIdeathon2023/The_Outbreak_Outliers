# To do: 
#   - Refractoring
#   - Documentation
transform_dist_plots <- function(.dataset, .column){
    .column_sym <- .safe_as_string(substitute(.column))
    
    .dataset %>%
        dplyr::transmute(identity = !!.column_sym,
                         `log-transform` = log10(identity),
                         `square root` = sqrt(identity),
                         inverse = identity^(-1)) %>%
        tidyr::pivot_longer(
            cols = tidyselect::everything(),
            names_to = 'transformation',
            values_to = 'x_axis') %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = x_axis, colour = transformation)) +
        ggplot2::geom_histogram(bins = 30, fill = 'transparent') +
        ggplot2::facet_wrap(~transformation, nrow = 2, ncol = 2, scales = 'free') +
        ggplot2::labs(x = '', y = 'Frequencies') +
        ggsci::scale_color_lancet() +
        ggthemes::theme_tufte() +
        ggplot2::theme(
            text = ggplot2::element_text(family = 'Helvetica', size = 12),
            legend.position = 'none')
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