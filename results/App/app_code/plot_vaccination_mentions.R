plot_vaccination_mentions <- function(.dataset, .date_range_lwr, .date_range_upr){
    if (rlang::is_missing(.date_range_lwr)) .date_range_lwr <- .get_default_lwr_date()
    if (rlang::is_missing(.date_range_upr)) .date_range_upr <- .get_default_upr_date()
    
    .dataset %>%
        dplyr::group_by(Daily) %>%
        dplyr::summarise(vaccinated = sum(intent_pred, na.rm = TRUE)) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = Daily,
                y = vaccinated)) +
        ggplot2::geom_col(fill = "#007A87") +
        ggplot2::labs(
            y = 'Counts',
            x = '') +
        ggplot2::scale_x_date(date_labels = '%b %Y') +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 25, hjust = 0.75)
        ) +
        ggplot2::geom_vline(
            xintercept = .date_range_lwr, 
            linetype = 'dotted',
            colour = "#7B0051") + 
        ggplot2::geom_vline(
            xintercept = .date_range_upr, 
            linetype = 'dotted',
            colour = "#7B0051")
        
}