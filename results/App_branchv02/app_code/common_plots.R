.plot_hcols <- function(.dataset, .groups_col, .n_largest = 5){
  .groups_col <- rlang::enquo(.groups_col)
  
  .dataset %>%
    dplyr::mutate(!!.groups_col := forcats::fct_infreq(!!.groups_col),
                  !!.groups_col := forcats::fct_rev(!!.groups_col)) %>%
    dplyr::group_by(!!.groups_col) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    head(.n_largest) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = n, 
        y = !!.groups_col,
        fill = !!.groups_col)) +
    ggplot2::geom_col()
}

.plot_vcols <- function(.dataset, .groups_col, .n_largest = 5){
    .groups_col <- rlang::enquo(.groups_col)
    
    .dataset %>%
        dplyr::mutate(!!.groups_col := forcats::fct_infreq(!!.groups_col)) %>%
        dplyr::group_by(!!.groups_col) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        head(.n_largest) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                y = n, 
                x = !!.groups_col,
                fill = !!.groups_col)) +
        ggplot2::geom_col()
}