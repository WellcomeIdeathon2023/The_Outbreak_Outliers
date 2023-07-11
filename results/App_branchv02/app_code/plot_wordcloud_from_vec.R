plot_wordcloud_from_vec <- 
    function(.word_vec, .min_freq = 1, .max_words = 250,
                                   .random = FALSE, .rotation = 0.35, 
                                   colours = .get_app_palette()){
        
        .freq_tbl <- 
            tibble::tibble(words = .word_vec) %>%
            dplyr::mutate(
                words = stringr::str_remove_all(words, "\\'"),
                words = stringr::str_trim(words)) %>%
            dplyr::filter(words != '') %>%
            dplyr::group_by(words) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            head(.max_words)
        
        wordcloud::wordcloud(words = .freq_tbl$words, freq = .freq_tbl$n,
                             min.freq = .min_freq, max.words = .max_words,
                             random.order = .random, rot.per = .rotation,
                             colors = colours)
    }

plot_wordcloud <- function(.dataset, .words_col, .topic_label, .date_range_lwr, .date_range_upr, ...){
    if (!rlang::is_missing(.topic_label)) {
        .dataset <- 
            .dataset %>%
            dplyr::filter(topic %in% .topic_label)
    }
    
    if (rlang::is_missing(.date_range_lwr)) .date_range_lwr <- .get_default_lwr_date()
    if (rlang::is_missing(.date_range_upr)) .date_range_upr <- .get_default_upr_date()
    
    .words_col <- .safe_symbols(substitute(.words_col))
    
    
    .word_vec<- 
        .dataset %>%
        dplyr::filter(
            dplyr::between(date, .date_range_lwr, .date_range_upr)) %>%
        dplyr::pull(!!.words_col) %>%
        stringr::str_split(', ') %>%
        unlist()
        
    
    
    plot_wordcloud_from_vec(.word_vec, ...)
}


