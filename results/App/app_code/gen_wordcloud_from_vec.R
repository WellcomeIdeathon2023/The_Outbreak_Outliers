gen_wordcloud_from_vec <- 
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