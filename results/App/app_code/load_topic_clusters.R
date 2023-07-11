load_topic_clusters <- function(.path_to_csv){
    .path_to_csv %>%
        readr::read_csv(
            col_types = .def_topics_cols,
            show_col_types = FALSE,
            progress = FALSE) %>%
        dplyr::mutate(
            topic = dplyr::case_when(
                topic %in% c(0,5) ~ 'Vaccination slots',
                topic == 1 ~ 'News',
                topic == 2 ~ 'Stats updates',
                topic == 3 ~ 'Vaccine hesistancy',
                topic == 4 ~ 'Vacine support'),
            topic = factor(topic),
            tknzd_words = stringr::str_remove_all(tknzd_words, '^\\[|\\]$')) %>%
        janitor::clean_names()
        
}

.def_topics_cols <- 
    readr::cols(
    tweet_id = readr::col_integer(),   
    `0` = readr::col_double(),   
    `1` = readr::col_double(),   
    `2` = readr::col_double(),  
    `3` = readr::col_double(),
    `4` = readr::col_double(),  
    `5` = readr::col_double(),
    topic = readr::col_integer(),
    date = readr::col_date(format = '%Y-%m-%d %H:%M:%S'),               
    tknzd_words = readr::col_character())