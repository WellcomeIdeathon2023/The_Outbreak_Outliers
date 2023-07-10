get_topic_words <- function(.dataset, .topic_var, .tkns_col){
    .topic_var <- rlang::enquo(.topic_var)
    .tkns_col <- rlang::enquo(.tkns_col)
    
    .unique_topics <- 
        .dataset %>%
        dplyr::pull(!!.topic_var) %>%
        unique() %>%
        as.character() %>%
        sort()
    
        purrr::map(.unique_topics,
               function(t, .dataset){
                   .dataset %>%
                       dplyr::filter(!!.topic_var == t) %>%
                       dplyr::pull(!!.tkns_col) %>%
                       stringr::str_split(', ') %>%
                       unlist()
                   }, .dataset=.dataset) %>%
        purrr::set_names(.unique_topics)
    
}