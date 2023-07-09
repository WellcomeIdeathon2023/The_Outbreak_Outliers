prepare_vax_tweets_data <- function(.path_to_file){
    .ingest_data(.path_to_file) %>%
        .tag_for_removal() %>%
        .add_tweet_id() %>%
        .prepare_hashtag_col() %>%
        .remove_flagged_cols() %>%
        .finalise_dataset()
}

.ingest_data <- function(.path_to_file){
    .path_to_file %>%
        readr::read_csv(
            col_types = .ingest_data_cols(),
            show_col_types = FALSE,
            progress = FALSE)
}

.ingest_data_cols <- function(){
    readr::cols_only(
        user_location = readr::col_character(),
        user_description = readr::col_character(),
        user_followers = readr::col_integer(),
        user_friends = readr::col_integer(),
        user_favourites = readr::col_integer(),
        user_verified = readr::col_logical(),
        date = readr::col_date(format = '%d/%m/%Y %H:%M'),
        text = readr::col_character(),
        hashtags = readr::col_character(),
        is_retweet = readr::col_logical())
}

.tag_for_removal <- function(.dataset){
    .dataset %>%
        .tag_all_na_rows()
}

.tag_all_na_rows <- function(.dataset){
    .na_row_flag <- 
        .dataset %>%
        .get_na_row_flag() # Takes advantage of R highly optimised matrix operations
    
    .dataset %>%
        dplyr::mutate(.flag_all_na_row = .na_row_flag)
}

.core_tweet_vax_cols <- function(){
    c('user_location', 'user_description', 'user_followers', 
      'user_friends', 'user_favourites', 'user_verified', 
      'date', 'text',  'hashtags', 'is_retweet')
}

.get_na_row_flag <- function(.dataset){
    .dataset %>%
        dplyr::mutate(
            dplyr::across(
                .cols = tidyselect::all_of(.core_tweet_vax_cols()),
                .fns = is.na)) %>%
        apply(MARGIN = 1, all)
        # dplyr::rowwise() %>%
        # dplyr::transmute(
        #     .flag_na_row = all(dplyr::c_across(tidyselect::everything()))) %>%
        # dplyr::ungroup()
}

.add_tweet_id <- function(.dataset){
    .dataset %>%
        dplyr::mutate(tweet_id = dplyr::row_number())
}

.prepare_hashtag_col <- function(.dataset){
    .dataset %>%
        dplyr::mutate(
            hashtags = stringr::str_remove_all(
                string = hashtags, 
                pattern = "^\\[|\\]$"))
}

.remove_flagged_cols <- function(.dataset){
    .dataset %>%
        dplyr::filter(
            dplyr::if_all(
                .cols = tidyselect::starts_with('.flag'),
                .fns = ~!.x))
}

.finalise_dataset <- function(.dataset){
    .dataset %>%
        dplyr::select(-dplyr::starts_with('.')) %>%
        dplyr::relocate(tweet_id)
}