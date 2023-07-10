load_base_dataset <- function(.path_to_data){
  .path_to_data %>%
    readr::read_csv(
      col_types = .def_base_data_cols,
      show_col_types = FALSE,
      progress = FALSE) %>%
    dplyr::mutate(
      clnd_hashtags = stringr::str_remove_all(clnd_hashtags, '^\\[|\\]$'),
      clnd_hashtags = stringr::str_remove_all(clnd_hashtags, "\\'")
    ) %>%
    dplyr::select(
      tweet_id,
      date,
      user_location = clnd_user_location,
      user_followers,
      user_friends,
      user_favourites,
      user_verified,
      hashtags = clnd_hashtags)
}

.def_base_data_cols <- readr::cols_only(
  tweet_id = readr::col_integer(),
  date = readr::col_date(format = '%Y-%m-%d %H:%M:%S'),
  clnd_user_location = readr::col_character(),
  user_followers = readr::col_number(),
  user_friends = readr::col_number(),
  user_favourites = readr::col_number(),
  user_verified = readr::col_number(),
  clnd_hashtags = readr::col_character())