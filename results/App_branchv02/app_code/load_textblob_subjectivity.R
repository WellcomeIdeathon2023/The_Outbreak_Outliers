load_textblob_subjectivity <-function(.path_to_csv){
    .path_to_csv %>%
        readr::read_csv(
            col_types = .def_textblob_cols,
            show_col_types = FALSE,
            progress = FALSE) %>%
        dplyr::rename(
            subjectivity = textblob_subjectivity_label,
            score = textblob_subjectivity_score
        )
}

.def_textblob_cols <- readr::cols_only(
    tweet_id = readr::col_integer(),
    textblob_subjectivity_label = readr::col_character(),
    textblob_subjectivity_score = readr::col_double()
)