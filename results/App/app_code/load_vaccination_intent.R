load_vaccination_intent <- function(.path_to_csv, .threshold = 0.5){
    .path_to_csv %>%
        readr::read_csv(
            col_types = .def_vaccination_cols,
            show_col_types = FALSE,
            progress = FALSE) %>%
        dplyr::mutate(
            intent_pred = intent_pred >= .threshold
        )
}

.def_vaccination_cols <- readr::cols(
    tweet_id = readr::col_integer(),
    date = readr::col_date(format = '%Y-%m-%d %H:%M:%S'),
    intent_pred = readr::col_number()
)