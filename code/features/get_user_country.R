#' Harmonise user location to country level
#' 
#' Based on the work done by user ANDRADA in Kaggle to clean user location
#'  from twitter data. A work in progress to say the least. . .
#' 
#' @importFrom dplyr mutate
#' 
#' @param .dataset A dataframe or a tibble object
#' @param .from_column A column containing location information as raw text.
#'  It can be a string character with the column's name or a symbol.
#' @param .to_column Target column to store the output. If the column doesn't 
#'  exist, a new one will be created. Any previous column with the same name
#'  will be replaced. It can be a string character with the column's name or 
#'  a symbol.
#' 
#' @returns A tibble
#' 
#' @export
get_user_country <- function(.dataset, .from_column, .to_column){
    .from_col_sym <- .safe_as_string(substitute(.from_column))
    .to_col_sym <- .safe_as_string(substitute(.to_column))
    
    .dataset %>%
        dplyr::mutate(
            !!.to_col_sym := stringr::str_to_lower(!!.from_col_sym),
            !!.to_col_sym := ifelse(grepl("india", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("delhi", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("mumbai", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("bengaluru", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("bangalore", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("bhubaneswar", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("hyderabad", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("punjab", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("bengal", !!.to_col_sym), "India", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("china", !!.to_col_sym), "China", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("beijing", !!.to_col_sym), "China", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("hong kong", !!.to_col_sym), "Hong Kong", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("singapore", !!.to_col_sym), "Singapore", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("australia", !!.to_col_sym), "Australia", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("melbourne", !!.to_col_sym), "Australia", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("sydney", !!.to_col_sym), "Australia", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("south africa", !!.to_col_sym), "Africa", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("england", !!.to_col_sym), "UK", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("united kingdom", !!.to_col_sym), "UK", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("london", !!.to_col_sym), "UK", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("uk", !!.to_col_sym), "UK", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("scotland", !!.to_col_sym), "UK", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("united states", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("usa", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("us", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("u.s.a.", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("estados unidos", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("washington", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("new york", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("angeles", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("atlanta", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("california", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("chicago", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("boston", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("philadelphia", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("diego", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("seattle", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("texas", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("nyc", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("vegas", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("francisco", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("florida", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("dallas", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("denver", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("new orleans", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("brooklyn", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("phoenix, az", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("pittsburgh, pa", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("nashville, tn", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("baltimore, md", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("charlotte, nc", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl(".*, ca$", !!.to_col_sym), "US", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("mexico", !!.to_col_sym), "Mexico", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("worldwide", !!.to_col_sym), "Unspecified", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("global", !!.to_col_sym), "Unspecified", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("earth", !!.to_col_sym), "Unspecified", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("everywhere", !!.to_col_sym), "Unspecified", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("nigeria", !!.to_col_sym), "Nigeria", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("kenya", !!.to_col_sym), "Kenya", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("switzerland", !!.to_col_sym), "Switzerland", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("ireland", !!.to_col_sym), "Ireland", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("ontario", !!.to_col_sym), "Canada", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("canada", !!.to_col_sym), "Canada", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("toronto", !!.to_col_sym), "Canada", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("philippines", !!.to_col_sym), "Philippines", !!.to_col_sym),
            !!.to_col_sym := ifelse(grepl("malaysia", !!.to_col_sym), "Malaysia", !!.to_col_sym))
}

#Internals=====================================================================
#' Safely coerce an argument to a string (reused)
#' 
#' @importFrom stringr str_remove_all
#' 
#' @importFrom rlang sym
#' 
#' @noRd
.safe_as_string <- function(.obj){
    deparse(.obj) %>%
        stringr::str_remove_all('\\"') %>%
        rlang::sym()
}

