explode_python_lists <- function(.chr_list_vec){
  stringr::str_split(.chr_list_vec, ', ') %>%
    unlist() %>%
    stringr::str_trim()
}
