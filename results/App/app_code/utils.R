.safe_symbols <- function(x){
  rlang::sym(gsub('\\"', replacement = '', deparse(x)))
}

.get_app_palette <- function(){
  c("#FF5A5F", "#FFB400", "#007A87", "#FFAA91", "#7B0051")
}