make_dir <- function(path){
  if( !dir.exists(path) ){
    dir.create(path)
  }
}

check_empty <- function(column) {
  empty_count <- sum(is.na(column) | column == "")
  return(empty_count >= 0.8 * length(column))
}