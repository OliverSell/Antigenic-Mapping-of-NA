make_dir <- function(path){
  if( !dir.exists(path) ){
    dir.create(path)
  }
}

check_empty <- function(column) {
  empty_count <- sum(is.na(column) | column == "")
  return(empty_count >= 0.8 * length(column))
}


set_colnames_from_row <- function(tbl, row_number = 1) {
  # Check if the specified row number is within the number of rows in the table
  if(row_number > nrow(tbl) | row_number < 1) {
    stop("The specified row number is out of bounds.")
  }
  # Set column names from the specified row
  colnames(tbl) <- as.character(unlist(tbl[row_number, ]))
  #colnames(tbl) <- paste0("Col",as.character(unlist(tbl[row_number, ])))
  
  # Remove the row that was used for column names
  tbl <- tbl[-row_number, ]
  
  return(tbl)
}
