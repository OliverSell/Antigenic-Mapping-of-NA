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


get_latest_version_folder <- function(base_path, folder_prefix, date) {
  potential_folders <- c(paste0(folder_prefix, date), paste0(folder_prefix, date, "_v", 1:100))
  existing_folders <- potential_folders[dir.exists(file.path(base_path, potential_folders))]
  
  if (length(existing_folders) > 0) {
    selected_folder <- existing_folders[which.max(nchar(existing_folders))]
    return(file.path(base_path, selected_folder))
  } else {
    stop("No matching folders found.")
  }
}
