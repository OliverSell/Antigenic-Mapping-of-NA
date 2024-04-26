library("fs")
library("stringr")

make_dir <- function(path){
  if( !dir.exists(path) ){
    dir.create(path)
  }
}

check_empty <- function(column) {
  empty_count <- sum(is.na(column) | column == "")
  return(empty_count >= 0.8 * length(column))
}

# Rename columns to a row
set_colnames_from_row <- function(tbl, row_number = 1) {
  if(row_number > nrow(tbl) | row_number < 1) {
    stop("The specified row number is out of bounds.")
  }
  colnames(tbl) <- as.character(unlist(tbl[row_number, ]))
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

# Create a unique directory
create_unique_folder <- function(base_path, folder_name) {
  full_path <- file.path(base_path, folder_name)
  
  # Check if the directory already exists
  if (dir_exists(full_path)) {
    suffix <- 2                            # Start suffix from 2

    repeat {
      new_folder_name <- paste0(folder_name, "_v", suffix)
      full_path <- file.path(base_path, new_folder_name)
      if (!dir_exists(full_path)) {
        folder_name <- new_folder_name
        break
      }
      suffix <- suffix + 1
    }
  }
  # Create the directory
  dir_create(full_path)
  
  return(full_path)
}

get_latest_version_folder <- function(base_path, folder_prefix, date) {
  # Match folders with date
  search_pattern <- paste0(folder_prefix, date, "(|_v[0-9]+)$")
  
  # Filter folders based on the search pattern
  all_folders <- dir_ls(base_path, type = "directory")
  date_folders <- all_folders[grepl(search_pattern, basename(all_folders))]
  
  # Find latest version 
  if (length(date_folders) > 0) {
    sorted_folders <- sort(date_folders, decreasing = TRUE)
    latest_folder <- sorted_folders[1]
  } else {
    latest_folder <- NA
    warning("No matching folders found for the specified date and virus type.")
  }
  
  return(latest_folder)
}

# Round up values in report_tables. 
### Some values are numeric, charecters or <16. This function finds a way around that.
convert_and_round_char <- function(x) {
  # Check if the string is numeric
  if_else(grepl("^[0-9.]+$", x), 
          as.character(round(as.numeric(x), digits = 2)), 
          x)
}