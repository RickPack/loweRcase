## Renames all folders and files under each folder identified for folders_to_lowercase
## called by 01_lower_case_filenames_and_folders_call.r

lower_case_filenames_and_folder_names <- function(folders_to_lowercase) {
  if (substr(git_repo, nchar(git_repo), nchar(git_repo)) == '/') {
    stop("Slash found at at end of git_repo argument. Please remove from folder name")
  }
  if (sum(substr(folders_to_lowercase, nchar(folders_to_lowercase), nchar(folders_to_lowercase)) == '/') > 0) {
    stop("Slash found at at end of folders_to_lowercase argument. Please remove from folder name")
  }
  err_list  <- as.character()
  warn_list <- as.character()
  all_old_files   <- as.character()
  all_old_dirs    <- as.character()
  all_new_files <- as.character()
  all_new_dirs  <- as.character()
  
  for (i in 1:length(folders_to_lowercase)) {
    process_folder <- folders_to_lowercase[i]
    message(paste0("Renaming files in ", process_folder, " and its subfolders"))
    message(paste("Folder", i, "of ",  length(folders_to_lowercase)))
    
    ## Filename section, folder name section is next
    old.filenames <- list.files(path = process_folder, pattern = ".", recursive = TRUE)
    old.filepaths <- paste0(process_folder, '/',
                            old.filenames)
    
    ## Regular expression to extract filename from end of file path
    old.filenames <- stringr::str_extract(old.filenames,"([^/]+$)")
    ## If no capital letter, remove from vector of names to change.
    old.filepaths <- 
      old.filepaths[grepl("[A-Z]",old.filenames)]
    old.filenames <- 
      old.filenames[grepl("[A-Z]",old.filenames)]
    
    ## initialize, really clear of values in case length of old.filenames = 0
    new.filepaths <- as.character()
    if (length(old.filenames) > 0) {
      new.filenames <- tolower(old.filenames)
      
      new.filepaths <- paste0(tolower(dirname(old.filepaths)), 
                              '/',
                              new.filenames)
    }
    
    
    ## Filename rename loop, folder section is next
    for (filechg in 1:length(new.filepaths)) {
      ## if no filename to change, skips through and goes to folder loop
      if(length(new.filepaths) == 0) next
      
      old.filepaths.try     <- old.filepaths[filechg]
      new.filepaths.try     <- new.filepaths[filechg]
      ## for output files_renamed_...CSV, all_new_files column just the filename.
      ## Folder displayed in all_old_files column.
      new.filenames.changed <- new.filenames[filechg]
      
      # if no subfolder found or somehow NA appears, skip
      if (length(new.filepaths.try) == 0 | is.na(new.filepaths.try) | 
          new.filepaths.try == "NA" | new.filepaths.try == "") next
      
      ## Key rename function call      
      tryvar_file <- file.rename(old.filepaths.try, new.filepaths.try)
      if (tryvar_file == FALSE) {
        err_list <- c(paste0("File rename failed : ", old.filepaths.try), err_list )
      } else {
        ## store files renamed in a vector
        all_old_files <- c(all_old_files, old.filepaths.try)
        all_new_files <- c(all_new_files, new.filenames.changed)
      }
    }
    
    ## folder section
    message(paste0("Renaming subfolders under ", process_folder))
    git_folder_pattern <- gsub('/', '\\/', process_folder)
    old.directories <- dirname(old.filepaths)
    old.directories.tochange <- stringr::str_extract(
      old.directories, paste0("(?<=", git_folder_pattern, ").*"))
    ## If no capital letter, remove from vector of names to change.
    old.directories.tochange <- 
      old.directories.tochange[grepl("[A-Z]",old.directories.tochange)]
    
    ## initialize, really clear of values in case length of old.directories.tochange = 0
    new.directories            <- as.character()
    if (length(old.directories.tochange) > 0) {
      new.directories.lower_case <- tolower(old.directories.tochange)
      new.directories <- paste0(process_folder,
                                new.directories.lower_case)
    }
    
    ## Folder rename loop
    for (dirchg in 1:length(new.directories)) {
      ## if no folder name to change, skips through and goes to folder loop
      if(length(new.directories) == 0) next
      
      old.directories.try      <- old.directories[dirchg]
      new.directories.try      <- new.directories[dirchg]
      
      # if no subfolder found or somehow NA appears, skip
      if (length(new.directories.try) == 0 | is.na(new.directories.try) |
          new.directories.try == "NA" | new.directories.try == "") next
      
      ## Key rename function call      
      tryvar_dir <- file.rename(old.directories.try, new.directories.try)
      if (tryvar_dir == FALSE) {
        err_list <- c(paste0("Folder rename failed : ", old.directories.try), err_list )
      } else {
        ## store directories renamed in a vector
        dirs_renamed <- setdiff(new.directories.try, old.directories.try) 
        renamed_dirs <- setdiff(old.directories.try, new.directories.try)
        all_new_dirs <- c(all_new_dirs, dirs_renamed)
        all_old_dirs <- c(all_old_files, renamed_files)
      }
    }
  }
  
  if (length(err_list) > 0) {
    message("Errors: ")
    print(err_list)
  } 
  if (length(warn_list) > 0) {
    message("Warnings: ")
    print(warn_list)
  } 
  
  ## for appending datetime in the CSV as an unseparated number   
  ## to avoid file overwriting
  csv_save_suffix <- paste0(gsub("[^0-9]","", Sys.time())[1], ".csv")
  
  if (length(all_new_files) == 0 & length(all_new_dirs) == 0) {
    message("")
    message("")
    message("No folders or files found with an upper-case letter. Nothing renamed!")
  }
  if (length(all_new_files) > 0) {
    files_changed_frm <- data.frame(cbind(all_old_files, all_new_files))
    colnames(files_changed_frm) <- c("all_old_files", "all_new_files")
    write.csv(files_changed_frm, paste0("files_renamed_", csv_save_suffix))
    message(paste0(
      "Assuming no errors, old and new filepaths placed in ", getwd(),
      "/files_renamed_", csv_save_suffix))
    files_changed_frm <<- files_changed_frm
  }
  if (length(all_new_dirs) > 0) {
    dirs_changed_frm  <- data.frame(cbind(all_old_dirs, all_new_dirs))
    write.csv(dirs_changed_frm, paste0("folders_renamed_", csv_save_suffix))
    message(paste0(
      "Assuming no errors, old and new folder names placed in ", getwd(),
      "/folders_renamed_", csv_save_suffix))
    print(all_new_dirs)
    dirs_changed_frm  <<- dirs_changed_frm
  }
  
 ## function end
}
