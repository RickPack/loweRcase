####  Call function lower_case_filenames_and_folder_names()  ####
## Renames all folders and files under each folder identified for folders_to_lowercase
## Saves old and new filenames and folders in 
## files_renamed and folders_renamed CSVs in your working directory,

## !! CRITICAL: First run at the Git command line:  
##              git config core.ignorecase false
##    Errors printed at script termination may not be important.
##    Scroll up after running to see all errors and warnings.

## Calling the function:
##     lower_case_filenames_and_folder_names(folders_to_lowercase) 
##     Run twice because files and folders that appear the second 
##       run in the printed list of renamed objects were probably not renamed.

## Commit your Git repo as it is prior to renaming, to maximize safety!!

## Git repo location? !! Do not include slash at end !!
git_repo <- "C:/Users/RickPack/Documents/git"
## Folders to lower-case names as well filenames 
## (automatically includes subfolders)
## !! Do not include slash at end (e.g., "/analyses" is correct) !!
folders_to_lowercase <-
  c(paste0(git_repo, "/biscale"),
    paste0(git_repo, "/tidycensus"))

## Commit your Git repo as it is prior to renaming, to maximize safety!!

source(paste0(git_repo, "/ds_utils/r/tools/lower_case_filenames_folders/", "02_lower_case_filenames_and_folders.r"))
lower_case_filenames_and_folder_names(folders_to_lowercase) 
