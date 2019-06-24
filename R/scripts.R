#' get_archives
#'
#' @param pattern : file pattern
#' returns archive in a folder
#' @return a vector of archive names
#' @example
#' get_archives()
#' @export

get_archives <- function(pattern){
  require(fs)
  fs::dir_ls(regexp = pattern, # all files with the .7z or .zip extension
             recurse = TRUE)
}


#' unarchive
#' unzip archive files
#'
#' @param arch_path path to the archive
#' @export

unarchive <- function(arch_path){
  require(archive) # unzip functions
  require(purrr)

  message("######################")
  ## print filename for debug
  filename <- stringr::str_split(arch_path, "/") %>% unlist() %>% last()
  print(filename)

  temp <- tempdir() # répertoire temporaire pour la décompression
  message("open archive")
  arch_to_extract <-  try_catch(
    archive::archive(arch_path),
            .e = ~ paste0("There is an error: ", .x),
            .w = ~ paste0("This is a warning: ", .x))
  print( paste("writing files from :", filename))

  for(path in arch_to_extract){

    # Extract path and filenames
    data <- path %>%
            as_data_frame() %>%
            dplyr::filter(
              stringr::str_detect(
                value, '.shp|.dbf|.prj|.shx|.cpg')) %>% # detect shapefile files
            dplyr::mutate(
              file = stringr::str_extract(value, "\\w*\\.[a-z]{3}"))

    # stockage dans un répertoire temporaire
    for (fichier in data$value) {
      file.copy(from = fichier, to = file.path(temp, basename(fichier)))
    }

  }

  ## Conversion
  print(list.files(path = temp))

  rm(temp) # suppression du répertoire temporaire
  print("leaving unarchive")
}


#' create_convert
#' Create convert folder to store converted files
#' @examples
#' \dontrun{
#' create_convert()
#' create_convert("toto")
#' }

create_convert <- function(folder_name = "convert"){
  require(attempt) # try catch non archive file
  try_catch(dir_create(folder_name),
            .e = ~ paste0("There is an error: ", .x),
            .w = ~ paste0("This is a warning: ", .x))
}

#' convert them to GeoJSON in the convert folder and zip them

#' provide a summary of actions


#' prepare_data
#'
#' Prepare the data from archived shapefiles
#'
#' @example
#' prepare_data()
#' importFrom magrittr "%>%"
#' @export

prepare_data <- function(){
  require(dplyr)   # data science !
  require(purrr)   # functionnal programming
  require(attempt) # try catch non archive file
  pattern <- ".*[.][7z|zip].*"

  ## list all shapefiles in the folder
  message("### Archives found : ###")
  print(fs::dir_info(recurse = TRUE) %>%
    filter(type == "file", grepl(pattern = pattern, path) ) %>%
    arrange(desc(size)) %>%
    select(path, size, modification_time)
  )

  ## get archive paths
  archives_list <- arabesqueFonds::get_archives(pattern)

  ## Create convert folder
  arabesqueFonds::create_convert()

  ## extract data from file
  message("\n### Extract Archives ###")
  purrr::map(archives_list, arabesqueFonds::unarchive)

  return(0)
}
