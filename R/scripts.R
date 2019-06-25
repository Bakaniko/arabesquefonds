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
#' @param out_folder path to the out folder
#' @export

unarchive <- function(arch_path, out_folder = "converted/"){
  require(archive) # unzip functions
  require(attempt) # try_catch features

  message("######################")
  ## print filename for debug
  filename <- stringr::str_split(arch_path, "/") %>% unlist() %>% last()
  #print(filename)

  temp <- tempdir() # répertoire temporaire pour la décompression
  message(paste("Opening archive :",filename))

  try_catch(
    archive::archive_extract(arch_path, temp),
            .e = ~ paste0("There is an error: ", .x),
            .w = ~ paste0("This is a warning: ", .x))


  path <- "ADMIN-EXPRESS-COG_2-0__SHP__FRA_2019-05-20/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2019-05-20/ADE-COG_2-0_SHP_LAMB93_FR/CHEF_LIEU.shp"
  #path <- fs::path(temp, path)
   #print(list.files(path))
  ## Conversion
  #files <- list.files(path = temp, recursive = TRUE)
  print(dir_info(path = temp) %>% select(path))
  toto <- dir_ls(temp, recurse = TRUE, glob = "*.shp") %>% head(n=1) %>%
    st_read()
  toto

  # For each files, test if it is a shapefile them opens it with {sf}
  #convert_to_geojson_out <- purrr::partial(convert_to_geojson,dir = temp)
  #purrr::walk(files, convert_to_geojson)
  #purrr::map(files, print)
  #for(){}
  ## Extraction
  # message(paste("writing files from :", filename))

  ## Cleaning and exiting
  rm(temp) # suppression du répertoire temporaire
  message("leaving unarchive")
}


#' create_convert
#' Create converted folder to store converted files
#' @param folder_name name of the folder to create
#' @examples
#' \dontrun{
#' create_convert()
#' create_convert("toto")
#' }

create_convert <- function(folder_name = "converted"){
  require(attempt)

  # try to create a new folder
  try_catch(dir_create(folder_name),
            .e = ~ paste0("There is an error: ", .x),
            .w = ~ paste0("This is a warning: ", .x))
}

#' convert file to geoJson
#'
#' @param path path to file to convert
#' @param out_folder
#' @param layer layer name (optional)
#' @param dir folder of source file (optional)
#' @examples
#' \dontrun{
#' convert_to_geojson(path, out)
#' }
convert_to_geojson <- function(path, layer =NULL, dir = NULL){
  require(sf)
  require(attempt)

  # try to open the file
  #print(path)

  # print(list.files(path))
  # spatial_df <- attempt::try_catch(sf::st_read(dsn=path),
  #           .e = ~ paste0("There is an error: ", .x),
  #           .w = ~ paste0("This is a warning: ", .x))
  # print(spatial_df)
  #print(list.files(outfolder))

}
#' provide a summary of actions


#' prepare_data
#'
#' Prepare the data from archived shapefiles
#' @param folder_name name of the destination folder
#' @example
#' prepare_data()
#' importFrom magrittr "%>%"
#' @export

prepare_data <- function(folder_name = "converted"){
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
  arabesqueFonds::create_convert(folder_name)

  ## extract data from file
  message("\n### Extract Archives ###")
  purrr::map(archives_list, arabesqueFonds::unarchive)

  return(NULL)
}
