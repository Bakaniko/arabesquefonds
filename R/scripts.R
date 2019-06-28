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


  if (!"GeoJSON" %in% sf::st_drivers()[,1]) {
    message("GeoJSON driver not available")
    return(1)
  }
  if (!"ESRI Shapefile" %in% sf::st_drivers()[,1]) {
    message("ESRI Shapefile driver not available")
    return(1)
  }

  try_catch(
    archive::archive_extract(arch_path, temp),
            .e = ~ paste0("There is an error: ", .x),
            .w = ~ paste0("This is a warning: ", .x))

  ## Conversion


  files <- dir_ls(temp, recurse = TRUE, glob = "*.shp")
  purrr::map(files, convert_to_geojson, dir = "converted", extension = ".geojson")
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
#' @param extension extension to use(optional, default = json)
#' @examples
#' \dontrun{
#' convert_to_geojson(path, out)
#' }
convert_to_geojson <- function(path, layer =NULL, dir = NULL, extension = ".geojson"){
  require(sf)
  require(attempt)



  # read data

  data <- sf::st_read(dsn = path)

  # Extract CRS

    path_to_proj <- path %>% stringr::str_replace(pattern = "\\.[a-z]{3}", replacement = ".prj")

    # read wkt string
    wkt <- readLines(path_to_proj, warn = FALSE)
    # Apply crs to data
    #st_crs(data, wkt = wkt)
    #st_set_crs(data, st_crs( 2154))

    #data <- data %>% st_transform(crs = st_crs(2154))

    print(st_crs(data))

  # extract layer name
  layer_name <- path %>%
    stringr::str_split(pattern = "/") %>%
    unlist() %>%
    last() %>% stringr::str_replace(pattern = "\\.[a-z]{3}", replacement = "")

  # Create destination name
  destination <- paste0(dir, "/", layer_name, extension)

  # write file
  data %>% st_write(dsn = destination, delete_dsn=TRUE)

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
