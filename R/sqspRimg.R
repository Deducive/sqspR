#' Extract image download links from XML
#'
#' The only way to export from Squarespace is a Wordpress format XML file.
#' If all you are trying to do is recover the images from your
#' Squarespace site rather than migrate to Wordpress this will involve lot of
#' unnecessary time and effort.
#' This package takes the XML export and produces a dataframe of downloadable
#' links to previously uploaded site images.
#' You can then download the files from the links at your leisure.
#' @description Get a dataframe of image download links from the standard XML Squarespace export.
#' @param file.types File type suffixes as a vector.
#' @param local.path The local path to the XML file.
#' @return A dataframe of downloadable links.
#' @export
#' @examples
#' sqspRimg(file.types, local.path)
#' sqspRimg(c(jpg, png, bmp), "~/Desktop/export.xml")

sqspRimg <- function(file.types, local.path) {

  ## Set regex for file types & URL pattern
  post.ext   <-
    paste0("(.*?(?:", paste(file.types, collapse = "|"), "))|.*")

  url_pattern <-
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  ## Read in XML as character & filter for downloadable URLs
  char_imp <-
    scan(local.path,
         what = "character",
         sep = NULL) %>%
    enframe(name = NULL) %>%
    filter(grepl("https://images.squarespace-cdn.com", value, ignore.case = TRUE)) %>%
    mutate(value = str_extract(value, url_pattern)) %>%
    mutate(ext   = str_sub(value,
                           start = as.integer(nchar(value) - 2)))

  ## Take file types and add a dot for file extensions
  file_types_df <- file.types %>%
    enframe(name = NULL, value = "file.type") %>%
    mutate(file.ext = paste0(".", file.type))

  ## Convert vector to df and create
  urls_df <-
    lapply(file_types_df$file.ext, function(x)
      filter(char_imp, grepl(x, value, ignore.case = TRUE))) %>%
    bind_rows() %>%
    distinct() %>%
    transmute(download.image = value)
}
