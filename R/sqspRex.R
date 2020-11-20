#' Extract download links from XML
#'
#' The only way to export from Squarespace is a Wordpress format XML file.
#' If all you are trying to do is recover the downloadable files from your
#' Squarespace site rather than migrate to Wordpress this will involve lot of
#' unnecessary time and effort.
#' This package takes the XML export and produces a dataframe of downloadable
#' file links.
#' You can then download the files from the links at your leisure.
#' @description Get a dataframe of download links from the standard XML Squarespace export.
#' @param file.types File type suffixes as a vector.
#' @param local.path The local path to the XML file.
#' @param site.alias The Squarespace alias of your site most likely in the format of a
#' subdomain such as "my-site.squarespace.com without any "https://" or "http://".
#' @return A dataframe of downloadable links.
#' @export
#' @examples
#' sqspRex(file.types, local.path, site.stem)
#' sqspRex(c(pdf, doc, docx, xls), "~/Desktop/export.xml", "my-site.squarespace.com")

sqspRex <- function(file.types, local.path, site.alias) {

  ## Set regex for file types
  post.ext   <-
    paste0("(.*?(?:", paste(file.types, collapse = "|"), "))|.*")

  ## Read in XML as character & filter for downloadable URLs
  char_imp <-
    scan(local.path,
         what = "character",
         sep = NULL) %>%
    enframe(name = NULL) %>%
    filter(grepl("href=\"(.*?)\"", value))

  ## Take file types and add a dot for file extensions
  file_types_df <- file.types %>%
    enframe(name = NULL, value = "file.type") %>%
    mutate(file.ext = paste0(".", file.type))

  ## Convert vector to df and create
  urls_df <-
    lapply(file_types_df$file.ext, function(x)
      filter(char_imp, grepl(x, value))) %>%
    bind_rows() %>%
    distinct() %>%
    mutate(
      url.extract = str_remove_all(value, "href"),
      url.extract = str_remove_all(url.extract, '"'),
      url.extract = str_remove_all(url.extract, '=')
    ) %>%
    mutate(url.clean = str_extract(url.extract, post.ext)) %>%
    mutate(url.clean = if_else(
      !grepl("http", url.clean),
      paste0(site.alias, url.clean),
      url.clean
    )) %>%
    transmute(url.clean = gsub("https://", "", url.clean)) %>%
    filter(str_detect(url.clean, site.alias)) %>%
    transmute(download.file = paste0("https://", url.clean))
}
