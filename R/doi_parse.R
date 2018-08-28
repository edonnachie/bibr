#' https://github.com/regexhq/doi-regex
#' http://www.doi.org/doi_handbook/2_Numbering.html#2.2
doiRegex <- '(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![%"#? ])\\S)+)'

#' Test whether string contains a doi
#'
#' @param string String to test
#' @return Logical indicator TRUE (matches doi specification) or FALSE
#' @export
is_doi <- function(string) {
  grepl(doiRegex, string, perl=TRUE)
}


#' Extract all DOI from a character object
#'
#'
#' @param string String from which to extract all doi entries
#' @param as_vector A single logical value. If TRUE (default), calls `unlist` to return to a single character vector. If FALSE, returns a list of character vectors, one for each element of `string`
#' @export
#' @importFrom stringi stri_extract_all
extract_doi <- function(string, as_vector = TRUE) {
  out <- stringi::stri_extract_all_regex(string, pattern = doiRegex)
  if (as_vector)
    out <- unlist(out)

  return(out)
}


#' Remove the URL part (i.e. "http://dx.doi.org/") from a doi
#'
#' @param doi DOI, with or without URL part
#' @return string with URL part removed
#' @export
doi_strip_url <- function(doi){
  sub("http://dx.doi.org/", "", doi)
}
