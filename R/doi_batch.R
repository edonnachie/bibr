#' Process a list of dois from a file
#' and add bibtex file to dir
#' @param file File containing list of doi identifiers
#' @param dir Directory in which to save the bibtex citation on success
#' @return Result of doi_to_bibtex for each entry in file
#' @export
doi_batch <- function(file, dir){
  dois <- scan(file, what = "character")
  lapply(dois, doi_to_bibtex, dir = dir)
}
