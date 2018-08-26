#' Extract a DOI from the first page of a PDF file
#'
#' This function returns the first DOI from the first page
#' of a given PDF file. It uses `pdftools::pdf_text` to convert the first
#' page of the PDF to text. The PDF must contain readable text.
#'
#' @param path Path specifying a PDF file from which the DOI should be extracted
#' @return Character string with matched DOI, or NA if no DOI found
#' @importFrom pdftools pdf_text
#' @export
pdf_extract_doi <- function(path) {
  txt <- tryCatch(
    pdftools::pdf_text(path)[1],
    warning = function(w){return(NA_character_)},
    error = function(e){return(NA_character_)}
  )
  extract_doi(txt, as_vector = TRUE)[1]
}


#' Parse a filename in format AuthorYYYY-Title.pdf
#'
#' @param fname File name in format "AuthorYYYY-Title.pdf" (minor variations allowed)
#' @return tibble with columns `filename`, `author`, `year` and `title`.
#' @importFrom stringi stri_match_first_regex
#' @importFrom tibble as_tibble
pdf_parse_filename <- function(fname){
  fname <- basename(fname)
  out <- stringi::stri_match_first_regex(fname, "^([A-Za-z-_]+)(\\d{4})[_-](.+)\\.pdf$")
  colnames(out) <- c("filename", "author", "year", "title")

  # Cleanup
  out[is.na(out[, "filename"]), "filename"] <- fname[which(is.na(out[, "filename"]))]
  out[, "title"] <- gsub("_", " ", out[, "title"])
  out[, "author"] <- gsub("_", " ", out[, "author"])
  out[, "author"] <- gsub("-$", "", out[, "author"])

  tibble::as_tibble(out)
}
