#' Look up a DOI and return citation as a bibtex entry
#'
#' This is a wrapper function that first queries crossref
#' using `doi_lookup_crossref`. If no result is found, it
#' then tries querying dx.doi.org directly using `doi_lookup_direct`.
#'
#' @param doi DOI identifier (or DOI hyperlink)
#' @return Character vector containing bibtex citation, or NULL if no entry found
#' @export
doi_lookup <- function(doi){
  if (!is_doi(doi)) return(NA_character_)

  # First, try crossref
  ref <- doi_lookup_crossref(doi)

  # If no result found, try dx.doi.org directly
  if(is.null(ref)){
    ref <- tryCatch({
      message("No result from crossref, querying dx.doi.org directly")
      ref <- doi_lookup_direct(doi)
    },
    warning = function(w){message("dx.doi.org: ", w); return(NULL)},
    error = function(e){message("dx.doi.org: ", e); return(NULL)}
    )
  }
  return(ref)
}


#' Lookup a doi on crossref.org and return the citation in bibtex format
#'
#' @param doi DOI identifier (or DOI hyperlink)
#' @return Character vector containing bibtex citation, or NULL if no entry found
#' @importFrom rcrossref::cr_cn
#' @export
doi_lookup_crossref <- function(doi) {
  if (!is_doi(doi)) return(NA_character_)

  tryCatch(
    rcrossref::cr_cn(doi = doi),
    warning = function(w){message("Crossref: ", w); return(NULL)},
    error = function(e){message("Crossref: ", e); return(NULL)}
  )
}

#' Lookup a doi on doi.org and return the citation in bibtex format
#'
#' @param doi DOI identifier (or DOI hyperlink)
#' @return Character vector containing bibtex citation, or NULL if no entry found
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr config
#' @importFrom httr content
#' @export
doi_lookup_direct <- function(doi) {
  if (!is_doi(doi)) return(NA_character_)
  bib <- httr::content(
    httr::GET(paste0("http://dx.doi.org/", doi),
              httr::add_headers(Accept = "application/x-bibtex; encoding=UTF-8"),
              httr::config(followlocation = TRUE)
    ),
    as = "text")
  if (grepl("DOI not found", bib)) return(NA_character_)
  return(bib)
}
