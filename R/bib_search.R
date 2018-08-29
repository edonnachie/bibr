#' Search for pattern in a list of bibtex objects
#' @param bib List of bibtex objects
#' @param pattern to search for
#' @param fuzzy Whether to use fuzzy matching (agrep) (default: true)
#' @param ignore.case Whether to ignore case (default: true)
#' @param regexp Whether pattern is a regular expression (default: true)
#' @param ... Further parameters passed to agrep or grep
#' @return List of bibtex objects matching the pattern
search_bibentries <- function(bib, pattern,
                              fuzzy = TRUE,
                              ignore.case = TRUE,
                              regexp = TRUE,
                              ...){
  if(fuzzy)
    res <- agrep(pattern, bib,
                 fixed = !regexp, ignore.case = ignore.case, ...)
  else
    res <- grep(pattern, bib,
                fixed = !regexp, ignore.case = ignore.case, ...)
  bib[res]
}

#' Search through data.frame containing file metadata
#' @param files data.frame with file metadata
#' @param pattern Pattern to search for
#' @param fuzzy Whether to use fuzzy matching (agrep) (default: true)
#' @param ignore.case Whether to ignore case (default: true)
#' @param regexp Whether pattern is a regular expression (default: true)
#' @param ... Further parameters passed to agrep or grep
#' @return rows of data frame matching pattern
#' @export
search_files <- function(files, pattern,
                         fuzzy = TRUE,
                         ignore.case = TRUE,
                         regexp = TRUE,
                         ...){
  if(fuzzy)
    res <- agrep(pattern, with(files, paste(file, author, title, year, doi)),
                 fixed = !regexp, ignore.case = ignore.case, ...)
  else
    res <- grep(pattern, with(files, paste(file, author, title, year, doi)),
                fixed = !regexp, ignore.case = ignore.case, ...)
  files[res, ]
}

#' Search library (i.e. combination of search_bibtex and search_files)
#' @param pattern Pattern to search for
#' @param bib List of bibtex objects
#' @param files data.frame with file metadata
#' @param fuzzy Whether to use fuzzy matching (agrep) (default: true)
#' @param ignore.case Whether to ignore case (default: true)
#' @param regexp Whether pattern is a regular expression (default: true)
#' @param ... Further parameters passed to agrep or grep
#' @return rows of data frame matching pattern
#' @export
search_library <- function(pattern, bib, files,
                           fuzzy = TRUE,
                           ignore.case = TRUE,
                           regexp = TRUE,
                           ...){
  list(
    files = search_files(files, pattern,
                         fuzzy = fuzzy, ignore.case = ignore.case,
                         regexp = regexp, ...),
    bib = search_bibentries(bib, pattern,
                            fuzzy = fuzzy, ignore.case = ignore.case,
                            regexp = regexp, ...)
  )
}
