#' Clean a DOI to form a valid filename
#'
#' 1) "/" is converted to "_"
#' 2) "doi:" is removed
#' 3) file extension `ext` is appended (Default: "bib")
#'
#' @param doi DOI to clean
#' @param ext File extension to append (Default: "bib")
#' @return String with "/" converted to "_" and ".bib" appended
#' @export
doi_to_filename <- function(doi, ext = "bib"){
  paste(
    gsub("/", "_", sub("doi:", "", doi)),
    ext, sep = "."
    )
}



#' Retrieve the bibtex citation for a given DOI and save to a file.
#' Not vectorised!
#'
#' @param doi DOI to look up and and save to file
#' @param dir Directory in which to save the bibtex file (default: "bibtex")
#' @return The result of the rcrossref::cr_cn lookup
#' @export
doi_to_bibtex <- function(doi, dir = "bibtex"){
  # Clean doi
  doi <- doi_strip_url(doi)
  # Lookup doi
  ref <- doi_lookup(doi)
  # Save bibtex file
  if(!is.null(ref) &! is.null(dir)){
    fname <- doi_to_filename(doi)
    cat(ref, file = file.path(dir, fname))
  }
  return(ref)
}



#' Read a directory of bibtex files and return a list of bibentry objects
#' Files that cannot be read are moved to a subdirectory "bad"
#' @param dir Directory containing bibtex files
#' @return List of bibtex objects
#' @importFrom bibtex read.bib
#' @export
read_bib_dir <- function(dir){
  files <- list.files(dir, full.names = TRUE)

  # Need to filter out the directories,
  # because list.files won't just list the files
  files <- files[!dir.exists(files)]

  biblist <- lapply(files, function(f){
    tryCatch(bibtex::read.bib(f),
             error = function(e){
               dir_bad <- file.path(dir, "bad")
               dir.create(dir_bad, showWarnings = FALSE)
               file.rename(f, to = file.path(dir_bad, basename(f)))
               warning(paste("\nFile: ", f, "\nMoved to ", dir_bad, "\nError: ", e))
               return(NA)
             }
    )
  })

  return(biblist[which(!is.na(biblist))])
}


#' Retrieve a DOI from a bibentry object
#' @param bib Bibtex object
#' @return String containing doi
#' @export
doi_from_bibtex <- function(bib){
  as.character(sapply(bib, function(x) x[1]$doi))
}

#' Rename a bibtex file to either the cleaned DOI or,
#' if not present, the bibkey
#'
#' @param bibfile File containing a single bibtex entry
#' @return Nothing
#' @importFrom bibtex read.bib
#' @export
rename_bibtex_doi <- function(bibfile){
  dir <- dirname(bibfile)
  bib <- bibtex::read.bib(bibfile)
  doi <- doi_from_bibtex(bib)
  doi_fname <- doi_to_filename(doi)

  if(basename(bibfile) != doi_fname){
    cat("Renaming ", basename(bibfile), " to ", doi_fname, "\n")
    file.rename(bibfile, file.path(dir, doi_fname))
  }
}

#' Parse a bibtex file and save each entry to a separate file
#'
#' @param bibfile File containing (possibly) multiple bibtex entries
#' @param dir_to Directory in which to save the individual entries (default: NULL, use same directory as source file)
#' @return NULL
#' @export
split_bibtex <- function(bibfile, dir_to = NULL){
  if(is.null(dir_to)) dir_to <- dirname(bibfile)
  biblist <- bibtex::read.bib(bibfile)
  lapply(biblist, function(bib){
    f <- doi_to_filename(doi_from_bibtex(bib))
    if(f == "NULL") f <- names(bib[1])
    fname <- file.path(dir_to, f)
    bibtex::write.bib(bib, file = fname)
  })
}

