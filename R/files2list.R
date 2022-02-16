#' Load Multiple Data Files into one List
#'
#' @description These functions load multiple data files
#' from a specified directory and save them into one list.
#' \itemize{
#'  \item `rda2list()` loads multiple .rda files
#'  \item `dat2list()` loads multiple .dat files
#'  \item `csv2list()` loads multiple .csv files
#' }
#'
#' @param .path A string indicating the path of the directory
#' that contains the files to be loaded. The default is the
#' current working directory.
#' @param .file_pattern A regular expression to return the files that
#' this regular expression matches.
#' \itemize{
#'  \item For `rda2list()` the default is to load all files ending with ".rda"
#'  \item For `dat2list()` the default is to load all files ending with ".dat"
#'  \item For `csv2list()` the default is to load all files ending with ".csv"
#' }
#' @param .recursive A logical indicating whether to recurse
#' into directories. The default is FALSE.
#' @param .sep The field separator character.
#' The default is a semicolon.
#' @param .dec The decimal point character used in the files.
#' The default is a point.
#' @param .header A logical indicating whether the files have a column header.
#'
#' @return A list that contains the files in `.path` matching
#' the expression specified in `.file_pattern`.
#'
#' @seealso See also `utils::read.table()`.
#'
#' @examples
#' # load all .rda files in the current working directory into one list
#' rda2list()
#' # load all .dat files in the current working directory into one list
#' dat2list()
#' # load all .csv files in the current working directory into one list
#' csv2list()
#'
#' @name files2list
NULL
#> NULL

#' @rdname files2list
#' @export
rda2list <- function(.path = ".", .file_pattern = ".rda$", .recursive = FALSE) {

  files <- list.files(path = .path, pattern = .file_pattern, recursive = .recursive)

  list_rda <- function(x) {
    e <- rlang::new_environment()
    load(x, envir = e)
    as.list(e)
  }

  out <- purrr::map(.x = file.path(.path, files), .f = list_rda)
  out <- purrr::flatten(out)
  names(out) <- tools::file_path_sans_ext(files)
  return(out)
}

#' @rdname files2list
#' @export
dat2list <- function(.path = ".", .file_pattern = ".dat$", .recursive = FALSE, .sep = ";", .dec = ".", .header = TRUE) {

  files <- list.files(path = .path, pattern = .file_pattern, recursive = .recursive)

  list_dat <- function(x) {utils::read.table(x, sep = .sep, dec = .dec, header = .header)}

  out <- purrr::map(.x = file.path(.path, files), .f = list_dat)
  names(out) <- tools::file_path_sans_ext(files)
  return(out)
}

#' @rdname files2list
#' @export
csv2list <- function(.path = ".", .file_pattern = ".csv$", .recursive = FALSE, .sep = ";", .dec = ".", .header = TRUE) {

  files <- list.files(path = .path, pattern = .file_pattern, recursive = .recursive)

  list_csv <- function(x) {utils::read.csv(x, sep = .sep, dec = .dec, header = .header)}

  out <- purrr::map(.x = file.path(.path, files), .f = list_csv)
  names(out) <- tools::file_path_sans_ext(files)
  return(out)
}
