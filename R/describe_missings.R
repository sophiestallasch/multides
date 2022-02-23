#' Compute missing statistics
#'
#' @description This function provides missing statistics by calculating
#' a) the total sparseness and b) the missing percentage per column.
#'
#' @param data A data frame.
#'
#' @return A tibble with two columns:
#' \itemize{
#'  \item{`variable`} {The original names of the columns in `data`
#'  the missing percentage was calculated for}
#'  \item{`missings`} {Percentage of missings}
#' }
#' The first row of the returned tibble gives
#' the total percentage of missings across all columns
#' (i.e., "Total sparseness").
#'
#' @export
#'
#' @examples
#' describe_missings(studach)
describe_missings <- function(data) {

  variable <- missings <- NULL

  tot <- round(sum(is.na(data)) / length(unlist(data)) * 100, 2)
  tot <- data.frame(variable = "Total sparseness", missings = tot)

  miss_var <- dplyr::summarize(data, dplyr::across(dplyr::everything(), ~ round(sum(is.na(.)) / length(.) * 100, 2)))
  miss_var <- tidyr::pivot_longer(miss_var, cols = dplyr::everything(), names_to = "variable", values_to = "missings")

  out <- dplyr::bind_rows(tot, miss_var)

  return(out)
}
