#' Compute single- or multilevel correlations
#'
#' @description This function computes correlations between a series of specified variables.
#' If no cluster identifier is specified, the function provides simple
#' single-level correlations at the individual level of observations.
#' If a cluster identifier is specified, the function provides
#' multilevel correlations between the respective group means
#' at the specified hierarchical level.
#'
#' @param data A data frame.
#' @param .cols <[`data-masked`][dplyr::dplyr_data_masking]>
#' The names or column numbers of the numeric variables
#' to compute correlations for.
#' @param .id_cluster <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The name or column number of the cluster identifier
#' to compute multilevel correlations.
#'
#' @return A tibble containing a correlation matrix of variables specified in `.cols`.
#'
#' @export
#'
#' @examples
#' # compute single-level correlations between students' mathematics and reading achievement
#' correlate_ml(data = studach, .cols = read:math)
#'
#' # compute multilevel correlations between students' mathematics and reading achievement
#' # at the school level
#' correlate_ml(data = studach, .cols = c(gender, read, math), .id_cluster = id_sch)
correlate_ml <- function(data, .cols, .id_cluster) {

  term <- NULL

  cols <- rlang::enquos(.cols)
  id_cluster <- rlang::enquo(.id_cluster)

  # for single-level correlations
  if(rlang::quo_is_missing(id_cluster)) {
    out <- dplyr::select(data, (!!!cols))
  }

  # for multilevel correlations between group means at .id_cluster
  else {

    out <- dplyr::group_by(data, (!!id_cluster))
    out <- dplyr::summarize(out, dplyr::across((!!!cols), ~ mean(.x, na.rm = T)))
    out <- dplyr::select(out, -(!!id_cluster))
  }

  # generate correlation matrix
  out <- corrr::correlate(out)
  out <- dplyr::rename(out, variable = term)
  return(out)
}
