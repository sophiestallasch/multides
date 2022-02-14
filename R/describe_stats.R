#' Compute descriptive statistics
#'
#' @description This function provides a range of descriptive statistics
#' that are useful to explore numeric data.
#' If a grouping variable is specified, the descriptive statistics are
#' computed separately for each group.
#'
#'
#' @param data A data frame.
#' @param .cols <[`data-masked`][dplyr::dplyr_data_masking]>
#' The name/s or column number/s of the
#' numeric variable/s to compute descriptive statistics for.
#' @param .group <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The name of the grouping variable. The default is NULL.
#'
#' @return A tibble or a tibble grouped by `.group` containing
#' the following statistics:
#' \itemize{
#'  \item{`n`} {Number of valid observations}
#'  \item{`missings`} {Percentage of missings}
#'  \item{`mean`} {Mean}
#'  \item{`sd`} {Standard deviation}
#'  \item{`min`} {Minimum}
#'  \item{`p25`} {25th percentile}
#'  \item{`median`} {Median}
#'  \item{`p75`} {75th percentile}
#'  \item{`max`} {Maximum}
#'  \item{`mean_h`} {Harmonic mean}
#'  \item{`skewness`} {Skewness}
#'  \item{`kurtosis`} {Kurtosis}
#' }
#'
#' @export
#'
#' @examples
#' # -- Simple descriptive statistics across all observations
#'
#' # compute descriptive statistics for students' mathematics achievement
#'
#' # using the name of the variable
#' describe_stats(studach, .cols = math)
#'
#' # using the position by column number
#' describe_stats(studach, .cols = 11)
#'
#' # compute descriptive statistics for all numeric variables
#' describe_stats(studach, .cols = where(is.numeric))
#'
#' # -- Descriptive statistics by school type or gender
#' describe_stats(studach, .cols = math, .group = ts_name)
#' describe_stats(studach, .cols = math, .group = gender)
#'
describe_stats <- function(data, .cols, .group = NULL) {

  fn <- name <- value <- NULL

  cols <- rlang::enquos(.cols)
  group <- rlang::enquo(.group)

  out <- data

  # group data by .group if specified
  if(!rlang::quo_is_null(group)) {
    out <- dplyr::group_by(out, (!!group))
  }

  out <- dplyr::summarize(out, dplyr::across((!!!cols), list(n = ~ sum(!is.na(.)),
                                                             missings = ~ sum(is.na(.))/length(.),
                                                             mean = ~ mean(., na.rm = T),
                                                             sd = ~ stats::sd(., na.rm = T),
                                                             min = ~ min(., na.rm = T),
                                                             p25 = ~ stats::quantile(., probs = .25, na.rm = T),
                                                             median = ~ stats::median(., na.rm = T),
                                                             p75 = ~ stats::quantile(., probs = .75, na.rm = T),
                                                             max = ~ max(., na.rm = T),
                                                             mean_h = ~ (1/mean((1/.), na.rm = T)),
                                                             skewness = ~ moments::skewness(., na.rm = T),
                                                             kurtosis = ~ moments::kurtosis(., na.rm = T)),
                                             .names = c("{.fn}.{.col}")))

  # simple descriptive statistics across all observations
  if(rlang::quo_is_null(group)) {
    out <- tidyr::pivot_longer(out, dplyr::everything())
  }

  # descriptive statistics by group
  else {
    out <- tidyr::pivot_longer(out, -(!!group))
  }

  out <- dplyr::mutate(out, fn = stringr::str_extract(name, "^[^.]+"), name = stringr::str_extract(name, "(?<=[.]).*"))
  out <- tidyr::pivot_wider(out, names_from = fn, values_from = value)
  out <- dplyr::rename(out, variable = name)
  return(out)
}
