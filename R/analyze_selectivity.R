#' Quantify sample selectivity with reduced/cleaned data
#'
#' @description This function performs sensitivity analysis by
#' computing the effect size Cohen's d per column
#' to quantify sample selectivity of a reduced/cleaned (i.e., selective)
#' analysis sample (e.g., after excluding observations)
#' compared to its total base sample.
#'
#' @param .base_sample A data frame. Should contain the data of the
#' total base sample.
#' @param .analysis_sample A data frame. Should contain the data of the
#' reduced/cleaned analysis sample (e.g., after excluding observations).
#' The number of observations in the analysis sample should be
#' smaller than in the total base sample.
#' @param .cols <[`data-masked`][dplyr::dplyr_data_masking]>
#' The names or column numbers of the numeric variables
#' to compute Cohen's d for. Both `.base_sample` and `.analysis_sample`
#' must contain the columns specified.
#'
#' @return A tibble with 6 columns:
#' \itemize{
#'  \item{`variable`} {The column the sensitivity anlysis was performed for}
#'  \item{`mean_tbs`} {Mean in the total base sample}
#'  \item{`sd_tbs`} {Standard deviation in the total base sample}
#'  \item{`mean_as`} {Mean in the reduced/cleaned analysis sample}
#'  \item{`sd_as`} {Standard deviation in the reduced/cleaned analysis sample}
#'  \item{`d`} {Cohen's d quantifying the sample selectivity}
#' }
#'
#' @export
#'
#' @details Cohen's d is calculated with
#' \eqn{( M(reduced) - M(total) ) / SD(total)}.
#'
#' @examples
#' \dontrun{
#' analyze_selectivity(.base_sample = studach,
#' .analysis_sample = studach_reduced, .cols = gender:read)
#' }


analyze_selectivity <- function(.base_sample, .analysis_sample, .cols) {

  variable <- value <- NULL
  mean <- sd <- NULL
  dataframe <- NULL
  mean_tbs <- sd_tbs <- mean_as <- NULL

  mean_sd <- function(data) {
    out <- tidyr::pivot_longer(data, cols = dplyr::everything(), names_to = "variable", values_to = "value")
    out <- dplyr::group_by(out, variable)
    out <- dplyr::summarize(out, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
  }

  base <- dplyr::select(.base_sample, {{.cols}})
  analysis <- dplyr::select(.analysis_sample, {{.cols}})

  base <- mean_sd(base)
  analysis <- mean_sd(analysis)

  out <- dplyr::bind_rows(tbs = base, as = analysis, .id = "dataframe")

  out <- tidyr::pivot_wider(out, names_from = dataframe, values_from = mean:sd)
  out <- dplyr::mutate(out, d = (mean_as - mean_tbs) / sd_tbs)

  out <- out[, c("variable", "mean_tbs","sd_tbs", "mean_as", "sd_as", "d")]
  out <- dplyr::mutate(out, dplyr::across(-variable, ~ round(., 2)))

  return(out)
}
