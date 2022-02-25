#' Create a composite measure
#'
#' @description This function integrates a range of specified variables
#' into a simple composite measure
#' by computing the mean for each observation.
#' By default, the respective variables are standardized in advance.
#'
#' @param data A data frame.
#' @param .cols <[`data-masked`][dplyr::dplyr_data_masking]>
#' The names or column numbers of the numeric variables to be integrated into
#' the composite measure.
#' @param .composite The name of the new column for the composite measure.
#' If no name is supplied, the new composite measure will be named "composite".
#' @param .scale A logical indicating whether columns should be scaled in
#' advance (i.e., by means of z standardization). The default is TRUE.
#'
#' @return An tibble containing the new composite measure named as specified in
#' `.composite`.
#'
#' @importFrom rlang :=
#'
#' @export
#'
#' @examples
#' compose(data = studach, .cols = math:read, .composite = "achievement")
compose <- function(data, .cols, .composite = "composite", .scale = TRUE) {

  .id <- NULL

  if(rlang::is_true(.scale)) {

    data <- dplyr::mutate(data, .id = 1:dplyr::n())

    out <- dplyr::mutate(data, dplyr::across({{.cols}}, ~ (.- mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

    out <- dplyr::rowwise(out)
    out <- dplyr::mutate(out, {{.composite}} := mean(dplyr::c_across({{.cols}}), na.rm = FALSE))
    out <- dplyr::ungroup(out)

    out <- dplyr::select(out, .id, {{.composite}})

    out <- merge(data, out, by = ".id", suffixes = c("",".composite"))
    out <- dplyr::select(out, -.id)

  } else if (rlang::is_false(.scale)){

  out <- dplyr::rowwise(data)
  out <- dplyr::mutate(out, {{.composite}} := mean(dplyr::c_across({{.cols}}), na.rm = FALSE))
  out <- dplyr::ungroup(out)

  } else {stop("`.scale` must be logical")}

  return(out)
}
