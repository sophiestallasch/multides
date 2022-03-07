#' Generate formulas for single- and multilevel linear modeling
#'
#' @description This function generates formula expressions from vectors of
#' outcome and predictor names for single- and multilevel linear models to be
#' fitted with `stats::lm()` and `lme4::lmer()`, respectively.
#' This function is especially helpful for generating a
#' large number of formulas with varying outcomes and predictors at once.
#'
#' @param .outcome A character vector of outcome names.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Optional.
#' Character vectors of predictor names.
#' @param .clustering Optional. The cluster structure
#' to specify random intercepts for multilevel formulas.
#' @param .random_slopes Optional. A list of character vectors
#' of predictor names for which random slopes should be included in the formula.
#' Only relevant if `.clustering` is specified.
#' @param .ranef_cor A logical indicating whether
#' random intercepts and slopes should be correlated.
#' Only relevant if `.random_slopes` is specified.
#' The default is TRUE.
#'
#' @return A character vector of formula expressions.
#'
#' @export
#'
#' @details
#' \itemize{
#'  \item Generating a \strong{single-level} formula for models to be fitted
#'  with `stats::lm()` simply requires not specifying `.clustering`.
#'  \item Generating a \strong{multilevel} formula for models to be fitted with
#'  `lme4::lmer()` requires the specification of one or multiple nested cluster
#'  identifiers in `.clustering`. So far, only one single random effects term
#'  of the form "`(random expression | .clustering)`" can be expressed.
#'  \itemize{
#'      \item To express a formula for a simple two-level model,
#'      specify a single cluster identifier
#'      (e.g., "`.clustering = id_schools`").
#'      To express a formula for a multilevel model with more than two
#'      hierarchical (i.e., nested) levels, specify multiple cluster
#'      identifiers separated by a slash, beginning with the highest level
#'      (e.g., "`.clustering = id_schools / id_classrooms`").
#'      \item `.random_slopes` is only evaluated if `.clustering` is specified.
#'      The input has to be a list as created with `list()` (e.g.,
#'      for including a random slope term for vector "`x1`", specify
#'      "`.random_slopes = list(x1)`").
#'      If no slopes are specified, a formula for a
#'      random intercept model is generated (i.e., "`(1 | .clustering)`").
#'      \item If applicable, (i.e., if `.clustering` and `.random_slopes`
#'      are specified), `.ranef_cor`, by default, includes a term for
#'      correlated random effects in the formula,
#'      which corresponds to one vertical bar operator as used in
#'      \strong{lme4} (i.e., "`|`"). Setting `.ranef_cor` to FALSE corresponds
#'      to two vertical bar operators as used in \strong{lme4} (i.e., "`||`")
#'      and leads to including a term for uncorrelated random effects.
#'     }
#'
#' \item If an element in `.outcome` is `NA`, the output for this element
#'  will also be `NA`.
#' \item `...` also takes a list of character vectors of predictor names that
#' can be spliced with `!!!`.
#' \item It is possible to include a varying number of predictors for each
#' outcome by setting the corresponding entries in the character vectors
#' specified in `...` and/or the list of character vectors specified
#' in `.random_slopes`  to `NA`.
#' }
#'
#' @seealso See the \strong{stats} and \strong{lme4} packages.
#'
#' @examples
#' # Formulize single-level models with one or two predictors
#' formulize(.outcome = c("y1", "y2"), c("x1", "x2"), c(NA, "z2"))
#' # [1] "y1 ~ 1 + x1"
#' # [2] "y2 ~ 1 + x2 + z2"
#'
#' # Formulize a three-level model with random intercepts
#' formulize(.outcome = c("y1", "y2"), c("x1", "x2"), c(NA, "z2"),
#' .clustering = id_schools/id_classrooms)
#' # [1] "y1 ~ 1 + x1 + (1 | id_schools/id_classrooms)"
#' # [2] "y2 ~ 1 + x2 + z2 + (1 | id_schools/id_classrooms)"
#'
#' # Formulize a two-level model with random intercept and slopes
#' formulize(.outcome = c("y1", "y2"), c("x1", "x2"), c(NA, "z2"),
#' .clustering = id_schools,
#' .random_slopes = list(c("x1", "x2"), c(NA, "z2")))
#' # [1] "y1 ~ 1 + x1 + (1 + x1 | id_schools)"
#' # [2] "y2 ~ 1 + x2 + z2 + (1 + x2 + z2 | id_schools)"

formulize <- function(.outcome, ..., .clustering, .random_slopes = NULL, .ranef_cor = TRUE) {

  if(!missing(.random_slopes) & !is.list(.random_slopes)) {
    stop("`.random_slopes` must be a list. Use list(): '.random_slopes = list(x, ...)'")
  }


  if(missing(...)) {
    fmla <- purrr::map_chr(.outcome, ~ paste0(., " ~ 1"))
  }

  else{
    preds <- rlang::list2(...)

    preds <- purrr::transpose(preds)
    preds <- purrr::simplify_all(preds)
    preds <- purrr::map(preds, stringi::stri_flatten, collapse = " + ", na_empty = TRUE, omit_empty = TRUE)

    fmla <- purrr::map2_chr(.outcome, preds, ~ ifelse(.y == "", paste(.x, .y, sep = " ~ 1"), paste(.x, .y, sep = " ~ 1 + ")))
  }

  if(!missing(.clustering)){

    clustering <- rlang::enexprs(.clustering)


    if(missing(.random_slopes)) {

      fmla <- purrr::map_chr(fmla, ~ paste0(., " + (1 | ", {{clustering}}, ")"))

      if(!missing(.ranef_cor)) {message("`.random_slopes` is not specified. Argument `.ranef_cor` is ignored")}
    }

    else{

      slopes <- purrr::transpose(.random_slopes)
      slopes <- purrr::simplify_all(slopes)
      slopes <- purrr::map(slopes, stringi::stri_flatten, collapse = " + ", na_empty = TRUE, omit_empty = TRUE)
      slopes <- purrr::map(slopes, ~ ifelse(. == "", "1", paste0("1 + ", .)))

      fmla <- purrr::map2_chr(fmla, slopes, paste, sep = " + (")

      if(isTRUE(.ranef_cor)) {
        fmla <- purrr::map_chr(fmla, ~ paste0(., " | ", {{clustering}}, ")"))
      }
      else if(isFALSE(.ranef_cor)){
        fmla <- purrr::map_chr(fmla, ~ paste0(., " || ", {{clustering}}, ")"))
      }
      else {stop("`.ranef_cor` must be logical")}
    }
  }
  else if(!missing(.random_slopes)) {message("`.clustering` is not specified. Argument `.random_slopes` is ignored")}
  else if(!missing(.ranef_cor)) {message("`.clustering` is not specified. Argument `.ranef_cor` is ignored")}
  else if(!missing(.random_slopes) & !missing(.ranef_cor)) {
    message("`.clustering` is not specified. Arguments `.random_slopes` and `.ranef_cor` are ignored")
  }

  fmla <- ifelse(grepl("^NA", fmla), NA, fmla)
  return(fmla)
}


