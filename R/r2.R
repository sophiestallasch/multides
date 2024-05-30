#' Compute (delta) squared multiple correlation coefficients with variances
#'
#' @description These functions compute squared multiple correlation
#' coefficients (i.e., proportions of explained variance) \eqn{R^2} for
#' (a set of) covariates based on (un)conditional variances and
#' differences \eqn{\Delta} between the \eqn{R^2} values of two nested covariate
#' models A and B (where the covariates in model B represent a subset of the
#' covariates in model A), as well as corresponding large-sample variances
#' implementing the formulas given in Hedges & Hedberg (2013) and
#' Alf & Graf (1999), respectively.
#'
#' \itemize{
#'  \item `r2()` computes \eqn{R^2} and its variance
#'  (Hedges & Hedberg, 2013, p. 451).
#'  \item `r2.delta()` computes \eqn{\Delta R^2} and its variance
#'  (Alf & Graf, 1999, Equations (19) and (21)).
#' }
#'
#' @param var A numeric vector of the unconditional (i.e., unadjusted)
#' variance.
#' @param var_adj A numeric vector of the conditional (i.e., covariate-adjusted)
#' variance.
#' @param N Optional. A numeric vector of the sample size. Necessary for
#' computing the variance.
#' @param trunc A string determining the treatment of negative
#' (\eqn{\Delta})\eqn{R^2} values. The default is "zero".
#' \itemize{
#'  \item "zero": Negative values are truncated to zero.
#'  \item "na": Negative values are set to NA.
#'  \item "none": Negative values are kept. Note that no variances are computed
#'  for negative (\eqn{\Delta})\eqn{R^2} values.
#' }
#' @param r2_full A numeric vector of \eqn{R^2} for a model A with a set of
#' covariates.
#' @param r2_reduced A numeric vector of \eqn{R^2} for a model B with a subset
#' of the covariates from model A.
#' @param var_delta0 A logical indicating whether variances of zero
#' \eqn{\Delta R^2} should be computed. The default is FALSE.
#'
#' @details
#' \strong{Squared multiple correlation coefficients}
#'
#' Covariates that are correlated with (or, put differently: explain variance
#' in) an outcome reduce error variance. \eqn{R^2} quantifies the proportion of
#' the total variance in an outcome that can be explained by one or more
#' covariates. Formally, \eqn{R^2} is the ratio of the difference between the
#' unconditional variance and the on covariates \eqn{C} conditional variance
#' to the unconditional variance (\eqn{(\sigma^2-\sigma^2_{|C})/\sigma^2}).
#'
#' \strong{Multilevel designs}
#'
#' In multilevel models, covariates may act at either hierarchical level.
#' Generally, group-mean centering of within-cluster covariates is
#' recommended to ensure that covariates explain variance exclusively at that
#' hierarchical level at which they are specified
#' (e.g., Konstantopoulos, 2012). Consider, for example, a three-level design,
#' where students at L1 are nested within classrooms at L2, which are in turn
#' nested within schools at L3.
#' Given (a set of) classroom-mean centered L1 covariates \eqn{C_{L1}},
#' \eqn{R^2_{L1}} is the ratio of the difference between the unconditional and
#' the conditional between-student-within-classroom variances to the
#' unconditional between-student-within-classroom variance located at L1
#' (\eqn{(\sigma^2_{L1}-\sigma^2_{L1|C_{L1}})/\sigma^2_{L1}});
#' given (a set of) school-mean centered L2 covariates \eqn{C_{L2}},
#' \eqn{R^2_{L2}} is the ratio of the difference between the unconditional and
#' the conditional between-classroom-within-school variances to the
#' unconditional between-classroom-within-school variance located at L2
#' (\eqn{(\sigma^2_{L2}-\sigma^2_{L2|C_{L2}})/\sigma^2_{L2}}); and finally,
#' given (a set of) L3 covariates \eqn{C_{L3}},
#' \eqn{R^2_{L3}} is the ratio of the difference between the unconditional and
#' the conditional between-school variances to the
#' unconditional between-school variance located at L3
#' (\eqn{(\sigma^2_{L3}-\sigma^2_{L3|C_{L3}})/\sigma^2_{L3}}).
#'
#' \strong{Delta squared multiple correlation coefficients}
#'
#' \eqn{\Delta R^2} quantifies the increment in the proportion of explained
#' variance when adding covariates to a model. Formally, \eqn{\Delta R^2} is the
#' difference between the \eqn{R^2} of a full model A with a certain set of
#' covariates and the \eqn{R^2} of a reduced model B with a subset of those
#' covariates that are included in model A (\eqn{R^2_A-R^2_B}).
#'
#' @return A matrix that contains:
#' \itemize{
#'  \item `r2()`
#' \itemize{
#'  \item `Estimate` \eqn{R^2}.
#'  \item `Variance` Variance of \eqn{R^2}, if `N` is supplied.
#'  }
#'  \item `r2.delta()`
#' \itemize{
#'  \item `Estimate` \eqn{\Delta}\eqn{R^2}.
#'  \item `Variance` Variance of \eqn{\Delta}\eqn{R^2}, if `N` is supplied.
#'  }
#' }
#'
#' @references
#' Alf, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for the
#' difference between two squared multiple correlations: A simplified approach.
#' \emph{Psychological Methods}, \emph{4}(1), 70–75.
#' https://doi.org/10.1037/1082-989X.4.1.70
#'
#' Hedges, L. V., & Hedberg, E. C. (2013). Intraclass correlations and
#' covariate outcome correlations for planning two- and three-level
#' cluster-randomized experiments in education. \emph{Evaluation Review},
#' \emph{37}(6), 445–489. https://doi.org/10.1177/0193841X14529126
#'
#' Konstantopoulos, S. (2012). The impact of covariates on statistical power
#' in cluster randomized designs: Which level matters more?
#' \emph{Multivariate Behavioral Research}, \emph{47}(3), 392–420.
#' https://doi.org/10.1080/00273171.2012.673898
#'
#'
#' @examples
#' # compute R-squared and its sampling variance
#' r2(var = 3.25, var_adj = 1.75, N = 100, trunc = "zero")
#'
#' # compute Delta R-squared and its sampling variance
#' r2.delta(r2_full = .75, r2_reduced = .50, N = 100, trunc = "zero", var_delta0 = FALSE)
#'
#'
#' @export
r2 <- function(var, var_adj, N = NULL, trunc = c("zero", "na", "none")) {

  trunc <- match.arg(trunc)

  # -- R-squared
  est <- (var - var_adj) / var

  est <- switch(trunc,
                "zero" = ifelse(est >= 0, est, 0),
                "na" = ifelse(est >= 0, est, NA),
                "none" = est
  )

  # -- Variance of R-squared
  if(!is.null(N)) {

    var <- 4 * est * (1 - est)^2 / N

    var <- ifelse(var >= 0, var, NA)

    out <- matrix(c(est, var), ncol = 2)
    colnames(out) <- c("Estimate", "variance")
  }

  else {
    out <- matrix(est, ncol = 1)
    colnames(out) <- "Estimate"
    message("Note: Only R-squared was computed. Specify `N` to compute the variance.")
  }

  return(out)
}

#' @rdname r2
#' @export
r2.delta <- function(r2_full, r2_reduced, N = NULL, trunc = c("zero", "na", "none"), var_delta0 = FALSE) {

  trunc <- match.arg(trunc)

  r2_full <- ifelse(r2_full >= 0, r2_full, 0)
  r2_reduced <- ifelse(r2_reduced >= 0, r2_reduced, 0)

  # -- Delta R-squared
  est <- r2_full - r2_reduced


  # -- Variance of Delta R-squared
  if(!is.null(N)) {

    f <- r2_full
    r <- r2_reduced

    var <- ifelse(est >= 0,
                  4*f * (1-f)^2 / N + 4*r * (1-r)^2 / N - 8*sqrt(f)*sqrt(r) *
                    (.5 * (2*(sqrt(r)/sqrt(f))-sqrt(f) * sqrt(r)) *
                       (1 - f - r - (sqrt(r)/sqrt(f))^2) + (sqrt(r)/sqrt(f))^3) / N,
                  NA)

    if(var_delta0 == FALSE) {
      var <- ifelse(est != 0, var, NA)
    }

    var <- ifelse(var >= 0, var, NA)

    est <- switch(trunc,
                  "zero" = ifelse(est >= 0, est, 0),
                  "na" = ifelse(est >= 0, est, NA),
                  "none" = est
    )

    out <- matrix(c(est, var), ncol = 2)
    colnames(out) <- c("Estimate", "variance")
  }

  else {
    est <- switch(trunc,
                  "zero" = ifelse(est >= 0, est, 0),
                  "na" = ifelse(est >= 0, est, NA),
                  "none" = est
    )

    out <- matrix(est, ncol = 1)
    colnames(out) <- "Estimate"
    message("Note: Only Delta R-squared was computed. Specify `N` to compute the variance.")
  }

  return(out)
}
