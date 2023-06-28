#' Compute intraclass correlation coefficients with variances and covariances
#'
#' @description These functions compute intraclass correlation coefficients
#' (ICCs), as well as corresponding large sample variances and covariances based
#' on the unconditional variance components in multilevel designs with up to
#' four hierarchical levels (L), implementing the formulas given in
#' Donner & Koval (1980) and Hedges et al. (2012).
#'
#'
#' \itemize{
#'  \item `icc.2l.balanced()` computes the ICC at L2 and its variance
#'  in a balanced two-level design (Hedges et al., 2012, Equation (1)).
#'  \item `icc.2l.unbalanced()` computes the ICC at L2 and its variance
#'  in an unbalanced two-level design (Donner & Koval, 1980, Equation (3)).
#'  \item `icc.3l.balanced()` computes the ICC at L2 and L3, their variances,
#'  and covariance in a balanced three-level design
#'  (Hedges et al., 2012, Equations (4) to (6)).
#'  \item `icc.3l.unbalanced()` computes the ICC at L2 and L3, their variances,
#'  and the covariance between the variance components at L2 and L3 in an
#'  unbalanced three-level design (Hedges et al., 2012, Equations (7) to (9)).
#'  \item `icc.4l.balanced()` computes the ICC at L2, L3, and L4, their
#'  variances, and covariances in a balanced four-level design
#'  (Hedges et al., 2012, Equations (10) to (15)).
#' }
#'
#' @param data A data frame. See details.
#' @param var_l1 <[`data-masked`][dplyr::dplyr_data_masking]>
#' The column name of the variance component at L1.
#' @param var_l2 <[`data-masked`][dplyr::dplyr_data_masking]>
#' The column name of the variance component at L2.
#' @param var_l3 <[`data-masked`][dplyr::dplyr_data_masking]>
#' The column name of the variance component at L3.
#' @param var_l4 <[`data-masked`][dplyr::dplyr_data_masking]>
#' The column name of the variance component at L4.
#' @param se_var_l2 <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the standard error of the variance
#' component at L2.
#' @param se_var_l3 <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the standard error of the variance
#' component at L3.
#' @param se_var_l4 <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the standard error of the variance
#' component at L4.
#' @param N <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the total sample size at L1.
#' @param j <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the average cluster size at L3
#' (e.g., the (harmonic) mean or median number of L2 clusters per L3 cluster).
#' @param k <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the average cluster size at L4
#' (e.g., the (harmonic) mean or median number of L3 clusters per L4 cluster).
#' @param n_per_l2 <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the (varying) cluster sizes at L2
#' (i.e., the number of L1 units in each L2 cluster).
#' Cluster sizes broken down for each cluster may be obtained via
#' [`multides::cluster_size()`][multides::cluster_size].
#' @param id_l3 <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The column name of the L3 cluster identifier.
#'
#'
#' @details
#' \strong{Intraclass correlation coefficients (ICCs)}
#'
#' Outcomes of observations within the same cluster are usually not independent
#' but rather correlated. The ICC informs on the degree of within-cluster
#' similarity. It quantifies the proportion of total variance in an outcome
#' that can be attributed to differences between clusters. Formally, the ICC
#' is the ratio of the between-cluster variance component to the sum of the
#' within- and between-cluster variance components. Although originated in the
#' two-level context (see Fisher, 1925), the concept
#' of the ICC naturally extends to more complex multilevel data structures.
#' For example, in a three-level design, where students at L1 are nested within
#' classrooms at L2, which are in turn nested within schools at L3 and the
#' outcome of interest is student achievement, the ICC at L2
#' (\eqn{\sigma^2_{L2}/\sigma^2_{Total}})
#' depicts between-classroom (within-school) achievement differences
#' and the ICC at L3
#' (\eqn{\sigma^2_{L3}/\sigma^2_{Total}})
#' depicts between-school achievement differences
#' (where \eqn{\sigma^2_{Total}=\sigma^2_{L1}+\sigma^2_{L2}+\sigma^2_{L3}}).
#'
#' \strong{Balanced and unbalanced designs}
#'
#' Following Hedges et al. (2012), the functions provided differentiate
#' between balanced and unbalanced designs.
#' \itemize{
#'  \item A balanced design assumes equal cluster sizes (i.e., all clusters
#'  have the same number of lower-level units). Here, the cluster size may
#'  be an average across clusters, e.g., in terms of the (harmonic) mean or
#'  median number of lower-level units per cluster.
#'  \item An unbalanced design, in contrast, allows the clusters to vary in
#'  their size (i.e., clusters may contain a different number of lower-level
#'  units).
#' }
#' The differentiation between balanced and unbalanced designs is only
#' relevant to compute the (co)variances for the multilevel variance component
#' structure. Thus, when only ICCs should be returned (by not specifying the
#' arguments on standard errors and sample or cluster sizes), the functions for
#' balanced and unbalanced designs reveal equivalent results.
#'
#' \strong{Structure of the data frame supplied to `data`}
#'
#' If a grouped data frame is supplied to `data`, this same grouping structure
#' will be retained in the new data frame.
#'
#' In unbalanced designs, the data frame supplied to `data` should have one
#' single row for each L2 cluster and should contain a variable supplied
#' to `n_per_l2` that informs on the size of each L2 cluster
#' (i.e., the number of L1 units in each L2 cluster). To generate a data frame
#' of this structure, the [`multides::cluster_size()`][multides::cluster_size]
#' function can be used while keeping only distinct rows for the L2 cluster
#' identifier.
#' Note that in unbalanced designs, the results are summarized across cluster
#' sizes, unless (co)variances are not computed.
#'
#' \strong{Further notes}
#'
#' \itemize{
#'  \item The variance of the ICC in an unbalanced
#'  two-level design (as formulated in Donner & Koval, 1980, Equation (3)) does
#'  not depend on the standard error of the variance component at L2, but rather
#'  solely on the total sample size at L1 and the (varying) cluster sizes at L2.
#'  \item Covariances between ICCs at different hierarchical levels are computed
#'  for balanced designs only. Note that the covariance computed for an
#'  unbalanced three-level design is \emph{not} the covariance between
#'  the ICCs at L2 and L3, but rather the covariance between the variance
#'  components at L2 and L3.
#'  \item If standard errors of variance components and/or sample or cluster
#'  sizes are not supplied, no (co)variances will be returned. In this case,
#'  a respective message is printed.
#' }
#'
#'
#'
#' @return A data frame that contains the following estimates:
#' \itemize{
#'  \item `icc.2l.balanced()` and `icc.2l.unbalanced()`
#' \itemize{
#'  \item `icc` The ICC.
#'  \item `var_icc` The variance of the ICC.
#'  }
#'  \item `icc.3l.balanced()`
#' \itemize{
#'  \item `icc_l2` The ICC at L2.
#'  \item `var_icc_l2` The variance of the ICC at L2.
#'  \item `icc_l3` The ICC at L3.
#'  \item `var_icc_l3` The variance of the ICC at L3.
#'  \item `cov_icc_l2_l3` The covariance of the ICCs at L2 and L3.
#'  }
#'  \item `icc.3l.unbalanced()`
#' \itemize{
#'  \item `icc_l2` The ICC at L2.
#'  \item `var_icc_l2` The variance of the ICC at L2.
#'  \item `icc_l3` The ICC at L3.
#'  \item `var_icc_l3` The variance of the ICC at L3.
#'  \item `cov_var_l2_l3` The covariance of the variance components at L2
#'  and L3.
#'  }
#'  \item `icc.4l.balanced()`
#' \itemize{
#'  \item `icc_l2` The ICC at L2.
#'  \item `var_icc_l2` The variance of the ICC at L2.
#'  \item `icc_l3` The ICC at L3.
#'  \item `var_icc_l3` The variance of the ICC at L3.
#'  \item `icc_l4` The ICC at L4.
#'  \item `var_icc_l4` The variance of the ICC at L4.
#'  \item `cov_icc_l2_l3` The covariance of the ICCs at L2 and L3.
#'  \item `cov_icc_l2_l4` The covariance of the ICCs at L2 and L4.
#'  \item `cov_icc_l3_l4` The covariance of the ICCs at L3 and L4.
#'  }
#' }
#'
#' @references
#' Donner, A., & Koval, J. J. (1980). The large sample variance of an
#' intraclass correlation.
#' \emph{Biometrika}, 67(3), 719–722. https://doi.org/10.1093/biomet/67.3.719
#'
#' Fisher, R. A. (1925). \emph{Statistical methods for research workers}.
#' Oliver & Boyd.
#'
#' Hedges, L. V., Hedberg, E. C., & Kuyper, A. M. (2012).
#' The variance of intraclass correlations in three- and four-level models.
#' \emph{Educational and Psychological Measurement}, 72(6), 893–909.
#' https://doi.org/10.1177/0013164412445193
#'
#'
#' @examples
#' set.seed(42)
#' dat <- data.frame(v_l1 = runif(1,.5,1),
#'                   v_l2 = runif(1,0,.5),
#'                   se_v_l2 = runif(1,.01,.1))
#'
#' # compute ICC with corresponding sampling variance
#' # in a balanced two-level design
#' icc.2l.balanced(data = dat,
#'                 var_l1 = v_l1, var_l2 = v_l2, se_var_l2 = se_v_l2)
#'
#' @name icc
NULL
#> NULL

#' @rdname icc
#' @export
icc.2l.balanced <- function(data, var_l1, var_l2, se_var_l2) {

  # check arguments
  if(missing(var_l1) | missing(var_l2)) {
    stop("Both 'var_l1' and 'var_l2' must be supplied.")
  }

  # -- ICC
  out <- dplyr::mutate(data,
                       icc = {{var_l2}} / ({{var_l1}} + {{var_l2}}))

  # -- var(ICC)
  if(!missing(se_var_l2)) {
    out <- dplyr::mutate(out,
                         var_icc = ( (1-icc)^2 * {{se_var_l2}}^2 ) / ({{var_l1}} + {{var_l2}})^2 )
  }

  else {
    message("Note: 'se_var_l2' is missing. Var(ICC) is not computed.")
  }

  # collect relevant variables while keeping possibly existing grouping in `data`
  if(isTRUE(dplyr::is.grouped_df(out))) {

    existing_groups <- dplyr::group_vars(out)
    dplyr::select(out, dplyr::all_of(existing_groups), tidyr::contains("icc"))

  }

  else {
    dplyr::select(out, tidyr::contains("icc"))
  }
}



#' @rdname icc
#' @export
icc.2l.unbalanced <- function(data, var_l1, var_l2, N, n_per_l2) {

  # check arguments
  if(missing(var_l1) | missing(var_l2)) {
    stop("Both 'var_l1' and 'var_l2' must be supplied.")
  }

  # -- ICC
  out <- dplyr::mutate(data,
                       icc = {{var_l2}} / ({{var_l1}} + {{var_l2}}))

  # -- Var(ICC)
  if(!missing(N) & !missing(n_per_l2)) {

    # involve unequal cluster sizes
    out <- dplyr::summarize(out,
                            term1a = sum({{n_per_l2}} * ({{n_per_l2}} -1) * (1 + ({{n_per_l2}} - 1) * icc^2 ) /
                                           ((1 + ({{n_per_l2}} - 1) * icc)^2) ),
                            term2a = sum({{n_per_l2}} * ({{n_per_l2}} -1) / (1 + ({{n_per_l2}} - 1) * icc) ),
                            icc = unique(icc),
                            N = unique({{N}}),
                            .groups = "keep")

    out <- dplyr::summarize(out,
                            icc = icc,
                            var_icc = (2*N * (1-icc)^2) / (N*term1a - icc^2 * term2a^2),
                            .groups = "keep")
  }

  else {
    .args_def <- ls()[!ls() %in% "out"]
    .args_pass <- names(as.list(match.call())[-1])

    if (any(!.args_def %in% .args_pass)) {
      message(paste("Note:",
                    paste(setdiff(.args_def, .args_pass), collapse=", "),
                    "is missing. Var(ICC) is not computed."))
    }
  }

  # collect relevant variables while keeping possibly existing grouping in `data`
  if(isTRUE(dplyr::is.grouped_df(out))) {

    existing_groups <- dplyr::group_vars(out)
    dplyr::select(out, dplyr::all_of(existing_groups), tidyr::contains("icc"))

  }

  else {
    dplyr::select(out, tidyr::contains("icc"))
  }
}


#' @rdname icc
#' @export
icc.3l.balanced <- function(data, var_l1, var_l2, var_l3, se_var_l2, se_var_l3, j) {

  # check arguments
  if(missing(var_l1) | missing(var_l2) | missing(var_l3)) {
    stop("'var_l1', 'var_l2', and 'var_l3' must be supplied.")
  }

  out <- dplyr::mutate(data,
                       var_tot = {{var_l1}} + {{var_l2}} + {{var_l3}},
                       # -- ICC_L2
                       icc_l2 = {{var_l2}} / var_tot,
                       # -- ICC_L3
                       icc_l3 = {{var_l3}} / var_tot)


  if(!missing(se_var_l2) & !missing(se_var_l3) & !missing(j)) {

    # -- Var(ICC_L2)
    out <- dplyr::mutate(out,
                         term1a = ( ({{j}} * (1-icc_l2)^2 + 2*icc_l2*(1-icc_l2) ) * {{se_var_l2}}^2) / ({{j}} * var_tot^2),
                         term2a = (icc_l2^2 * {{se_var_l3}}^2) / var_tot^2,
                         var_icc_l2 = term1a + term2a)

    # -- Var(ICC_L3)
    out <- dplyr::mutate(out,
                         term1b = ( ({{j}} * icc_l3^2 + 2*icc_l3*(1-icc_l3)) * {{se_var_l2}}^2) / ({{j}} * var_tot^2),
                         term2b = ((1-icc_l3)^2 * {{se_var_l3}}^2) / var_tot^2,
                         var_icc_l3 = term1b + term2b)


    # -- Cov(ICC_L2, ICC_L3)
    out <- dplyr::mutate(out,
                         term1c = ( -({{j}} * icc_l3 * (1-icc_l2) + icc_l2 * icc_l3 + (1-icc_l2) * (1-icc_l3) ) * {{se_var_l2}}^2 ) / ({{j}} * var_tot^2),
                         term2c = (icc_l2 * (1-icc_l3) * {{se_var_l3}}^2) / var_tot^2,
                         cov_icc_l2_l3 = term1c - term2c)

  }

  else {
    .args_def <- ls()[!ls() %in% "out"]
    .args_pass <- names(as.list(match.call())[-1])

    if (any(!.args_def %in% .args_pass)) {
      message(paste("Note:",
                    paste(setdiff(.args_def, .args_pass), collapse=", "),
                    "is missing. Var(ICC_L2), Var(ICC_L3), Cov(ICC_L2, ICC_L3) is not computed."))
    }
  }


  # collect relevant variables while keeping possibly existing grouping in `data`
  if(isTRUE(dplyr::is.grouped_df(out))) {

    existing_groups <- dplyr::group_vars(out)
    dplyr::select(out, dplyr::all_of(existing_groups), tidyr::ends_with("icc_l2"), tidyr::contains(c("icc_l3", "cov")))

  }

  else {
    dplyr::select(out, tidyr::ends_with("icc_l2"), tidyr::contains(c("icc_l3", "cov")))
  }
}


#' @rdname icc
#' @export
icc.3l.unbalanced <- function(data, var_l1, var_l2, var_l3, se_var_l2, se_var_l3, n_per_l2, id_l3) {

  # check arguments
  if(missing(var_l1) | missing(var_l2) | missing(var_l3)) {
    stop("'var_l1', 'var_l2', and 'var_l3' must be supplied.")
  }

  out <- dplyr::mutate(data,
                       var_tot = {{var_l1}} + {{var_l2}} + {{var_l3}},
                       # -- ICC_L2
                       icc_l2 = {{var_l2}} / var_tot,
                       # -- ICC_L3
                       icc_l3 = {{var_l3}} / var_tot)

  # -- Var(ICC_L2) and Var(ICC_L3)
  # involve unequal cluster sizes
  if(!missing(se_var_l2) & !missing(se_var_l3) & !missing(n_per_l2) & !missing(id_l3)) {

    # Cov(var_l2,var_l3)
    bi_ai <- dplyr::group_by(out, {{id_l3}}, .add = TRUE)

    bi_ai <- dplyr::summarize(bi_ai,
                              bi = sum({{n_per_l2}}^2 / ({{n_per_l2}} * {{var_l2}} + {{var_l1}})^2 ),
                              ai = sum({{n_per_l2}} / ({{n_per_l2}} * {{var_l2}} + {{var_l1}}) ),

                              var_tot = unique(var_tot),
                              icc_l2 = unique(icc_l2),
                              icc_l3 = unique(icc_l3),

                              var_l3 = unique({{var_l3}}),
                              se_var_l2 = unique({{se_var_l2}}),
                              se_var_l3 = unique({{se_var_l3}}),
                              .groups = "keep")

    bi_ai <- dplyr::ungroup(bi_ai, {{id_l3}})

    out <- dplyr::summarize(bi_ai,
                            cov_var_l2_l3 = - ( ( unique( se_var_l2^2) * (sum( bi / (1 + ai * var_l3) ) ) ) /
                                                  sum(ai^2 / (1 + ai * var_l3) ) ),

                            var_tot = unique(var_tot),
                            icc_l2 = unique(icc_l2),
                            icc_l3 = unique(icc_l3),

                            se_var_l2 = unique(se_var_l2),
                            se_var_l3 = unique(se_var_l3),
                            .groups = "keep")

    # -- Var(ICC_L2)
    out <- dplyr::mutate(out,
                         term1a = ((1-icc_l2)^2 * se_var_l2^2) / var_tot^2,
                         term2a = (icc_l2^2 * se_var_l3^2) / var_tot^2,
                         term3a = (2 * icc_l2 * (1-icc_l2) * cov_var_l2_l3) / var_tot^2,
                         var_icc_l2 = term1a + term2a - term3a)


    # -- var(ICC_L3)
    out <- dplyr::mutate(out,
                         term1b = (icc_l3^2 * se_var_l2^2) / var_tot^2,
                         term2b = ((1-icc_l3)^2 * se_var_l3^2) / var_tot^2,
                         term3b = (2 * icc_l3 * (1-icc_l3) * cov_var_l2_l3) / var_tot^2,
                         var_icc_l3 = term1b + term2b - term3b)
  }

  else {
    .args_def <- ls()[!ls() %in% "out"]
    .args_pass <- names(as.list(match.call())[-1])

    if (any(!.args_def %in% .args_pass)) {

      message(paste("Note:",
                    paste(setdiff(.args_def, .args_pass), collapse=", "),
                    "is missing. Var(ICC_L2), Var(ICC_L3),",
                    glue::glue("Cov({rlang::enexpr(var_l2)},{rlang::enexpr(var_l3)}) is not computed.")))
    }
  }


  # collect relevant variables while keeping possibly existing grouping structure in `data`
  if(isTRUE(dplyr::is.grouped_df(out))) {

    existing_groups <- dplyr::group_vars(out)
    dplyr::select(out, dplyr::all_of(existing_groups),
                  tidyr::contains(c("icc_l2", "icc_l3", "cov")))

  }

  else {
    dplyr::select(out,
                  tidyr::contains(c("icc_l2", "icc_l3", "cov")))
  }
}


#' @rdname icc
#' @export
icc.4l.balanced <- function(data, var_l1, var_l2, var_l3, var_l4, se_var_l2, se_var_l3, se_var_l4, j, k) {

  # check arguments
  if(missing(var_l1) | missing(var_l2) | missing(var_l3) | missing(var_l4)) {
    stop("'var_l1', 'var_l2', 'var_l3', and 'var_l4' must be supplied.")
  }

  out <- dplyr::mutate(data,
                       var_tot = {{var_l1}} + {{var_l2}} + {{var_l3}} + {{var_l4}},
                       # -- ICC_L2
                       icc_l2 = {{var_l2}} / var_tot,
                       # -- ICC_L3
                       icc_l3 = {{var_l3}} / var_tot,
                       # -- ICC_L4
                       icc_l4 = {{var_l4}} / var_tot)


  if(!missing(se_var_l2) & !missing(se_var_l3) & !missing(se_var_l4) & !missing(j) & !missing(k)) {


    # -- Var(ICC_L2)
    out <- dplyr::mutate(out,
                         term1a = (({{k}}*{{j}}^2 * (1-icc_l2)^2 + 2*{{k}}*{{j}}*icc_l2 * (1-icc_l2) + 2*icc_l2^2) * {{se_var_l2}}^2) / ({{k}}*{{j}}^2 * var_tot^2),
                         term2a = (({{k}}-2) * icc_l2^2 * {{se_var_l3}}^2) / ({{k}} * var_tot^2),
                         term3a = (icc_l2^2 * {{se_var_l4}}^2) / var_tot^2,
                         var_icc_l2 = term1a + term2a + term3a)

    # -- Var(ICC_L3)
    out <- dplyr::mutate(out,
                         term1b = (({{k}}*{{j}}^2 * icc_l3^2 + 2*({{k}}*{{j}}-1) * icc_l3 * (1-icc_l3)) * {{se_var_l2}}^2) / ({{k}}*{{j}}^2 * var_tot^2),
                         term2b = (({{k}} * (1-icc_l3)^2 + 2*icc_l3 * (1-icc_l3)) * {{se_var_l3}}^2) / ({{k}} * var_tot^2),
                         term3b = (icc_l3^2 * {{se_var_l4}}^2) / var_tot^2,
                         var_icc_l3 = term1b + term2b + term3b)

    # -- Var(ICC_L4)
    out <- dplyr::mutate(out,
                         term1c = (({{k}}*{{j}} * ({{j}}-2) * icc_l4^2 - 2*icc_l4 * (1-icc_l4)) * {{se_var_l2}}^2) / ({{k}}*{{j}}^2 * var_tot^2),
                         term2c = (({{k}}*icc_l4^2 + 2*icc_l4 * (1-icc_l4)) * {{se_var_l3}}^2) / ({{k}} * var_tot^2),
                         term3c = ((1-icc_l4)^2 * {{se_var_l4}}^2) / var_tot^2,
                         var_icc_l4 = term1c + term2c + term3c)


    # -- Cov(ICC_L2, ICC_L3)
    out <- dplyr::mutate(out,
                         term1d = ((-{{k}}*{{j}}^2 * (1-icc_l2) * icc_l3 - {{k}}*{{j}} * (icc_l2 * icc_l3 + (1-icc_l2)*(1-icc_l3)) -icc_l2*(1-icc_l3) + icc_l2*icc_l3) * {{se_var_l2}}^2) / ({{k}}*{{j}}^2 * var_tot^2),
                         term2d = (({{k}} * icc_l2*(1-icc_l3) - icc_l2*(1-icc_l3) + icc_l2*icc_l3) * {{se_var_l3}}^2) / ({{k}} * var_tot^2),
                         term3d = (icc_l2*icc_l3 * {{se_var_l4}}^2) / var_tot^2,
                         cov_icc_l2_l3 = term1d - term2d + term3d)

    # -- Cov(ICC_L2, ICC_L4)
    out <- dplyr::mutate(out,
                         term1e = ((-{{k}}*{{j}}^2 * (1-icc_l2) * icc_l4 + {{k}}*{{j}} * ((1-icc_l2) * icc_l4 - icc_l2 * icc_l4) + icc_l2*icc_l4 - icc_l2*(1-icc_l4)) * {{se_var_l2}}^2) / ({{k}}*{{j}}^2 * var_tot^2),
                         term2e = (({{k}}*icc_l2*icc_l4 - icc_l2*icc_l4 + icc_l2*(1-icc_l4)) * {{se_var_l3}}^2) / ({{k}} * var_tot^2),
                         term3e = (icc_l2*(1-icc_l4) * {{se_var_l4}}^2) / var_tot^2,
                         cov_icc_l2_l4 = term1e + term2e - term3e)

    # -- Cov(ICC_L2, ICC_L4)
    out <- dplyr::mutate(out,
                         term1f = (({{k}}*{{j}}^2 * icc_l3*icc_l4 + {{k}}*{{j}} * ((1-icc_l3)*icc_l4 - icc_l3*icc_l4) + icc_l3*icc_l4 - (1-icc_l3)*(1-icc_l4)) * {{se_var_l2}}^2) / ({{k}}*{{j}}^2 * var_tot^2),
                         term2f = (({{k}} * (1-icc_l3) * icc_l4 + icc_l3*icc_l4 + (1-icc_l3)*(1-icc_l4)) * {{se_var_l3}}^2) / ({{k}} * var_tot^2),
                         term3f = (icc_l3 * (1-icc_l4) * {{se_var_l4}}^2) / var_tot^2,
                         cov_icc_l3_l4 = term1f - term2f - term3f)
  }

  else {
    .args_def <- ls()[!ls() %in% "out"]
    .args_pass <- names(as.list(match.call())[-1])

    if (any(!.args_def %in% .args_pass)) {
      message(paste("Note:",
                    paste(setdiff(.args_def, .args_pass), collapse=", "),
                    "is missing.",
                    "Var(ICC_L2), Var(ICC_L3), Var(ICC_L4), Cov(ICC_L2,ICC_L3), Cov(ICC_L2,ICC_L4), Cov(ICC_L3,ICC_L4) is not computed."))
    }
  }


  # collect relevant variables while keeping possibly existing grouping in `data`
  if(isTRUE(dplyr::is.grouped_df(out))) {

    existing_groups <- dplyr::group_vars(out)
    dplyr::select(out, dplyr::all_of(existing_groups), tidyr::ends_with(c("icc_l2", "icc_l3", "icc_l4")), tidyr::contains("cov"))

  }

  else {
    dplyr::select(out, tidyr::ends_with(c("icc_l2", "icc_l3", "icc_l4")), tidyr::contains("cov"))
  }
}

# -- handle bare variable names
utils::globalVariables(c("ai", "bi", "cov_var_l2_l3",
                         "icc", "icc_l2", "icc_l3", "icc_l4", "se_var_l2", "se_var_l3",
                         "term1a", "term1b", "term1c", "term1d", "term1e", "term1f",
                         "term2a", "term2b", "term2c", "term2d", "term2e", "term2f",
                         "term3a", "term3b", "term3c", "term3d", "term3e", "term3f",
                         "var_tot"))
