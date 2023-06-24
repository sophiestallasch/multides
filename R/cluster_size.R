#' Compute cluster sizes in multilevel designs
#'
#' @description This function counts the number of unique observations within
#' each of the user-defined clusters in the context of multilevel
#' (i.e., clustered or nested) data with an arbitrary number of hierarchical
#' levels (L).
#' For example, when students at L1 are nested within classrooms at L2, which
#' are in turn nested within schools at L3, the function computes the cluster
#' size at L2 (i.e., number of students per classroom) and the cluster sizes
#' at L3 (i.e., number of students per school and number of classrooms per
#' school).
#' By default, cluster sizes are returned broken down by L1 unit.
#' Instead of returning cluster sizes broken down by L1 unit, it is also
#' possible to compute summary statistics by supplying the respective summary
#' functions to the `.fns` argument.
#' Total sample sizes at each hierarchical level can be included via the
#' `.total`argument.
#'
#' @param .data A data frame.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#' The names or column numbers of the identifiers depicting the cluster
#' structure in ascending order (e.g., students, classrooms, schools:
#' `id_student, id_classroom, id_school`). At least two identifiers must be
#' specified. Note that if `.fns` is not supplied, the first identifier will be
#' the first column in the new data frame.
#' @param .fns Optional. A list of function/s to compute summary statistics
#' across cluster sizes (e.g., median cluster size:
#' `.fns = list("mdn" = median)`)
#' @param .total Optional. A logical indicating whether the number of unique
#' observations at each hierarchical level should be summed up. If TRUE, the
#' total sample size at each hierarchical level is included in the new data
#' frame. The default is FALSE.
#'
#' @return A data frame.
#'
#' @export
#'
#' @examples
#'
#' # compute cluster sizes in a three-level design
#' # (e.g., students within classrooms within schools)
#' cluster_size(studach, id_stu, id_cla, id_sch)
#'
#' # compute median cluster sizes in a two-level design
#' # (e.g., students within schools) and include total
#' # and include total sample sizes at each hierarchical level
#' cluster_size(studach, id_stu, id_sch,
#'              .fns = list("mdn" = median),
#'              .total = TRUE)

cluster_size <- function(.data, ..., .fns, .total = FALSE) {

  ids <- rlang::enexprs(...)
  out_list <- vector("list")

  # cluster sizes: long format, by lowest level ID
  if(missing(.fns)) {

    for (i in 1:(length(ids)-1)) {

      out_list[[i]] <- dplyr::group_by(.data, !!!ids[i+1], .add = TRUE)
      out_list[[i]] <- dplyr::reframe(out_list[[i]],
                                      "{ids[1]}" := unique(!!!ids[1]),
                                      "{ids[1]}_units_per_{ids[i+1]}" := length(unique(!!!ids[1])),
                                      "{ids[i]}_units_per_{ids[i+1]}" := length(unique(!!!ids[i])))
      out_list[[i]] <- dplyr::ungroup(out_list[[i]])

    }

    if (length(out_list) >= 2) {
      out <- out_list[[1]]
      for(i in 2:length(out_list)) out <- merge(out, out_list[[i]], by = rlang::as_string(ids[[1]]))
      out <- dplyr::reframe(out, dplyr::across(c(..., names(out)[!names(out) %in% ids]), ~ .))
    }

    else {
      out <- out_list[[1]]
      out <- dplyr::reframe(out, dplyr::across(c(..., names(out)[!names(out) %in% ids]), ~ .))
      }

  }

  # cluster sizes: summaries
  else {

    fns <- rlang::quos(.fns)

    for (i in 1:(length(ids)-1)) {

      out_list[[i]] <- dplyr::group_by(.data, !!!ids[i+1], .add = TRUE)
      out_list[[i]] <- dplyr::summarize(out_list[[i]],
                                        "{ids[i]}_units_per_{ids[i+1]}" := length(unique(!!!ids[i])),
                                        "{ids[1]}_units_per_{ids[i+1]}" := length(unique(!!!ids[1])))
      out_list[[i]] <- dplyr::ungroup(out_list[[i]])
      out_list[[i]] <- dplyr::summarize(out_list[[i]], dplyr::across(2:length(names(out_list[[i]])), (!!!fns), .names = "{.col}_{.fn}"))
    }

    out <- as.data.frame(dplyr::bind_cols(out_list) )
  }

  # include total sample sizes at each hierarchical level
  if (.total == TRUE) {
    n <- dplyr::summarize(.data, dplyr::across(c(...), ~ length(unique(.)), .names = "n_{.col}" ))
    out <- cbind(out, n)
  }

  return(out)
}


ignore_unused_imports <- function() {
  mitml::testEstimates()
}
