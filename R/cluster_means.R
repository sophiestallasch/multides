#' Compute cluster means
#'
#' @description This function computes the mean of a single or a range of
#' specified variable/s within each cluster.
#'
#' @param data A data frame.
#' @param .cols <[`data-masked`][dplyr::dplyr_data_masking]>
#' The name/s or column number/s of the numeric variable/s for which
#' cluster means should be computed.
#' @param .id_cluster <[`data-masked`][dplyr::dplyr_data_masking]>
#' The name or column number of the identifier for the clusters within which
#' means should be computed.
#' @param .suffix Optional. A string to be added to the original variable
#' name/s to label the new column/s for the cluster means. The default is the
#' suffix "_cl".
#'
#' @return An tibble containing the new cluster means indicated by the suffix
#' specified in `.suffix`.
#'
#' @export
#'
#' @examples
#' # Compute cluster means at the school level for mathematics
#' cluster_means(data = studach, .cols = math, .id_cluster = id_sch, .suffix = "_sch")
#'
#' # Compute cluster means at the classroom level for mathematics and reading
#' cluster_means(data = studach, .cols = math:read, .id_cluster = id_cla, .suffix = "_cla")
cluster_means <- function(data, .cols, .id_cluster, .suffix = "_cl") {
  out <- dplyr::group_by(data, {{.id_cluster}})
  out <- dplyr::mutate(out, dplyr::across({{.cols}}, ~ mean(., na.rm = T), .names = paste0("{.col}", .suffix)))
  out <- dplyr::ungroup(out)
  return(out)
}

