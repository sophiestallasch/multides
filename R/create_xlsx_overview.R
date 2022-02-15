#' Create an Excel workbook providing an overview with descriptive statistics and correlations
#'
#' @description This function uses the \strong{openxlsx} package to create an Excel workbook
#' with multiple sheets that contain descriptive statistics and
#' correlations for a specified variable set.
#' There are options to specify a) a grouping variable to include descriptive statistics that
#' are calculated separately by group and b) up to two cluster identifiers
#' to compute multilevel correlations between the respective group means
#' at the specified hierarchical levels.
#'
#' @param data A data frame.
#' @param .cols <[`data-masked`][dplyr::dplyr_data_masking]>
#' The names or column numbers of the numeric variables
#' to create the overview for.
#' @param .file A character string specifying the file name and the path
#' where to save the .xlsx file.
#' The default is to save an .xlsx file named 'overview' in the current working directory.
#' @param .group <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The name or column number of the grouping variable
#' to compute descriptive statistics by group. The default is NULL.
#' @param .id_cluster_l2 <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The name or column number of the identifier for the second
#' hierarchical level (e.g., classrooms). The default is NULL.
#' @param .id_cluster_l3 <[`data-masked`][dplyr::dplyr_data_masking]>
#' Optional. The name or column number of the identifier for the third
#' hierarchical level (e.g., schools). The default is NULL.
#' @param .round Optional. An integer constant indicating the number of digits
#' for rounding the results. The default is to print 3 digits.
#'
#' @return An Excel workbook (as .xlsx file) saved in `.file` with multiple sheets
#' providing an overview with descriptive statistics and correlations
#' for the variables specified in `.cols`.
#'
#' @export
#'
#' @seealso See the \strong{openxlsx} package.
#'
#' @examples
#' \dontrun{
#' create_xlsx_overview(data = studach, .cols = gender:read,
#' .file = "student_achievement_overview.xlsx",
#' .group = ts, .id_cluster_l2 = id_cla, .id_cluster_l3 = id_sch,
#' .round = 3)
#' }

create_xlsx_overview <- function(data, .cols, .file = "overview.xlsx", .group = NULL, .id_cluster_l2 = NULL, .id_cluster_l3 = NULL, .round = 3) {

  cols <- rlang::enquos(.cols)
  group <- rlang::enquo(.group)
  id_l2 <- rlang::enquo(.id_cluster_l2)
  id_l3 <- rlang::enquo(.id_cluster_l3)

  # create workbook
  wb <- openxlsx::createWorkbook(.file)

  # overall descriptives across groups
  descr <- describe_stats(data, (!!!cols))
  descr <- dplyr::mutate(descr, dplyr::across(dplyr::everything()[-1], ~ round(., .round)))
  openxlsx::addWorksheet(wb, "descr")
  openxlsx::writeData(wb, descr, sheet = "descr", startRow = 1, startCol = 1)

  # descriptives by group, if .group is specified
  if (!rlang::quo_is_null(group)) {
    descr <- describe_stats(data, (!!!cols), .group = (!!group))
    descr <- dplyr::mutate(descr, dplyr::across(dplyr::everything()[-c(1:2)], ~ round(., .round)))
    openxlsx::addWorksheet(wb, paste0("descr_", rlang::as_label(group)))
    openxlsx::writeData(wb, descr, sheet = paste0("descr_", rlang::as_label(group)), startRow = 1, startCol = 1)
  }

  # single-level correlations (at the individual level of observations)
  cor_l1 <- correlate_ml(data, (!!!cols))
  cor_l1 <- dplyr::mutate(cor_l1, dplyr::across((!!!cols), ~ round(., .round)))
  openxlsx::addWorksheet(wb, "cor_l1")
  openxlsx::writeData(wb, cor_l1, sheet = "cor_l1", startRow = 1, startCol = 1)

  # multi-level correlations (at level 2), if .id_l2 is supplied
  if (!rlang::quo_is_null(id_l2)) {
    cor_l2 <- correlate_ml(data, (!!!cols), .id_cluster = (!!id_l2))
    cor_l2 <- dplyr::mutate(cor_l2, dplyr::across((!!!cols), ~ round(., .round)))
    openxlsx::addWorksheet(wb, "cor_l2")
    openxlsx::writeData(wb, cor_l2, sheet = "cor_l2", startRow = 1, startCol = 1)
  }

  # multilevel correlations (at level 3), if .id_l3 is supplied
  if (!rlang::quo_is_null(id_l3)) {
    cor_l3 <- correlate_ml(data, (!!!cols), .id_cluster = (!!id_l3))
    cor_l3 <- dplyr::mutate(cor_l3, dplyr::across((!!!cols), ~ round(., .round)))
    openxlsx::addWorksheet(wb, "cor_l3")
    openxlsx::writeData(wb, cor_l3, sheet = "cor_l3", startRow = 1, startCol = 1)
  }

  # save workbook
  openxlsx::saveWorkbook(wb, file = .file, overwrite = T)
  message(paste0("Workbook was saved in: ", getwd(), "/", .file))
}
