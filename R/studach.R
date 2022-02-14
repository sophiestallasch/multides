#' Sample Multilevel Student Data
#'
#' A hierarchical clustered \code{data.frame} with randomly generated data of students' achievement and sociodemographic characteristics
#' with students at level (L) 1 being nested within classrooms at L2, that are nested within schools at L3, that are, again, nested within
#' school types at L4.
#'
#' @format A data frame with 26870 rows and 12 variables:
#' \describe{
#'   \item{id_stu}{Student identifier}
#'   \item{id_cla}{Classroom identifier}
#'   \item{id_sch}{School identifier}
#'   \item{ts}{School type identifier}
#'   \item{ts_name}{Name of school type with vocational, intermediate, multitrack, comprehensive, and academic}
#'   \item{n_stu}{Number of students per classroom}
#'   \item{n_cla}{Number of classrooms per school}
#'   \item{n_sch}{Number of schools per school type}
#'   \item{gender}{Binary gender with 0 = male, 1 = female}
#'   \item{ses}{Socioeconomic status}
#'   \item{math}{Mathematics achievement}
#'   \item{read}{Reading achievement}
#' }
"studach"
