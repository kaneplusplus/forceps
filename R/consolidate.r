
#' Consolidate a set of similarly-cohorted data sets
#'
#' @param data_list the list of similarly-cohorted data sets.
#' @param on the variable to consolidate on.
#' @importFrom purrr reduce
#' @export
consolidate <- function(data_list, on) {
  reduce(data_list, full_join, by = on)
}
