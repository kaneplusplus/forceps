
#' @importFrom crayon red
attr_or_na <- function(x, attr) {
  ret <- c()
  for (j in seq_len(ncol(x))) {
    v <- attributes(x[[j]])[[attr]]
    if (length(v) > 1) {
      v <- paste(v, collapse = ":")
    }
    if (is.null(v)) {
      v <- NA
    }
    ret <- c(ret, v)
  }
  if (ncol(x) != length(ret)) {
    stop(red("Column-return mismatch."))
  }
  ret
}

#' @title Describe a data set
#'
#' @description An ADaM-formatted .sas7bdat file includes a minimal amount
#' of information about each of the variables, which are stored as
#' attributes. This fuction "describes" these data sets and includes
#' the variable names, descriptions, types, and other summary information.
#' @param x the data.frame with label attributes..
#' @param data_name the name of the data set to describe. If not null, then
#' a column, data_name is added and the value is repeated. Otherwise,
#' a new column is not added. Default NULL.
#' @return A tibble with description information.
#' @importFrom tibble tibble
#' @export
describe_data <- function(x, data_name = NULL) {
  ret <- tibble(var_name = colnames(x))
  if(!is.null(data_name)) {
    ret <- cbind(tibble(data_name = rep(data_name, ncol(x))), ret)
  }
  ret$type <- unlist(lapply(x, typeof))
  all_attr <- c()
  for (j in seq_len(ncol(x))) {
    all_attr <- c(all_attr, list(attributes(x[[j]])))
  }
  all_attr_names <- unique(unlist(lapply(all_attr, names)))
  for (an in all_attr_names) {
    ret[[an]] <- attr_or_na(x, an)
  }
  ret
}

#' @title Consolidate Mulitple Data Set Descriptions
#'
#' @param ... a set of formatted data.frames.
#' @return A single data.frame composed of descriptions of all variables
#' for all data sets specified.
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows
#' @export
consolidated_describe_data <- function(...) {
  arg_list <- list(...)
  aln <- names(arg_list)
  if (is.list(arg_list) && length(arg_list) == 1 && is.null(aln)) {
    arg_list <- arg_list[[1]]
    aln <- names(arg_list)
  }
  ret <- tibble(data_name = character(), var_name = character(), 
                type = character(), levels = character(),
                class = character(), label = character(),
                format_sas = character())
  if (length(arg_list) > 0) {
    ret <- NULL
    data_source <- as.character(match.call())[-1]
    for (i in seq_along(arg_list)) {
      ret_temp <- describe_data(arg_list[[i]], aln[i])
      ret_temp$data_source <- data_source[i]
      ret <- bind_rows(ret, ret_temp)
    }
    names(ret)[which(names(ret) == "format.sas")] <- "format_sas"
  }
  as_tibble(ret)
}
