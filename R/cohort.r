#' Cohort a dataset.
#'
#' @param x a data.frame.
#' @param on which variable should be collapsed?
#' @param name the variable name of the resulting embedded data.frames.
#' @param remove_equiv_columns should equivalent columns be removed?
#' @param keep_cols column names that should be kept.
#' @param verbose should information about dropped columns be printed?
#' (default FALSE)
#' @return The collapsed data.frame with numerically encoded columens removed.
#' @importFrom dplyr %>% mutate_if mutate_at
#' @importFrom crayon green
#' @export
cohort <- function(x, on, name = "data",
  remove_equiv_columns = FALSE, keep_cols = character(), verbose = FALSE) {

  . <- NULL

  if (remove_equiv_columns) {
    x %>%
      tcat("Removing equivalent columns.\n", verbose = verbose,
           style = green) %>%
      remove_equiv_columns(verbose = verbose, keep_cols = keep_cols)
  }

  to_factor <- function(x) {
    col_types <- sapply(x, class)
    colnames(x)[ !(colnames(x) %in% on) & col_types == "character"]
  }
  if (verbose) {
    cat(green("Mutating character columns to factors.\n"))
    ma <- to_factor(x)
    if (length(ma) == 0) {
      cat(italic("No variables to turn into factor.\n"))
    } else {
      cat(
        italic("\tThe following variables will be turned into factors:\n\t\t"))
      cat(italic(paste(ma, collapse = "\n\t\t")))
      cat("\n")
    }
  }
  x <- mutate_at(x, to_factor(x), as.factor)
  x %>%
    mutate_at(to_factor(.), as.factor) %>%
    tcat("Collapsing rows.\n", verbose = verbose, style = green) %>%
    collapse_rows(on, name)
}


collapsible_vars <- function(x, group_var) {
  s <- NULL
  spl <- split(seq_len(nrow(x)), x[,group_var])
  if (length(spl) == nrow(x)) {
    character()
  } else {
    check_vars <- setdiff(colnames(x), group_var)
    check_vals <- Reduce(`&`,
      Map(function(v) {
            vapply(x[v, check_vars],
              function(x) {
                isTRUE(all(x == x[1])) | all(is.na(x))
              }, NA) 
          }, spl))
    check_vars[check_vals]
  }
}

# Collapse the rows of a data.frame object
#
# @param x a data.frame.
# @param key which variable should be collpased on?
# @param collapse_name the variable name of the collapsed sub-data.frames.
#' @importFrom tidyr nest
#' @importFrom dplyr sym group_by 
#' @importFrom rlang :=
collapse_rows <- function(x, key, collapse_name = "data") {
  g <- attributes(group_by(x, !!sym(key)))$groups
  if (nrow(g) == nrow(x)) {
    x
  } else {
    sv <- c(key, collapsible_vars(x, key))
    nsv <- setdiff(colnames(x), sv)
    nest(x, !!collapse_name := nsv)
  }
}


# Print a message
#
# @param x the data the function will return
# @param msg the message to cat.
# @param verbose should the message be cat'ed? Default TRUE.
# @param style the crayon style to use when printing. Default reset.
# @param ... other paramters passed to cat.
# @importFrom crayon reset
tcat <- function(x, msg, verbose = TRUE, style = crayon::reset, ...) {
  if (verbose) {
    cat(style(msg), ...)
  }
  x
}


