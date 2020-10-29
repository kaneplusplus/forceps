
#' @title Remove redundant equivalent columns
#'
#' @description Find the equivalant columns of a data.frame. Keep the first
#' remove the rest.
#' @param x a data.frame that may have repeated, equivalent columns.
#' @param keep_cols a character vector of columsn that should be kept.
#' @param verbose should information about dropped columns be printed?
#' (default FALSE)
#' @examples
#'
#' iris$Sepal.Length2 <- 3 * iris$Sepal.Length + 3
#' remove_equiv_columns(iris)
#'
#' @return a data frame where redundant columns have been dropeed.
#' @importFrom crayon italic
#' @importFrom equivalent has_equiv_column
#' @export
remove_equiv_columns <- function(x, keep_cols = character(), verbose = FALSE) {
  ec <- has_equiv_column(x, keep_cols, verbose)
  if (verbose) {
    if (any(ec)) {
      equiv_col <- c()
      for (j in seq_len(ncol(x)-1)) {
        equiv_col <- vapply((j+1):ncol(x),
               function(cn) {
                 if (!(names(x)[cn] %in% keep_cols)) {
                   equiv(x[[j]], x[[cn]])
                 } else {
                   FALSE
                 }
               },
               NA)
        if (isTRUE(any(equiv_col))) {
          cat(italic("\tThe following column(s) equivalent to ", 
                     names(x)[j], "\n\tand are getting dropped:\n\t\t",
                     paste(names(x[,-seq_len(j), drop = FALSE])[equiv_col],
                     sep = "", collapse = "\n\t\t"),
                     "\n", sep = ""), sep = "")
        }
      }
    } else {
      cat(italic("No equivalent columns found.\n"))
    }
  }
  x[, !ec]
}

