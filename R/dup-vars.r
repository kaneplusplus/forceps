#' Find Variables Appearing Multiple Data Sets
#'
#' @param x the list of data.frames.
#' @param on the collapse variable.
#' @param x_names the names of the data sets. (Default names(x))
#' @importFrom dplyr full_join distinct
#' @importFrom crayon red
#' @importFrom equivalent equiv
#' @export
duplicated_vars <- function(x, on, x_names = names(x)) {
  if (is.null(x_names)) {
    stop(red("The supplied list must have names."))
  }
  dup_violations <- list()
  if (length(x) > 1) {
    dup_names <- setdiff(dup_vars(x), on)
    for (dn in dup_names) {
      dup_inds <- which(unlist(lapply(x, function(d) dn %in% colnames(d))))
      dup_ret <- as_tibble(x[[dup_inds[1]]][,c(on, dn)])
      colnames(dup_ret)[2] <- x_names[dup_inds[1]]
      dup_ret <- dup_ret[!duplicated(dup_ret[[on]]),]
      for (di in dup_inds[-1]) {
        nd <- x[[di]][, c(on, dn)]
        colnames(nd)[2] <- x_names[di]
        dup_ret <- full_join(dup_ret, nd[!duplicated(nd[[on]]),],
                             by = on)
      }
      dup_ret$var <- dn
      dup_ret <- dup_ret[, c(1, ncol(dup_ret),
                             setdiff(seq_len(ncol(dup_ret)),
                                     c(1, ncol(dup_ret))))]
      dup_violations <- c(dup_violations, list(dup_ret))
    }
    names(dup_violations) <- dup_names
  }
  dup_violations
}


#' Variables Duplicated Across Data Sets
#'
#' @param x the list of data.frames.
#' @export
dup_vars <- function(x) {
  all_names <- unlist(lapply(x, colnames))
  unique(all_names[duplicated(all_names)])
}
