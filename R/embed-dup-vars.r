
#' @title Embed Duplicated Variables
#'
#' @param data_list the list of similarly-cohorted data sets.
#' @param on the variable to consolidate on.
#' @importFrom dplyr bind_cols select left_join
#' @importFrom tibble tibble
#' @export
embed_dup_vars <- function(data_list, on) {
  dvs <- duplicated_vars(data_list, on = on)

  for (dv in dvs) {
    for (j in seq_along(dv)[-(1:2)]) {
      jd <- bind_cols(dv[,1], tibble(dv[,j]))
      names(jd)[2] <- dv$var[1]

      ds_name <- names(dv)[j]

      jdj <- left_join(select(data_list[[ds_name]], !!on), jd, by = on)

      ds_new <- data_list[[ds_name]]
      ds_new <- select(ds_new, -(!!names(jd)[2]))

      if (is.null(ds_new[[ds_name]])) {
        ds_new[[ds_name]] <- 
          map(seq_len(nrow(jdj)), ~ tibble(!!names(jdj)[2] := jdj[[2]][[.x]]))
      } else {
        for (i in seq_along(nrow(ds_new))) {
          tib <- ds_new[[ds_name]][[i]]
          tib[[names(jdj)[2]]] <- rep(jdj[[2]][i], nrow(tib))
          ds_new[[ds_name]][[i]] <- tib
        }
      }
      data_list[[ds_name]] <- ds_new
    }
  }
  data_list
}
