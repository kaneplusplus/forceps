extract_var_names <- function(expr_text) {
  get_variable_names(parse(text=expr_text)[[1]])
}

check_role <- function(role, x) {
  var_names <- extract_var_names(role)
  vnc <- var_names %in% names(x) 
  if (any(!vnc)) {
    em <- paste("The following variables are not in the data set:\n\t",
      paste(var_names[!vnc], sep = "\n\t"))
    stop(em)
    FALSE
  } else {
    TRUE
  }
}

#' Add Variable Roles to a data.frame Object
#' @param x a data.frame object.
#' @param roles a named list mapping the role to the variable in the data.frame.
#' @importFrom checkmate assert check_data_frame check_list
#' @importFrom purrr map_lgl map_dfr
#' @export
add_roles <- function(x, roles) {
  assert(
    check_data_frame(x),
    check_list(roles),
    !any(names(roles) == ""),
    length(unique(roles)) == length(roles),
    combine = "and"
  )

  check <- map_lgl(roles, check_role, x)
  
  forceps_roles <- map_dfr(seq_along(roles), 
    function(i) {
      tibble(term = roles[[i]], 
             role = rep(names(roles)[i], length(roles[[i]])))
    }
  )
  att <- attributes(x)
  att$forceps_roles <- forceps_roles
  attributes(x) <- att
  if (length(roles) > 0) {
    class(x) <- c("tbl_df_role", class(x))
  }
  x
}

#' Show Variable Roles
#' @param x a data.frame object whose variables have been assigned roles.
#' @export
roles <- function(x) {
  attributes(x)$forceps_roles
}
