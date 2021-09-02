#' @export
extract_var_names <- function(expr_text) {
  get_variable_names(parse(text=expr_text)[[1]])
}

#' @export
extract_fun_names <- function(expr_text) {
  get_function_names(parse(text=expr_text)[[1]])
}

# Create extract_fun_names function

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

# Make add_roles a method, dispatch on roles (list or data.frame.
# create assign_roles function and check_roles function.

#' Add Variable Roles to a data.frame Object
#' @param x a data.frame object.
#' @param roles a named list mapping the role to the variable in the data.frame.
#' @param ... unused.
#' @importFrom checkmate assert check_data_frame check_list check_character
#' @importFrom purrr map_lgl map_dfr map_chr
#' @export
add_roles <- function(x, roles, ...) {
  UseMethod("add_roles", roles)
}

set_roles <- function(x, roles) {
  att <- attributes(x)
  att$forceps_roles <- roles
  attributes(x) <- att
  if (length(roles) > 0) {
    class(x) <- c("tbl_df_role", class(x))
  }
  x
}

#' @export
add_roles.default <- function(x, roles, ...) {
  stop(
    "Don't know how to set roles when roles is of type:\n\t",
    paste(class(roles), sep = "\n\t"),
    sep = ""
  )
}

#' @export
add_roles.list <- function(x, roles, ...) {
  if (!check_data_frame(x)) {
    stop("The first argument to `add_roles()` should be a data.frame")
  }
  if (any(names(roles) == "")) {
    stop("All roles must have a name")
  }
  if (length(unique(names(roles))) != length(names(roles))) {
    stop("Role names may not be repeated.")
  }

  check <- map_lgl(roles, check_role, x)
  
  forceps_roles <- map_dfr(seq_along(roles), 
    function(i) {
      ret <- tibble(term = roles[[i]], 
             role = rep(names(roles)[i], length(roles[[i]])),
      )
      ret$type <- map_chr(ret$term, ~ class(x[[.x]])[1] )
      ret$type[ret$type == "NULL"] <- "survival"
      ret
    }
  )
  set_roles(x, forceps_roles)
}

#' @importFrom purrr map
#' @export
add_roles.data.frame <- function(x, roles, ...) {
  assert(
    check_data_frame(x),
    check_data_frame(roles),
    all(map_lgl(roles, ~ inherits(.x, "character"))),
    all(sort(names(roles)) == sort(c("term", "role", "type"))),
    all( 
      unlist(map(roles$term, ~ get_variable_names(parse(text = .x)[[1]]))) %in%
      names(x)
    ),
    all(
      roles$type %in% 
      c("factor", "numeric", "character", "survival", "integer")
    ),
    combine = "and"
  )
  set_roles(x, roles)
}

#' Show Variable Roles
#' @param x a data.frame object whose variables have been assigned roles.
#' @export
roles <- function(x) {
  attributes(x)$forceps_roles
}

#' Select by Role
#'
#' @param x a data.frame object whose variables have been assigned roles.
#' @param roles the roles to keep.
#' @param other variables in `x` to keep.
#' @importFrom dplyr all_of
#' @export
select_role <- function(x, roles, keep_var = NULL) {
  terms <- intersect(
    colnames(x), 
    c(
      roles(x)$term[roles(x)$role %in% roles], 
      keep_var
    ))
  select(x, all_of(terms))
}

