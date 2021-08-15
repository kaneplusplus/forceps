extract_var_names <- function(expr_text) {
  get_variable_names(parse(text=expr_text)[[1]])
}

extract_fun_names <- function(expr_text) {
  get_function_names(parse(text=expr_text)[[1]])
}

check_role <- function(x, role) {
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

#' Set Variable Roles to a data.frame Object
#' @param x a data.frame object.
#' @param roles a named list mapping the role to the variable in the data.frame.
#' @importFrom checkmate assert check_data_frame check_list check_character
#' @importFrom purrr map_lgl map_dfr map_chr
#' @export
set_roles <- function(x, roles) {
  UseMethod("add_roles", roles)
}

set_roles.default <- function(x, roles) {
  stop(
    paste(
      "Don't know how to set roles with object of type:\n\t", 
      paste(class(roles), sep = "\n\t"),
      sep = ""
    )
  )
}

get_type <- function(x, term) {
  ret <- class(x[[term]])
  if (is.null(ret)) {
    var_names <- extract_var_names(term)
    fun_names <- extract_fun_names(term)
    if (
      length(var_names) == 2 && 
      length(fun_names) == 1 && 
      fun_names == "Surv"
    ) {
      ret <- "survival"
    }
  ret
}

check_roles_df <- function(x, roles) {
  assert(
    check_data_frame(x),
    check_data_frame(roles)
    all(roles$term) %in% names(x)
  )

  cr <- roles[,"term"]
  cr$type <- map_chr(cr$term, ~ get_type(x, .x))
  assert(all(cr$type == roles$type))  
  invisible(TRUE)
}

assign_roles <- function(x, roles) {
  att <- attributes(x)
  att$forceps_roles <- roles
  attributes(x) <- att
  if (length(roles) > 0) {
    class(x) <- c("tbl_df_role", class(x))
  }
  x
}

set_roles.data.frame <- function(x, roles) {
  check_roles_df(x, roles)
  assign_roles(x, roles)
}

set_roles.list <- function(x, roles) {
  assert(
    check_data_frame(x),
    check_list(roles),
    !any(names(roles) == ""),
    length(unique(roles)) == length(roles),
    combine = "and"
  )

  check <- map_lgl(roles, ~ check_role(x, .x))
  
  forceps_roles <- map_dfr(seq_along(roles), 
    function(i) {
      ret <- tibble(term = roles[[i]], 
             role = rep(names(roles)[i], length(roles[[i]])),
      )
      ret$type <- map_chr(ret$term, ~ get_type(x[[.x]]) )
      ret$type[ret$type == "NULL"] <- "survival"
      ret
    }
  )
  assign_roles(x, forceps_roles)
}

#' Show Variable Roles
#' @param x a data.frame object whose variables have been assigned roles.
#' @export
roles <- function(x) {
  attributes(x)$forceps_roles
}

#' Filter by Role
#'
#' @param x a data.frame object whose variables have been assigned roles.
#' @export
role_filter <- function(x, roles) {
  terms <- intersect(colnames(x), roles(x)$term[roles(x)$role %in% roles])
  select(x, terms)
}
