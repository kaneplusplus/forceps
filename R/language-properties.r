
# language_properties(parse(text="tail(y, n=3)")[[1]])
#' @importFrom codetools makeCodeWalker walkCode
language_properties <- function(expr){
  functions <- NULL
  variables <- NULL
  props <- environment()

  cw <- makeCodeWalker (
    call = function(e, w) {
      if( is.null(e) || length(e) == 0 || as.character(e[[1]]) == "::") return()
      # add the current function to the list
      props[["functions"]] <-
          c( props[["functions"]], as.character(e[[1]]) )

        # process the list of expressions
      w$call.list( e[-1] , w )

    },
    leaf = function(e, w ){
      # deal with argument list of functions
      if( typeof(e) == "pairlist" ){
        stop("Don't know what to do.")
        w$call.list( e, w )
      } else if (typeof(e) == "symbol") {
        props[["variables"]] <-
          c(props[["variables"]], as.character(e))
      }
    },
    call.list = function( e, w ){
      for( a in as.list(e) ){
        if(!missing(a) ){
          walkCode(a, w)
        }
      }
    },
    env = props # so that we can populate "functions"
  )

  # Get the properties of the language object.
  walkCode(expr,  w=cw)

  list(functions=unique(props[["functions"]]),
       variables=unique(props[["variables"]]))
}

#' @export
get_variable_names <- function(expr) {
  language_properties(expr)$variables
}

#' @export
get_function_names <- function(expr) {
  language_properties(expr)$functions
}

