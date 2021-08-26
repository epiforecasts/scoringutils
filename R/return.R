#' @export
return_ <- function(x) {
  UseMethod("return_")
}

#' @export
return_.data.table <- function(x) {
 return(x[])
}

#' @export
return_.default <- return
