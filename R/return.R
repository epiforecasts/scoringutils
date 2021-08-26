return_ <- function(x) {
  UseMethod("return_")
}

return_.data.table <- function(x) {
 return(x[])
}

return_.default <- return
