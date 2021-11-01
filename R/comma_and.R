#' @export
#' @title Add commas and 'and'
#' @description Given a character vector, add commas and 'and'.
#' @param x a character vector
comma_and <- function(x) {
  y <- length(x)
  if(y<2) return(x)
  if(y==2) return(paste0(x[1], ' and ', x[2]))
  else paste0(paste0(x[1:(y-1)], collapse=', '), ', and ', x[y])
}
