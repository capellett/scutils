
# function not exported
#' @title Not in
#' @description Binary operator which returns a logical vector indicating
#' if there is no match between contents of x and y.
#
#' @param x,y vectors or NULL.
#'  Long vectors not supported for y.
#' @return A logical vector of the same length as x.
#' @export
#'
  '%nin%' <- function(x,y) is.na(match(x,y))
