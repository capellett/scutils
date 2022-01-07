#' @title Remove Missing Values
#' @description Remove missing values (NA) from a vector
#' @param x a vector.
#' @return A vector the same as x but with missing values removed.
#' @export
na.rm <- function(x) x[!is.na(x)]
