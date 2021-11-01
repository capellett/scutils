#' @title Expect in
#' @description testthat, this function is a modified version of
#' testthat::expect_setequal. This function will check if actual set of the output
#' is included or is present in the expected set.
#' @param object actual output set after running the function
#' @param expected expected output set that will include the actual output set.
#' but may or may not entirely match the length and order of actual output set.
#' @importFrom rlang enquo
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @return object actual output set
#' @return expected expected output set
#' @export
expect_in <- function(object, expected) {

    act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    exp <- testthat::quasi_label(rlang::enquo(expected), arg = "expected")
    if (!is_vector(act$val) || !is_vector(exp$val)) {
      rlang::abort("`object` and `expected` must both be vectors")
    }
    if (!is.null(names(act$val)) && !is.null(names(exp$val))) {
      rlang::warn("expect_in() ignores names")
    }
    act$miss <- !act$val %in% exp$val
    if (any(act$miss)) {
      testthat::fail(paste0(act$lab, "[", locations(act$miss),
                  "] absent from ", exp$lab))
    }
    if (!any(act$miss)) {
      testthat::succeed()
    }
    invisible(act$val)
}

#' @title Check for vector
#' @description used within the expect_in function to check if input is a list
#' or atomic
#' @param x list or atomic
#' @export
is_vector <- function(x) {
  is.list(x) || (is.atomic(x) && !is.null(x))
}

#' @title Detect location
#' @description used within the expect_in function to check which element in actual
#' set is missing from expected set
#' @param i element from actual set
#' @export
locations <- function(i) {
  loc <- which(i)
  if (length(loc) == 1) {
    return(loc)
  }

  if (length(loc) > 10) {
    loc <- c(loc[1:9], "...")
  }

  paste0("c(", paste0(loc, collapse = ", "), ")")
}

