#' Binds two factors, here sentences
#'
#' @param a If a is defined as: (a <- factor(c("South", "Carolina","water", "use", "data")))
#' @param b If b is defined as: (b <- factor(c("and", "package", "building", "exercise")))
#'c (a,b)
#'factor(c(as.character(a), as.character(b)))

#' @return: If fbind(a,b) is entered and run then it will join or bind those two sentences
#' @export
#'
## @examples
# a <- c("On", "the")
# b <- c("way", "to", "Mars")
# fbind(a,b)
fbind <- function(a,b) {
  factor(c(as.character(a), as.character(b)))
}


