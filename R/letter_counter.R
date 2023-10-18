lettr_countr <- function(n) {
  if(n > 26) {lettr_countr(as.integer(n%/%26))}
  cat(LETTERS[n %% 26])
}

#' @export
letter_counter <- function(n) {
  if(n<1) return(NULL)
  lettr_countr(n)
}

### I can't get it to work with length > 1

# lettr_countr2 <- function(n) {
#   dplyr::if_else(
#     n > 26, lettr_countr2(as.integer(n%/%26)))
#   cat(LETTERS[n %% 26])
# }
#
#
# letter_counter2 <- function(n) {
#   dplyr::if_else(
#     n<1, NA_character_, lettr_countr2(n))
#   }
#
# letter_counter2(c(1,2,3,27))
