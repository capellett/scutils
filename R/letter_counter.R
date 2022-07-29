lettr_countr <- function(n) {
  if(n > 26) {lettr_countr(as.integer(n%/%26))}
  cat(LETTERS[n %% 26])
}

#' @export
letter_counter <- function(n) {
  if(n<1) return(NULL)
  lettr_countr(n)
}
