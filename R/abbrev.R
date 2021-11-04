## Function to abbreviate things
#' @export
abbrev <- function(str, pat, rep) {
  stringr::str_replace_all(str, regex(pat, ignore_case=TRUE), rep) }

#' @export
abbreviate_year <- function(year) {
  year %>%
    as.integer() %>%
    round() %>%
    as.character() %>%
    dplyr::if_else(nchar(.)==4,
                   stringr::str_sub(., 3) %>%
                     paste0("'", .),
                   .)
}
