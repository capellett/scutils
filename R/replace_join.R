#' @param x is a table with at least 2 columns: at least one id_col, and at least one replace_col.
#' @param y is a table with at least 2 columns: at least one id_col, and at least one replace_col.
#' @desc The idea is that y has some data that should be replaced by the values in x,
#' and there is other data in y (other columns or rows) which doesn't exist in x.
#' The output will have the values from x when available, and the values from y in other cases.
#' If there are columns in x and y which are not id_cols or replace_cols,
#' then the columns from each table will be included with suffix .x and .y
#' @noRd
replace_join <- function(x, y, id_cols, replace_cols) {
  x1 <- dplyr::left_join(
    x, dplyr::select(y, -dplyr::all_of(replace_cols)), by=id_cols)
  y1 <- dplyr::anti_join(y, x, by=id_cols)
  z <- dplyr::bind_rows(x1, y1)
  return(z)
}

a <- tibble::tribble(
  ~big_letter, ~lil_letter, ~Number, ~Numeral, ~Color, ~Texture,
  "A", "a", 1, "i", "blue", "smooth",
  "B", "b", 2, "ii", "red", "rough",
  "C", "c", 3, "iii", "green", "slick",
  "E", "e", 5, "v", "yellow", "gritty")

b <- tibble::tribble(
  ~big_letter, ~lil_letter, ~Number, ~Numeral, ~Color,
  "A",  "a", 1, "i", "robins egg",
  "B",  "b", 2, "ii", "burgundy",
  "C",  "c", 4, "iii", "forest green",
  "D",  "d", NA_real_, "iv", "purple")

replace_join(a, b, id_cols=c("big_letter", "lil_letter"), replace_cols=c("Number", "Numeral"))
