## Old table and new table. Same columns.
## What rows are in one table but not the other?
# dplyr::anti_join(x, y, id_cols); dplyr::anti_join(y, x, id_cols)

## what rows are in both tables and have values in one table but not the other?
## in the old and not the new | in the new and not the old
#' @export
tbl_diff_missing <- function(x, y, id_cols, val_cols) {
  ## get only the matched rows
  x <- dplyr::semi_join(x, y, by=id_cols) %>%
    dplyr::select(id_cols, val_cols) %>%
    tidyr::pivot_longer(cols=val_cols, names_to="col_name",
                        values_to="value_x")

  y <- dplyr::semi_join(y, x, by=id_cols) %>%
    dplyr::select(id_cols, val_cols) %>%
    tidyr::pivot_longer(cols=val_cols, names_to="col_name",
                        values_to="value_y")

  z <- dplyr::left_join(x, y, by=c(id_cols, "col_name")) %>%
    dplyr::filter(is.na(value_x) & !is.na(value_y))
  ## ~ID, ~Col_Name, ~old_value, ~new_value
  z
}

## what rows have different values in the two tables?
#' @export
tbl_diff_values <- function(x, y, id_cols, val_cols) {
  ## inner join the two tables by id_cols
  ## then outer join the tbl_diff_missing results(?)
  ## no, a row could have some missing and some different values.
  x <- dplyr::semi_join(x, y, by=id_cols) %>%
    dplyr::select(id_cols, val_cols) %>%
    tidyr::pivot_longer(cols=val_cols, names_to="col_name",
                        values_to="value_x")

  y <- dplyr::semi_join(y, x, by=id_cols) %>%
    dplyr::select(id_cols, val_cols) %>%
    tidyr::pivot_longer(cols=val_cols, names_to="col_name",
                        values_to="value_y")

  z <- dplyr::left_join(x, y, by=c(id_cols, "col_name")) %>%
    dplyr::filter(value_x != value_y)
  ## ~ID, ~Col_Name, ~old_value, ~new_value
  z
}
