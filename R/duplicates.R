#' @title Determine all duplicated elements.
#' return TRUE for all duplicated entries, including the first one of each set.
#' @description 'duplicated2' returns a logical vector: TRUE for all the
#' duplicated elements, and FALSE for unique elements. Whereas duplicated(), a base function, does not
#' include the first entry of the duplicated set.
#' @param x a vector or a data frame or an array or NULL
#' @return A logical vector (TRUE for duplicates and FALSE for non duplicates)
#' @export
duplicated2 <- function (x) {
  duplicated(x) | duplicated(x, fromLast=TRUE) }


#' @title Return rows with duplicate values
#' @description Return all rows with duplicated values in the ID columns.
#' @param x a dataframe
#' @param idCols character vector of column names to check for duplicates.
#' @return a dataframe containing only rows with duplicated values in the ID columns.
#' @export
dupes <- function(x, idCols) {
  x[duplicated2(x[,idCols]),] }


#' @title Remove incomplete duplicate rows.
#' @description This function takes a table, an id column, and two data columns.
#   It removes duplicates that have an NA in one of the data columns
#  when the other duplicate has data for that column.
#    Note that this could be bad if there are two rows with the same id,
#   if each row has data that the other is missing...
#' @param data A data frame
#' @param id The id column, which should be unique but isn't.
#' @param x A column of interest, perhaps with some values missing (NA).
#' @param y Another column of interest, perhaps with some values missing (NA).
removeIncompleteDupes <- function(data, id, x, y) {
   data[
    !(
      (duplicated2(data[,c(id,x)]) &
         !(duplicated2(data[,c(id,y)])) &
         is.na(data[,y]) )
      |
       (duplicated2(data[,c(id,y)]) &
           !(duplicated2(data[,c(id,x)])) &
          is.na(data[,x]) )
    ),] }

# on testing this function for chk_df (created an example data frame),
# it did not remove the duplicated rows



# replace_Na <- function(dat, id_col, col_NA){
#   dat2 <- dat %>%
#     dplyr::arrange(dat,id_col) %>%
#     tidyr::fill(col_NA, .direction = "up")
#   return(dat2)
# }
#
#
# replace_Na2 <- function(dat, id_col, col_NA){
#   dat %>%
#     dplyr::group_by(id_col) %>%
#     tidyr::fill(col_NA, .direction = "downup") %>%
#     dplyr::ungroup()
# }

