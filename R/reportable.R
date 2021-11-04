#' @title reporTable
#' @description an HTML widget for creating an interactive table.
#' @export
reporTable <- function(x, ...) {
  DT::datatable(
    data=x,
    extensions = c('Buttons', 'ColReorder' #, 'Scroller'
    ),
    filter = list(position = 'top', clear = FALSE),
    rownames = FALSE,
    # options = list(autoWidth = TRUE)
    options = list(
      order = list(1, 'asc'),
      buttons = c('copy', 'csv', 'excel'), #  'colvis',
      autoWidth = FALSE,
      pageLength=min(nrow(x), 10),
      search = list(regex = TRUE),
      autoHideNavigation = TRUE,
      colReorder = TRUE,
      scroller = TRUE,
      scrollY = 600,
      scrollX = TRUE,
      dom = 'Blftip') # dom = 'tBlip',
  ) }

reporTable2 <- function(x, ...) {
  datatable(
    data=x,
    extensions = c('Buttons'),
    rownames = FALSE,
    width = "100%",
    height = "100%",
    options = list(
      colReorder = FALSE,
      search = FALSE,
      buttons = c('copy', 'csv', 'excel'),
      # order = list(list(2, 'desc'), list(1, 'asc')),
      dom = 'tBlip',
      # scrollX = FALSE,
      scrollY = FALSE,
      autoWidth = TRUE,
      columnDefs = list(
        list(width='35.7%', targets = 1,
             width='14.28%', targets = 2,
             width='3.57%', targets = c(3:14),
             width='7.14%', targets = 15) ) ) ) }

#' @export
pdf_table <- function(df, title="", ...) {
  df %>%
    knitr::kable(caption=title, booktabs=T, linesep = "", ...) %>%
    kableExtra::kable_styling(
      ., latex_options=c('striped', 'hold_position')) }

#' @export
report_source_if <- function(reports, source='', title=source, format='pdf') {
  reports <- reports %>%
    dplyr::filter(Source==source) %>%
    dplyr::select(-Source)
  if(nrow(reports)>0) {
    reports %>%
      pdf_table(title=title) %>%
      return() } }
