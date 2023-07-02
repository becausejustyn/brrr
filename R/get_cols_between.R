#' get_cols_between
#'
#' @param df dataframe
#' @param col_start column to start from
#' @param col_end last column you want
#'
#' @return the names of the columns between the range
#' @export
#'
#' @examples
#' \dontrun{
#' get_cols_between(df = mtcars, col_start = mpg, col_end = hp)
#' }
get_cols_between <- function(df, col_start, col_end) {
  col_idx <- match(col_start, names(df)):match(col_end, names(df))
  df |>
    # select(({{col_start}}:{{col_end}})) also works
    dplyr::select(dplyr::all_of(col_idx)) %>%
    names()
}
