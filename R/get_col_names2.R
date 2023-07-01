#' get_col_names2
#'
#' @param df dataframe
#' @param start_col starting column
#' @param end_col last column
#'
#' @return column names between a range
#' @export
#'
#' @examples get_col_names2(mtcars, 'cyl', 'drat')
get_col_names2 <- function(df, start_col, end_col){
  if (base::is.character(start_col) || base::is.character(end_col)) {
    output <- df |> dplyr::select(dplyr::all_of(start_col):dplyr::all_of(end_col)) |> base::names()
  } else {
    output <- df |> dplyr::select({{start_col}}:{{end_col}}) |> base::names()
  }
  return(output)
  # get_col_names <- function(df, start_col, end_col){
  #output <- df |> dplyr::select({{start_col}}:{{end_col}}) |> names()
  #return(output)}
}
