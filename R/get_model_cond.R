#' get_model_cond
#'
#' @param data dataframe
#' @param column_name column name for the conditions
#'
#' @return dataframe with labels for the conditions, where condition == 'FD1' returns fair for cond1 and dark for cond2
#' @export
#'
#' @examples
#' df <- data.frame(val = c('FD1', 'FL1', 'FD1', 'B1'))
#' get_model_cond(df, val)
get_model_cond <- function(data, column_name) {
  column_name <- rlang::enquo(column_name)
  data <- data |>
    dplyr::mutate(
      cond1 = dplyr::case_when(
        {{column_name}} == 'FD1' ~ 'fair',
        {{column_name}} == 'FD2' ~ 'dark',
        {{column_name}} == 'FL1' ~ 'fair',
        {{column_name}} == 'FL2' ~ 'light',
        {{column_name}} == 'B1' ~ 'light',
        {{column_name}} == 'B2' ~ 'dark') |> base::as.factor(),
      cond2 = dplyr::case_when(
        {{column_name}} == 'FD1' ~ 'dark',
        {{column_name}} == 'FD2' ~ 'fair',
        {{column_name}} == 'FL1' ~ 'light',
        {{column_name}} == 'FL2' ~ 'fair',
        {{column_name}} == 'B1' ~ 'dark',
        {{column_name}} == 'B2' ~ 'light') |> base::as.factor()
    )
  return(data)
}
