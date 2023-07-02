#' cond_label
#'
#' @param data dataframe
#' @param column_name condition label, e.g. 'FD1', 'FL1' or 'B1'
#'
#' @return dataframe with column named `type` where `FD1` becomes `fair_dark`, `FL1` becomes `fair_light`, and `B1` becomes `biased`.
#' @export
#'
#' @examples
#' \dontrun{
#' df |> cond_label(column_name = condition)
#' }
cond_label <- function(data, column_name){
  column_name <- rlang::enquo(column_name)
  data <- data |>
    dplyr::mutate(
      type = dplyr::case_when(
        {{column_name}} == 'FD1' ~ 'fair_dark',
        {{column_name}} == 'FL1' ~ 'fair_light',
        {{column_name}} == 'B1' ~ 'biased') |> as.factor()
    )
  return(data)
}
