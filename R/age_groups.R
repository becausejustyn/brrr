#' age_groups
#'
#' @param data dataframe
#' @param column age column (in years)
#'
#' @return column with age categories
#' @export
#'
#' @examples mtcars |> age_groups(mpg)
age_groups <- function(data, column){
  data <- data |>
    dplyr::mutate(
      age_x = dplyr::case_when(
        {{column}} <= 30 ~ '18 - 30',
        dplyr::between({{column}}, 30, 45) ~ '30 - 45',
        dplyr::between({{column}}, 45, 65) ~ '45 - 65',
        dplyr::between({{column}}, 65, 99) ~ '65+')
    )
  return(data)
}
