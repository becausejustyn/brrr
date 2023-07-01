#' age_groups
#'
#' @param data dataframe
#' @param column age column (in years)
#'
#' @return column with age categories
#' @export
#'
#' @examples
#' df <- data.frame(age = sample(18:65, size = 5, replace = TRUE))
#' df |> age_groups(age)
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
