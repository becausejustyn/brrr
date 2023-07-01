#' preferred_model
#'
#' @param data dataframe
#' @param value value for question 1
#' @param type condition
#'
#' @return dataframe of the preferred model from question 1
#' @export
#'
#' @examples
#' df <- data.frame(
#'   quest2_val = sample(1:100, 10),
#'   condition = sample(c('fair_dark', 'fair_light', 'biased'), 10, replace = TRUE)
#'   )
#' preferred_model(df, quest2_val, condition)
preferred_model <- function(data, value, type){
  data <- data |>
    dplyr::mutate(
      preferred_mod = dplyr::case_when(
        {{type}} == 'fair_dark' & {{value}} < 50 ~ 'fair',
        {{type}} == 'fair_dark' & {{value}} > 50 ~ 'dark',
        {{type}} == 'fair_light' & {{value}} > 50 ~ 'fair',
        {{type}} == 'fair_light' & {{value}} < 50 ~ 'light',
        {{type}} == 'biased' & {{value}} > 50 ~ 'light',
        {{type}} == 'biased' & {{value}} < 50 ~ 'dark',
        TRUE ~ NA)
    ) |> dplyr::filter(!is.na(preferred_mod))

  return(data)
}
