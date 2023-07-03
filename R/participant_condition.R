#' participant_condition
#'
#' @param data dataframe
#' @param id_col column of uuid
#' @param id_lst nested list with a list for 'fl' and 'fd'
#'
#' @return dataframe with a new column named `exp_cond` which specifies if the participant was in either the fair_light or fair_dark condition.
#' @export
#'
#' @examples
#' \dontrun{
#' participant_condition(df, uuid, uuid_condition_lst)
#' }
participant_condition <- function(data, id_col, id_lst){
  data <- data |>
    dplyr::mutate(
      exp_cond = dplyr::case_when(
        {{id_col}} %in% id_lst[['fl']] ~ 'fair_light',
        {{id_col}} %in% id_lst[['fd']] ~ 'fair_dark',
        TRUE ~ NA_character_)
      )
  return(data)
}
