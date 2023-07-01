#' reverse_conditions
#'
#' @param data dataframe
#' @param cond_col column of the condition
#' @param q1_val question 1 value
#' @param q2_val question 2 value
#'
#' @return dataframe with the counterbalanced conditions reversed
#' @export
#'
#' @examples
#' \dontrun{
#' reverse_conditions(df, condition_column, question1_value, question2_value)
#' }
reverse_conditions <- function(data, cond_col, q1_val, q2_val){
  # reverses the values for the counterbalanced conditions
  # adds a label if the row was reversed to keep track
  # e.g. reverse_conditions(exp_df_long1, condition, quest1_value, quest2_value)
  data |>
    dplyr::mutate(
      condition = base::as.factor({{cond_col}}),
      # reverse the value for the second condition, e.g. B2 so then you can condense it to 3 groups
      quest1_value = base::ifelse({{cond_col}} %in% c('B2', 'FD2', 'FL2'), base::abs({{q1_val}} - 100), {{q1_val}}),
      quest2_value = base::ifelse({{cond_col}} %in% c('B2', 'FD2', 'FL2'), base::abs({{q2_val}} - 100), {{q2_val}}),
      # now update the conditions to match this
      conditions_new = stringr::str_replace({{cond_col}}, '2$', '1') |> base::as.factor(),
      # dummy var for variables you reversed
      reversed_condition = base::ifelse({{cond_col}} %in% c('B2', 'FD2', 'FL2'), 1, 0) |> base::as.factor()
    ) |>
    dplyr::select(-c(which_question, question_version2))
}
