#' apply_value_label
#'
#' @param df dataframe
#' @param variable column to convert
#' @param survey_labels list of labels for variable
#' @param label_column current labels
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' df |> apply_value_label(col1, label_lst)
#' }
apply_value_label <- function(df, variable, survey_labels, label_column = "value_label") {
  df <- df |>
    dplyr::mutate(
      !!label_column := base::factor(dplyr::case_match(
        as.character({{variable}}),
        '1' ~ survey_labels[1],
        '2' ~ survey_labels[2],
        '3' ~ survey_labels[3],
        '4' ~ survey_labels[4],
        '5' ~ survey_labels[5]),
        levels = survey_labels)
    )

  return(df)
}
