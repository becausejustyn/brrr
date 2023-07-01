#' rename_columns
#'
#' @param df dataframe
#'
#' @return dataframe with renamed columns
#' @export
#'
#' @examples
#' \dontrun{
#' rename_columns(df)
#' }
rename_columns <- function(df) {
  df_renamed <- df |>
    dplyr::rename(
      duration = `Duration (in seconds)`,
      consent = Q1...18,
      prolific_id = Q1...19,
      age = Q2...20,
      country = Q3,
      ethnicity = Q4,
      example1 = Q2_1...23,
      example2 = Q2_2...24,
      quest1_1 = Q2...217,
      quest2_1 = Q3_1...218,
      quest2_2 = Q3_2...219,
      quest3_1 = Q4_1...220,
      quest3_2 = Q4_2...221,
      quest3_3 = Q4_3,
      quest4_1 = Q5_1...223,
      quest4_2 = Q5_2...224,
      quest4_3 = Q5_3,
      quest5_1 = Q6_1...226,
      quest5_2 = Q6_2...227,
      quest5_3 = Q6_3,
      quest6_1 = Q7_1...229,
      quest6_2 = Q7_2...230,
      quest6_3 = Q7_3,
      quest7_1 = Q8_1...232,
      quest7_2 = Q8_2...233,
      quest7_3 = Q8_3,
      quest8_1 = Q9_1...235,
      quest8_2 = Q9_2...236,
      quest8_3 = Q9_3,
      quest9_1 = Q10,
      quest10_1 = Q11
    ) |>
    dplyr::select(-c(IPAddress, Progress, RecipientLastName:DistributionChannel))

  return(df_renamed)
}
