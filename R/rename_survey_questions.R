#' rename_survey_questions
#'
#' @param df dataframe
#'
#' @return dataframe with the survey questions renamed
#' @export
#'
#' @examples
#' \dontrun{
#' rename_survey_questions(df)
#' }
rename_survey_questions <- function(df) {
  df_renamed <- df |>
    dplyr::rename(
      user_help_understanding = quest1_1,
      user_conf_helpful = quest2_1,
      user_conf_acc = quest2_2,

      user_view_general = quest3_1,
      user_view_health = quest3_2,
      user_view_face = quest3_3,

      supp_dev_general = quest4_1,
      supp_dev_health = quest4_2,
      supp_dev_face = quest4_3,

      accept_ai_general = quest5_1,
      accept_ai_health = quest5_2,
      accept_ai_face = quest5_3,

      trust_ai_general = quest6_1,
      trust_ai_health = quest6_2,
      trust_ai_face = quest6_3,

      share_info_general = quest7_1,
      share_info_health = quest7_2,
      share_info_face = quest7_3,

      rely_info_general = quest8_1,
      rely_info_health = quest8_2,
      rely_info_face = quest8_3
    )
  return(df_renamed)
}
