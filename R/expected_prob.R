#' expected_prob
#'
#' Given a condition in the experiment, find the number of times that condition was one of the trials.
#'
#' @param data dataframe
#' @param conditions experiment condition, can be multiple, e.g. c('fair', 'dark', 'light')
#'
#' @return rounded half of the number of instances. E.g. If a condition was in 501 trials, then 250 will be returned.
#' @export
#'
#' @examples
#' \dontrun{
#' expected_prob(df, conditions = c('dark', 'fair', 'light'))
#' }
expected_prob <- function(data, conditions) {
  # Use map() to iterate over each condition
  result <- purrr::map_dbl(conditions, function(cond) {
    # Get the instance count for the current condition
    instance_n <- data |>
      dplyr::filter(cond1 == cond | cond2 == cond) |>
      nrow()

    # Assuming a 50/50 chance, round down
    floor(instance_n / 2)
  })

  # Create a named vector with the results
  names(result) <- conditions

  return(result)
}
