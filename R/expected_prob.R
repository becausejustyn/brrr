#' expected_prob
#'
#' Given a condition in the experiment, find the number of times that condition was one of the trials.
#'
#' @param data dataframe
#' @param condition experiment condition
#'
#' @return rounded half of the number of instances. E.g. If a condition was in 501 trials, then 250 will be returned.
#' @export
#'
#' @examples
#' \dontrun{
#' expected_prob(df, 'FD1')
#' }
expected_prob <- function(data, condition){
  # get the instance count for each model
  instance_n <- data |>
    filter(cond1 == {{condition}} | cond2 == {{condition}}) |>
    nrow()

  # assuming a 50/50 chance, round down
  return(floor(instance_n / 2))
}
