#' get_id_vals
#'
#' @param data dataframe
#' @param id id vector to search render_id with
#'
#' @return dataframe of the render id
#' @export
#'
#' @examples
#' id_vals <- c(34, 95, 11)
#' df <- data.frame(render_id = c(34, 2, 35, 10, 11, 5))
#' get_id_vals(df, id_vals)
get_id_vals <- function(data, id){
  data |>
    dplyr::filter(render_id %in% c(id))
}
