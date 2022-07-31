#' prep gamelogs for aggregatin'
#' @param path where are the gamelogs? they oughta be csv too else this won't work
#' @return data.frame
#' @export
prep_gamelogs <- function(path) {
  # get all the data
  gamelogs <-
    read.csv(path) %>%
    janitor::clean_names()
  return(gamelogs)
}
