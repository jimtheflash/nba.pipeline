#' prep gamelogs for aggregatin'
#' @param path where are the gamelogs? they oughta be csv too else this won't work
#' @return data.frame
#' @export
prep_gamelogs <- function(path) {

  # get all the data
  gamelogs <-
    read.csv(path) %>%
    janitor::clean_names() %>%
    dplyr::select(-dplyr::ends_with('_rank'), -dplyr::ends_with('pct')) %>%
    dplyr::mutate(game_date = as.Date(game_date),
                  season_type = dplyr::if_else(season_type %in% c('Pre Season', 'Regular Season'), season_type, 'Post Season'))

  return(gamelogs)
}
