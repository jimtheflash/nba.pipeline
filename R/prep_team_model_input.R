#' Construct initial input for team game models
#' @param team_gamelogs data.frame
#' @param team_game_metadata data.frame
#' @param lag_regex character
#' @param rolling_regex character
#' @return data.frame
#' @export
prep_team_model_input <- function(team_gamelogs, team_game_metadata,
                                  lag_regex = '_team_(rolling|allowed|diff)',
                                  rolling_regex = 'rolling_(sum|sd|mean)$') {
  browser()
  team_model_input <- team_gamelogs %>%

    #### this is an input, so get rid of anything that could be an outcome ####
    dplyr::select(-dplyr::matches('outcome')) %>%

    #### lag anything that is likely in-scope ####
    dplyr::arrange(team_id, game_date) %>%
    dplyr::group_by(season_year, season_type, team_id) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches(lag_regex),
        list(
          lagged = ~ dplyr::lag(.x)
        )
      )
    ) %>%
    dplyr::ungroup() %>%

    #### exclude columns that reference the game in question ####
    dplyr::select(
      -dplyr::ends_with('_team'),
      -dplyr::ends_with('_allowed'),
      -dplyr::ends_with('_diff')
    ) %>%
    dplyr::select(
      -dplyr::matches(rolling_regex)
    ) %>%

    #### merge in team-game metadata ####
    dplyr::inner_join(team_game_metadata,
                      by = c('team_id', 'game_id'))

}
