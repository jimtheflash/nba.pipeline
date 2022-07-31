#' Add window functions to team-game summary data
#' @note Most of the windows are going to be within season-types, i.e. pre-season, regular, and post-season. For now.
#' @param team_gamelogs data.frame of summarised team-game data
#' @return data.frame
#' @export
add_team_game_windows <- function(team_gamelogs) {

  plus_windows <- team_gamelogs %>%

    #### add window functions for season irrespective of season_type ####
    dplyr::arrange(season_year, team_id, game_date) %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(season_game_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%

    #### add window functions for season by season_type ####
    dplyr::group_by(season_year, season_type, team_id) %>%
    dplyr::mutate(
      season_type_game_number = dplyr::row_number()
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches('_team|_allowed|_diff'),
        .fns = list(
          # rolling_sum = cumsum,
          rolling_mean = ~ dplyr::cummean(.x)
        ),
        .names = '{.col}_window_{.fn}'
      )
    )

  return(plus_windows)

}
