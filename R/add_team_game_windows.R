#' Add window functions to team-game summary data
#' @note Most of the windows are going to be within season-types, i.e. pre-season, regular, and post-season. For now.
#' @param team_gamelogs data.frame of summarised team-game data
#' @param to_roll_regex character regex for columns to be rolled up in window functions
#' @return data.frame
#' @export
add_team_game_windows <- function(team_gamelogs,
                                  to_roll_regex = '_team|_allowed|_diff') {

  plus_windows <- team_gamelogs %>%

    #### add window functions for season by season_type ####
    dplyr::group_by(season_year, season_type, team_id) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches(to_roll_regex),
        .fns = list(
          rolling_sum = ~ cumsum(as.numeric(.x)),
          rolling_mean = ~ dplyr::cummean(.x),
          rolling_sd = ~ cumvar(.x, sd=TRUE)
        ),
        .names = '{.col}_window_{.fn}'
      )
    ) %>%
    dplyr::ungroup()

  return(plus_windows)

}
