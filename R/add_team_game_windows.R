#' Add window functions to team-game summary data
#' @note Most of the windows are going to be within season-types, i.e. pre-season, regular, and post-season. For now.
#' @param team_gamelogs data.frame of summarised team-game data
#' @param team_game_metadata data.frame of more summarised team-game data
#' @param to_roll_regex character regex for columns to be rolled up in window functions
#' @return data.frame
#' @export
add_team_game_windows <- function(team_gamelogs, team_game_metadata,
                                  to_roll_regex = '_team|_allowed|_diff|ot_num|wl_num') {

  #### set up the window function list
  fun_list <- list(
    rolling_sum = ~ cumsum(as.numeric(.x)),
    rolling_mean = ~ dplyr::cummean(.x),
    rolling_sd = ~ cumvar(.x, sd=TRUE)
    )

  plus_windows <- team_gamelogs %>%

    #### MERGE ####
    dplyr::inner_join(team_game_metadata,
                      by = c('game_id', 'season_year', 'season_type', 'team_id', 'game_date', 'wl')) %>%

    #### add window functions for season by season_type ####
    dplyr::group_by(season_year, season_type, team_id) %>%
    dplyr::mutate(
      win_num = as.numeric(wl == 'W'),
      ot_num = as.numeric(ot == TRUE),
      dplyr::across(
        .cols = dplyr::matches(to_roll_regex),
        .fns = fun_list,
        .names = '{.col}_{.fn}'
      )
    ) %>%
    dplyr::select(-win_num,
                  -ot_num) %>%
    dplyr::ungroup()

  return(plus_windows)

}
