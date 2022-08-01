#' Add player-game window functions to gamelogs
#' @param player_gamelogs data.frame from `make_player_game_summaries()`
#' @return data.frame
#' @export
add_player_game_windows <- function(player_gamelogs, team_gamelogs) {

  plus_windows <- player_gamelogs %>%

    #### add window functions for season irrespective of season_type ####
    dplyr::arrange(season_year, player_id, game_date) %>%
    dplyr::group_by(season_year, player_id) %>%
    dplyr::mutate(season_player_game_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%

    #### add window functions for season by season_type ####
    dplyr::group_by(season_year, season_type, player_id) %>%
    dplyr::mutate(
      season_type_player_game_number = dplyr::row_number()
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with('_player'),
        .fns = list(
          rolling_sum = ~ cumsum(as.numeric(.x)),
          rolling_mean = ~ dplyr::cummean(as.numeric(.x)),
          rolling_sd = ~ suppressWarnings(cumvar(as.numeric(.x), sd=TRUE)) # suppressing the warnings for NaNs
        ),
        .names = '{.col}_{.fn}'
      )
    ) %>%
    dplyr::ungroup()

  #### return output ####
  return(plus_windows)

}
