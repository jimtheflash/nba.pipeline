make_league_date_leaders <- function(player_gamelogs, regular_season = TRUE) {

  #### first make a sequence of dates from the first day to the last day of the season ####
  if (regular_season) {
    player_gamelogs <- dplyr::filter(player_gamelogs, season_type == 'Regular Season')
  }

  league_date_leaders <- tidyr::expand_grid(
    player_id = unique(player_gamelogs$player_id),
    game_date = unique(player_gamelogs$game_date)
    ) %>%

    #### join in the player gamelogs - this will be a sparse data.frame to start! ####
    dplyr::full_join(
      x = .,
      y = player_gamelogs %>%
        dplyr::select(
          player_id,
          game_date,
          dplyr::matches('player_window')),
      by = c('player_id', 'game_date')
      ) %>%
    dplyr::arrange(player_id, game_date) %>%

    #### now FILL ####
    dplyr::group_by(player_id, season_year, season_type) %>%
    tidyr::fill(dplyr::matches('player_window')) %>%
    tidyr::fill(season_year, season_type, .direction = 'updown') %>%
    dplyr::ungroup() %>%

    #### get the max values for the stuff we care about
    dplyr::group_by(game_date) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches('rolling_sum|rolling_mean'),
        .fns = list(
          player_league_rank = ~ dplyr::percent_rank(.x)
          )
        )
      ) %>%
    dplyr::ungroup()



}
