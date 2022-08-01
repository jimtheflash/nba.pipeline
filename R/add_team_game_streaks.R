add_team_game_streaks <- function(team_gamelogs) {

  plus_streaks <- team_gamelogs %>%
    #### create streak features ####
    dplyr::arrange(season_year, team_id, game_date) %>%
    dplyr::group_by(season_year, team_id) %>%
    dplyr::mutate(
      home_game_streak_including_current = purrr::accumulate(as.numeric(home_away == 'home'), incrementor),
      away_game_streak_including_current = purrr::accumulate(as.numeric(home_away == 'away'), incrementor),
      loss_streak_excluding_current = purrr::accumulate(as.numeric(dplyr::lag(wl) == 'L'), incrementor),
      win_streak_excluding_current = purrr::accumulate(as.numeric(dplyr::lag(wl) == 'W'), incrementor)
      ) %>%
    dplyr::ungroup()

  #### return output ####
  return(plus_streaks)
}
