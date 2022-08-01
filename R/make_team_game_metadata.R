#' Make metadata for team-games
#' @param gamelogs data.frame from `prep_gamelogs()`
#' @return data.frame
#' @export
make_team_game_metadata <- function(gamelogs) {

  tg_meta <- gamelogs %>%

    #### create data.frame that is unique on season-team-game ####
    dplyr::select(season_year, season_type, team_id, game_id, game_date) %>%
    dplyr::group_by(season_year, season_type, team_id, game_id, game_date) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%

    #### group by season-team for overall season game number
    dplyr::arrange(season_year, team_id, game_date) %>%
    dplyr::group_by(season_year, team_id) %>%
    dplyr::mutate(team_season_game_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%

    #### group by season-season_type-team for more granular rollups ####
    dplyr::arrange(season_year, team_id, game_date) %>%
    dplyr::group_by(season_year, season_type, team_id) %>%
    dplyr::mutate(team_season_type_game_number = dplyr::row_number(),
                  team_season_type_days_since_previous_game = as.numeric(
                    difftime(time1 = game_date, time2 = dplyr::lag(game_date), units = 'hours')
                    ) / 24
                  ) %>%
    dplyr::ungroup() %>%

    #### join back onto gamelogs for additional engineering ####
    dplyr::inner_join(
      gamelogs %>%
        dplyr::select(game_id, team_id, matchup, wl) %>%
        dplyr::group_by(team_id, game_id, matchup, wl) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          home_away = dplyr::if_else(grepl(' vs', matchup), 'home', 'away')
        ) %>%
        dplyr::select(-matchup),
      by = c('team_id', 'game_id')
      ) %>%

    #### create scheduling features ####
    dplyr::arrange(season_year, team_id, game_date) %>%
    dplyr::group_by(season_year, team_id) %>%
    dplyr::mutate(
      b2b_firsthalf = dplyr::if_else(as.Date(game_date) == as.Date(dplyr::lead(game_date)) - 1, TRUE, FALSE),
      b2b_secondhalf = dplyr::if_else(as.Date(game_date) == as.Date(dplyr::lag(game_date)) + 1, TRUE, FALSE),
      games_in_last_seven_days_including_current = rollcount_days_p(game_id, as.Date(game_date), 7, .complete = TRUE),
      away_games_in_last_five_games_including_current = rollsum_p(home_away == 'away', 5, .complete = TRUE),
      away_games_in_last_ten_games_including_current = rollsum_p(home_away == 'away', 10, .complete = TRUE)
      ) %>%
    tidyr::replace_na(
      list(b2b_firsthalf = FALSE,
           b2b_secondhalf = FALSE)
    ) %>%

    #### create streak features ####
    dplyr::mutate(
      home_game_streak_including_current = purrr::accumulate(as.numeric(home_away == 'home'), incrementor),
      away_game_streak_including_current = purrr::accumulate(as.numeric(home_away == 'away'), incrementor),
      loss_streak_excluding_current = purrr::accumulate(as.numeric(dplyr::lag(wl) == 'L'), incrementor),
      win_streak_excluding_current = purrr::accumulate(as.numeric(dplyr::lag(wl) == 'W'), incrementor)
    ) %>%
    dplyr::ungroup() %>%

    #### create rotation features ####
    dplyr::inner_join(
      gamelogs %>%
        dplyr::group_by(team_id, game_id) %>%
        dplyr::summarise(
          team_game_players_used_gt_0minutes = dplyr::n_distinct(player_id[min > 0]),
          team_game_players_used_gt_10minutes = dplyr::n_distinct(player_id[min > 10]),
          team_game_players_used_gt_20minutes = dplyr::n_distinct(player_id[min > 20]),
          team_game_players_used_gt_30minutes = dplyr::n_distinct(player_id[min > 30]),
          team_game_players_mean_minutes = mean(min, na.rm = TRUE),
          team_game_players_max_minutes = max(min, na.rm = TRUE)
        ) %>%
        dplyr::ungroup(),
      by = c('team_id', 'game_id')
    ) %>%
    dplyr::arrange(season_year, team_id, game_date) %>%
    dplyr::group_by(season_year, season_type, team_id) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with('team_game_players'),
        list(
          rolling_mean = ~ dplyr::cummean(.x)
        )
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with('team_game_players'),
        list(
          lagged = ~ dplyr::lag(.x)
        )
      )
    ) %>%
    dplyr::select(-dplyr::ends_with('minutes'),
                  -dplyr::ends_with('rolling_mean')) %>%

    #### create overtime feature ####
    dplyr::inner_join(
      gamelogs %>%
        dplyr::group_by(game_id) %>%
        dplyr::summarise(ot = dplyr::if_else(sum(min, na.rm = TRUE) > 480, TRUE, FALSE)) %>%
        dplyr::ungroup(),
      by = 'game_id'
    )

  #### return output ####
  return(tg_meta)

}
