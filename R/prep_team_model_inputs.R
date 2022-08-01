#' Construct initial input for team game models
#' @param team_gamelogs data.frame
#' @param team_game_metadata data.frame
#' @param lag_regex character
#' @param rolling_regex character
#' @return data.frame
#' @export
prep_team_model_inputs <- function(team_gamelogs, team_game_metadata,
                                  lag_regex = '_team_(rolling|allowed|diff)|window_rolling',
                                  rolling_regex = 'rolling_(sum|sd|mean)$') {

  team_model_input <- team_gamelogs %>%

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

  #   #### merge in team-game metadata ####
  #   dplyr::inner_join(team_game_metadata,
  #                     by = c('game_id', 'season_year', 'season_type', 'team_id', 'game_date', 'wl')
  #                     )
  #
  #
  # %>%

    #### join the opponent data back on ####
    dplyr::inner_join(
      x = .,
      y = dplyr::select(., team_id, game_id,
                        dplyr::starts_with('b2b'),
                        dplyr::ends_with('game_number'),
                        dplyr::ends_with('lagged'),
                        dplyr::matches('days_since'),
                        dplyr::matches('games_in_last'),
                        dplyr::matches('streak')),
      by = c('team_id', 'game_id'),
      suffix = c('', '_opponent')
      ) %>%
    dplyr::rename_with(
      ~ paste0('opponent_', .),
      dplyr::ends_with('_opponent')
    ) %>%
    stats::setNames(gsub('_opponent$', '', names(.))) %>%

    #### remove all outcomes ####
    dplyr::select(-dplyr::matches('outcome'),
                  -wl,
                  -ot)

  #### calculate differences in features between team and opponent ####
  for (opp_col_nm in names(team_model_input)[grepl('^opponent', names(team_model_input)) & grepl('rolling_mean', names(team_model_input))]) {
    tm_col_nm <- gsub('^opponent_', '', opp_col_nm)
    new_col_nm <- paste0(tm_col_nm, '_vs_opp')
    opp_col <- team_model_input[[opp_col_nm]]
    tm_col <- team_model_input[[tm_col_nm]]
    new_val <- tm_col - opp_col
    team_model_input[[new_col_nm]] <- new_val
  }

  #### return output ####
  return(team_model_input)

}
