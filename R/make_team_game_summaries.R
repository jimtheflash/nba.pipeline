#' Summarise gamelogs to make team game data
#' @note This won't include any window functions, just team-game totals of stuff in the `include_vec` argument
#' @param gamelogs data.frame from `prep_gamelogs()`
#' @param include_vec character vector of field to include for aggregation, from `get_predictor_vec()`
#' @param exclude_vec character vector of field to include for aggregation, from `get_exclusion_vec()`
#' @return data.frame
#' @export
make_team_game_summaries <- function(gamelogs, include_vec=get_predictor_vec(), exclude_vec=get_exclusion_vec()) {

  #### summarise at the team-game level ####
  team_game_box <- gamelogs %>%

    ### get the team id's for each game ####
    dplyr::group_by(game_id) %>%
    dplyr::summarise(
      team_1 = min(team_id),
      team_2 = max(team_id)) %>%
    dplyr::ungroup() %>%

    ### join to all the gamelogs ####
    dplyr::inner_join(
      gamelogs %>%
        dplyr::group_by(season_year, season_type, team_id, game_id, game_date) %>%
        dplyr::summarise(
          dplyr::across(
            include_vec[!include_vec %in% exclude_vec],
            sum,
            .names = '{.col}_team'
          )
        ),
      by = 'game_id'
    ) %>%

    ### engineer the opp_team_id to quickly identify the opponent in a game ####
    dplyr::mutate(opp_team_id = dplyr::if_else(team_id == team_1, team_2, team_1)) %>%
    dplyr::select(-team_1, -team_2) %>%

    ### join back on the opponent data ####
    dplyr::left_join(
      x = .,
      y = dplyr::select(., season_year, game_id, team_id, dplyr::matches('_team')),
      by = c('season_year',
             'game_id',
             'team_id' = 'opp_team_id'),
      suffix = c('', '_opp')
      ) %>%
    dplyr::select(-team_id_opp) %>%

    ### fix the names to reflect that the opponent stats in a game are actually allowed (defensive) stats and to include 'team' ####
    stats::setNames(gsub('team_opp', 'allowed', names(.))) %>%
    stats::setNames(gsub('allowed', 'team_allowed', names(.)))

  #### compute the difference in team performance and what they allowed from their opponent that game ####
  for (p in include_vec[!include_vec %in% exclude_vec]) {
    team_var_name <- paste0(p, '_team')
    allowed_var_name <- paste0(p, '_team_allowed')
    diff_var_name <- paste0(p, '_team_diff')
    diffs <- team_game_box[[team_var_name]] - team_game_box[[allowed_var_name]]
    team_game_box[[diff_var_name]] <- diffs
  }

  #### add categorical outcomes ####
  team_game_box$outcome_wl <- dplyr::if_else(team_game_box$pts_team_diff > 0, 'W', 'L')

  #### output ####
  return(team_game_box)

}
