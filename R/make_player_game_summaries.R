#' Make player game summaries that are a bit more enriched than the gamelogs
#' @note this is the first summary step for players, look for `app_player_game_windows()` and other steps later in the pipeline
#' @param gamelogs data.frame output from `prep_gamelogs()`
#' @param team_gamelogs data.frame output from `make_team_game_summaries()`
#' @param cols_to_rename character of columns that need to be labeled as 'player' fields
#' @return data.frame
#' @export
make_player_game_summaries <- function(gamelogs, team_gamelogs,
                                       cols_to_rename = c(get_predictor_vec(), get_exclusion_vec())) {

  player_gamelogs <- gamelogs %>%

    #### append 'player' to columns we're going to care about calculating team proportions on ####
    dplyr::rename_with(
      .cols = cols_to_rename,
      .fn = ~ paste0(., "_player")
      ) %>%

    #### make rates again ####
    dplyr::mutate(
      ft_perc = dplyr::if_else(fta_player > 0, ftm_player/fta_player, NA_real_),
      fg_perc = dplyr::if_else(fga_player > 0, fgm_player/fga_player, NA_real_),
      fg3_perc = dplyr::if_else(fg3a_player > 0, fg3m_player/fg3a_player, NA_real_)
      ) %>%

    #### join team-game data to player-game data ####
    dplyr::inner_join(
      team_gamelogs,
      by = c('season_year', 'season_type', 'game_id', 'game_date', 'team_id')
      )

  #### rip through the columns that should have team values in them to calculate player proportions of team ####
  for (col_stem in get_predictor_vec()[!get_predictor_vec() %in% get_exclusion_vec()]) {
    numerator <- player_gamelogs[[paste0(col_stem, '_player')]]
    denominator <- player_gamelogs[[paste0(col_stem, '_team')]]
    new_col_name <- paste0(col_stem, '_player_perc_of_team')
    player_gamelogs[[new_col_name]] <- dplyr::if_else(denominator > 0 &
                                                        !is.na(numerator) &
                                                        !is.na(denominator),
                                                      numerator/denominator,
                                                      NA_real_)
  }

  #### return the output ####
  return(player_gamelogs)


}
