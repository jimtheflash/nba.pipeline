prep_team_model_outcomes <- function(team_gamelogs) {

  team_model_outcomes <- team_gamelogs %>%
    dplyr::select(
      team_id,
      game_id,
      wl,
      ot,
      pts_team_diff
    )

  return(team_model_outcomes)

}
