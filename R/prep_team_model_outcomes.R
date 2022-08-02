prep_team_model_outcomes <- function(team_gamelogs) {

  team_model_outcomes <- team_gamelogs %>%
    dplyr::select(
      team_id,
      game_id,
      wl_outcome = wl,
      ot_outcome = ot,
      pts_team_outcome = pts_team,
      pts_team_allowed_outcome = pts_team_allowed
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with('pts'),
        as.numeric
      )
    )

  return(team_model_outcomes)

}
