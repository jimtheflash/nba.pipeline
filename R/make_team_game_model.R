make_team_game_model <- function(recipe, team_modeling_data) {

  # see what kind of outcome and make the model based on that
  outcome_type <- recipe$var_info$type[recipe$var_info$role == 'outcome']

  # numeric outcomes are linear regression (for now)
  if (outcome_type == 'numeric') {
    mod <- parsnip::linear_reg(penalty = 1) %>%
      parsnip::set_engine('glmnet')
  }
  # nominal outcomes are logistic regression (for now)
  else if (outcome_type == 'nominal') {
    mod <- parsnip::logistic_reg(penalty = 1) %>%
      parsnip::set_engine('glmnet')
  }

  # make workflow
  tg_wf <- workflows::workflow() %>%
    workflows::add_model(mod) %>%
    workflows::add_recipe(recipe) %>%
    parsnip::fit(team_modeling_data)

  return(tg_wf)

}
