#' Make a tidymodels recipe for building team-game models
#' @param team_modeling_data modeling data from `make_model_data()`
#' @return a recipe
#' @export
make_team_game_recipe <- function(team_modeling_data) {

  #### recipe spec with formula ####
  team_game_rec <- recipes::recipe(outcome ~ ., data = team_modeling_data) %>%

    #### update roles ####
    recipes::update_role(dplyr::ends_with('_id'), new_role = 'id') %>%
    recipes::update_role(strata_var, new_role = 'strata_var') %>%
    recipes::update_role(season_year, season_type, new_role = 'season_var') %>%
    recipes::update_role(dplyr::ends_with('_outcome'), new_role = 'other_outcomes') %>%

    #### date stuff ####
    recipes::step_holiday(game_date) %>%
    recipes::step_date(game_date) %>%
    recipes::step_rm(game_date) %>%

    #### dummy stuff
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_rm(recipes::all_nominal_predictors()) %>%

    #### specify how to tidy up missing stuff ####
    recipes::step_naomit(recipes::all_predictors()) %>%

    #### feature reduction ####
    recipes::step_zv(recipes::all_numeric_predictors()) %>%
    recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.9) %>%

    #### normalizing ####
    recipes::step_YeoJohnson(recipes::all_numeric_predictors()) %>%

    #### prep and return ####
    recipes::prep()

  return(team_game_rec)

}
