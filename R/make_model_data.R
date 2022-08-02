#' Combine inputs and outcomes and split for modeling
#' @param team_model_inputs data.frame
#' @param team_model_outcomes data.frame
#' @param OUTCOME UNQUOTED name of outcome for this run
#' @return list
#' @export
make_model_data <- function(team_model_inputs, team_model_outcomes, OUTCOME) {

  #### merge data, relabel outcome ####
  model_data <- team_model_inputs %>%
    dplyr::inner_join(
      team_model_outcomes %>%
        dplyr::mutate(
          outcome = {{ OUTCOME }}
        ),
      by = c('game_id', 'team_id')
      )

  #### do a little pca with outcomes and other stuff to make a strata variable ####
  strata_data <- model_data %>%
      dplyr::mutate(
        home_ind = as.numeric(home_away == 'home'),
        win_ind = as.numeric(wl_outcome == 'W')
      ) %>%
      dplyr::select(
        -dplyr::ends_with('_id')
      ) %>%
      dplyr::select(
        dplyr::matches('window'),
        dplyr::matches('team'),
        dplyr::matches('pts'),
        dplyr::ends_with('_ind'))
  pca_preds <- stats::prcomp(~., data = strata_data) %>%
    predict(., strata_data) %>%
    tibble::as_tibble() %>%
    tidyr::replace_na(list(PC1 = 0))

  model_data$strata_var <- rsample::make_strata(pca_preds$PC1)

  #### split for testing/training ####
  split <- rsample::initial_split(model_data, prop = 8/10, strata = strata_var)
  training_data <- rsample::training(split)
  testing_data <- rsample::testing(split)

  output_list <-
    list(
      training = training_data,
      testing = testing_data,
      split = split
      )

  return(output_list)

}
