rm(list = ls())
t1 <- Sys.time()
document(); load_all()
gl <- prep_gamelogs('https://raw.githubusercontent.com/jimtheflash/gambling_stuff/main/data/02_curated/nba_gamelogs/current.csv')

# make the team stuff first
tg <- make_team_game_summaries(gl)
tgm <- make_team_game_metadata(gl)

tgw <- add_team_game_windows(tg, tgm)
tgs <- add_team_game_streaks(tgw)

# then use the team stuff to start making player summaries
pg <- make_player_game_summaries(gl, tg)
pgw <- add_player_game_windows(pg, tgw)

# prep for team-level models
team_inputs <- prep_team_model_inputs(tgs, tgm)
team_outcomes <- prep_team_model_outcomes(tgs)
team_modeling_data <- make_model_data(team_inputs, team_outcomes, wl_outcome)

# recipe
rec <- make_team_game_recipe(team_modeling_data$training)

# model
mod <- make_team_game_model(rec, team_modeling_data$training)

# eval
# testing_data <- team_modeling_data$testing
# new_preds <- predict(tg_wf, testing_data)
# cor.test(new_preds$.pred, testing_data$pts_team_allowed_outcome)
# plot(new_preds$.pred, testing_data$pts_team_allowed_outcome)

t2 <- Sys.time()
print(t2 - t1)
