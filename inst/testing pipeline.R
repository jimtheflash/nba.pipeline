load_all()
gl <- prep_gamelogs('/Users/jim/Documents/gambling_stuff/data/02_curated/nba_gamelogs/current.csv')

# make the team stuff first
tg <- make_team_game_summaries(gl)
tgm <- make_team_game_metadata(gl)
tgw <- add_team_game_windows(tg)

# then use the team stuff to start making player summaries
pg <- make_player_game_summaries(gl, tg)
pgw <- add_player_game_windows(pg, tgw)

# # prep for team-level models
# team_models <- prep_team_model_input()
#
# # make league-date leaders
# ll <- make_league_date_leaders(pgw)
#
# # make player-team leaders
# tl <- make_team_date_leaders(pgw, tgw)
#
#
