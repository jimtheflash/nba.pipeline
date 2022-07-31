get_predictor_vec <- function(path=system.file('specs', 'predictors', package = 'nba.aggregator')) {
  readLines(path)
}
get_exclusion_vec <- function(path=system.file('specs', 'exclusions', package = 'nba.aggregator')) {
  readLines(path)
}
# edit_predictor_vec <- function() {}
# edit_exclusion_vec <- function() {}
