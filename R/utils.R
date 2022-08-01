NULL

get_predictor_vec <- function(path=system.file('specs', 'predictors', package = 'nba.aggregator')) {
  readLines(path)
}
get_exclusion_vec <- function(path=system.file('specs', 'exclusions', package = 'nba.aggregator')) {
  readLines(path)
}


#' @title cumulative variance
#' @rdname utils
#' @param x numeric
#' @param sd logical should it return standard deviation instead of variance
#' @return numeric
#' @export
cumvar <- function (x, sd = FALSE) {
  # https://stackoverflow.com/questions/52459711/how-to-find-cumulative-variance-or-standard-deviation-in-r
  x <- x - x[sample.int(length(x), 1)]
  n <- seq_along(x)
  v <- (cumsum(x ^ 2) - cumsum(x) ^ 2 / n) / (n - 1)
  if (sd) v <- sqrt(v)
  v
}

# make an incrementor function to use with purrr::accumulate later
incrementor <- function(prev, new, growth = 1) {
  dplyr::if_else(new == 0|is.na(new), new, prev + growth)
}

# edit_predictor_vec <- function() {}
# edit_exclusion_vec <- function() {}
