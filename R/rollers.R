# also should probably make the rolling functions ahead of time, iirc it moves a lot faster that way
rollsum_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    sum,
    .before = p,
    ...
  )
}

rollmean_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    mean,
    .before = p,
    ...
  )
}

rollmax_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    max,
    .before = p,
    ...
  )
}

rollmin_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    min,
    .before = p,
    ...
  )
}

rollmedian_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    median,
    .before = p,
    ...
  )
}

rollvar_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    var,
    .before = p,
    ...
  )
}

rollcount_days_p <- function(x, date_index, p, ...) {
  slider::slide_period_vec(
    x,
    date_index,
    "day",
    dplyr::n_distinct,
    .before = p,
    ...
  )
}
