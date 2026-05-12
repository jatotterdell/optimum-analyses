suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
})

#' Calculate Pr(X > Y + delta)
#' where X and Y are independent Beta random variables
#' using numerical integration
#'
#' @param a Parameter one of beta density for X and Y
#' @param b Parameter two of beta density for X and Y
#' @param delta The difference we wish to assess (i.e. X - Y > delta)
#' @param ... other arguments passed to integrate/quadgk function
#' @return The value of the integral
#' @examples
#' beta_ineq(c(1, 1), c(1, 3))
#' @export
#' @importFrom pracma quadgk
beta_ineq <- function(a, b, delta = 0, ...) {
  stopifnot("a, b must be > 0" = all(c(a, b) > 0))
  integrand <- function(x) {
    stats::dbeta(x, a[1], b[1]) * stats::pbeta(x - delta, a[2], b[2])
  }
  tryCatch(
    pracma::quadgk(integrand, delta, 1, ...),
    error = function(err) NA
  )
}


#' Calculate Pr(X > Y + delta)
#' where X and Y are independent Beta random variables
#' using Normal approximation.
#'
#' @param a Parameter one of beta density for X and Y
#' @param b Parameter two of beta density for X and Y
#' @param delta The difference we wish to assess (i.e. X - Y > delta)
#' @return The value of the integral
#' @examples
#' beta_ineq_approx(c(1, 1), c(1, 3))
#' @export
beta_ineq_approx <- function(a, b, delta = 0) {
  stopifnot("a, b must be > 0" = all(c(a, b) > 0))
  m <- a / (a + b)
  v <- a * b / ((a + b)^2 * (a + b + 1))
  z <- (m[1] - m[2] - delta) / sqrt(v[1] + v[2])
  return(stats::pnorm(z))
}


#' Calculate Pr(X > Y + delta)
#' where X and Y are independent Beta random variables
#' using Monte Carlo method.
#'
#' @param a Parameter one of beta density for X and Y
#' @param b Parameter two of beta density for X and Y
#' @param delta The difference we wish to assess (i.e. X - Y > delta)
#' @param sims The number of Monte Carlo variates to generate for estimation
#' @return The value of the integral
#' @examples
#' beta_ineq_sim(c(1, 1), c(1, 3))
#' @export
beta_ineq_sim <- function(a, b, delta = 0, sims = 10000) {
  stopifnot("a, b must be > 0" = all(c(a, b) > 0))
  lens <- unlist(lapply(list(a, b), length))
  stopifnot("a, b must be same length" = all(max(lens) - min(lens) == 0))
  X <- stats::rbeta(sims, a[1], b[1])
  Y <- stats::rbeta(sims, a[2], b[2])
  p <- mean(X > Y + delta)
  return(p)
}

#' Draw random variates from beta-binomial distribution
#'
#' @param n The number of random values to sample
#' @param m The sample size
#' @param a First parameter
#' @param b Second parameter
#' @examples
#' rbetabinom(2, 10, 2, 3)
#' @export
rbetabinom <- function(n, m, a = 1, b = 1) {
  stopifnot("n must be > 0" = all(n > 0))
  stopifnot("a and b must be > 9" = all(c(a, b) > 0))
  return(stats::rbinom(n, m, stats::rbeta(n, a, b)))
}


#' Calculate the predicted probability of success
#'
#' @import data.table
#' @param a First parameter of first beta random variable
#' @param b Second parameter of first beta random variable
#' @param c First paramter of second beta random variable
#' @param d Second parameter of second beta random variable
#' @param m1 Sample size to predict for first beta random variable
#' @param m2 Sample size to predict for second beta random variable
#' @param k_ppos The posterior probability cut-point to be assessed
#' @param post_method The method to use for calculating posterior probabilities,
#' one of "exact" (numerical), "approx", "sim".
#' @param post_sim Number of posterior simulations if post_method = "sim".
#' @return The predicted probability of success
#' @export
calc_ppos <- function(
  a,
  b,
  m,
  k_ppos,
  post_method = "exact",
  post_sim = 1e4
) {
  stopifnot("a, b, and m must be > 0" = all(c(a, b, m) > 0))
  stopifnot("k_ppos must be in [0, 1]" = (k_ppos >= 0 & k_ppos <= 1))
  calc_post <- switch(post_method, "exact" = beta_ineq, "approx" = beta_ineq_approx, "sim" = beta_ineq_sim)
  y1pred <- rbetabinom(post_sim, m[1], a[1], b[1])
  y2pred <- rbetabinom(post_sim, m[2], a[2], b[2])
  ypred <- tibble(y1 = y1pred, y2 = y2pred) |>
    count(y1, y2)
  ypred <- ypred |>
    rowwise() |>
    mutate(p = calc_post(a + c(y1, y2), b + m - c(y1, y2))) |>
    ungroup()
  return(sum(ypred$n * (ypred$p > 0.95)) / sum(ypred$n))
}


select_form <- function(dat, frm) {
  filter(dat, form == frm) |>
    pull(data) |>
    pluck(1)
}


latex_unescape <- function(s) {
  pats <- c(
    "\\\\textbackslash\\{\\}" = "\\\\",
    "\\\\\\{" = "{",
    "\\\\\\}" = "}",
    "\\{\\[\\}" = "[",
    "\\{\\]\\}" = "]"
  )
  s <- s %>% str_replace_all(pats)
  return(s)
}

fix_footnote <- function(s) {
  s <- s %>%
    stringr::str_replace_all(
      r"(\\makecell\{(.+?)((?:\\\\.+?)+)\}(.+?)(?=&|\\\\|$))",
      r"(\\makecell{\1\2\3})"
    )
  return(s)
}
