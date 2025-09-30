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
