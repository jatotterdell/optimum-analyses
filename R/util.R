suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
})

select_form <- function(dat, frm) {
  filter(dat, form == frm) |>
    pull(data) |>
    pluck(1)
}

#' Modify a tbl_summary's header to use newlines
#' Header made by level (L) and number of observations (N)
#'
#' @param x A tbl_summary object
#'
#' @return None
#'
#' @export
my_modify_header_LN <- function(x) {
  header_fmt_LN <- function() {
    if (knitr::is_latex_output()) {
      str <- paste0(
        "\\makecell{{\\textbf{{{level}}} \\\\\\\\ ",
        "N = {formatC(n, format='d', big.mark=',')}}}"
      )
    } else if (knitr::is_html_output()) {
      str <- "**{level}** <br/> N = {formatC(n, format='d', big.mark=',')}"
    } else {
      str <- "**{level}** N = {formatC(n, format='d', big.mark=',')}"
    }
  }

  return(
    modify_header(x, all_stat_cols() ~ header_fmt_LN())
  )
}

#' Modify a tbl_summary's header to use newlines
#' Header made by level (L), number of observations (N) and percentage over total (P)
#'
#' @param x A tbl_summary object
#'
#' @return None
#'
#' @export
my_modify_header_LNP <- function(x) {
  header_fmt_LNP <- function() {
    if (knitr::is_latex_output()) {
      str <- paste0(
        "\\makecell{{\\textbf{{{level}}} ",
        "\\\\\\\\ ",
        "N = {formatC(n, format='d', big.mark=',')} ",
        "\\\\\\\\ ",
        "({style_percent(p)}%)}}"
      )
    } else if (knitr::is_html_output()) {
      str <- paste0(
        "**{level}** ",
        "<br/> ",
        "N = {formatC(n, format='d', big.mark=',')} ",
        "<br/> ",
        "({style_percent(p)}%)"
      )
    } else {
      str <- paste0(
        "**{level}** \n",
        "N = {formatC(n, format='d', big.mark=',')} \n",
        "({style_percent(p)}%)"
      )
    }
    return(str)
  }

  return(
    modify_header(x, all_stat_cols() ~ header_fmt_LNP())
  )
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

#' Print a tbl_summary table
#'
#' @param data A tbl_summary object
#'
#' @return None
#'
#' @export
my_print_table <- function(data) {
  # Put the footnote references back into the `makecell` command
  fix_footnote <- function(s) {
    s <- s %>%
      stringr::str_replace_all(
        r"(\\makecell\{(.+?)((?:\\\\.+?)+)\}(.+?)(?=&|\\\\|$))",
        r"(\\makecell{\1\2\3})"
      )
    return(s)
  }
  # Revert the escaping done by gt::as_latex
  # see: https://github.com/rstudio/gt/issues/1912
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
  # Always use gt as a backend
  data <- data %>% gtsummary::as_gt()
  # In case of LaTeX output:
  # unescape escape sequences, fix footnotes,
  # and return a knitr "asis" object
  if (knitr::is_latex_output()) {
    data <- data %>%
      gt::as_latex() %>%
      latex_unescape() %>%
      fix_footnote()
  }
  return(data)
}
