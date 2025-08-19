suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

select_form <- function(dat, frm) {
  filter(dat, form == frm) |>
    pull(data) |>
    pluck(1)
}
